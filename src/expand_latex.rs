use std::{
    collections::{btree_map::Entry, BTreeMap, BTreeSet, VecDeque},
    fmt::{Debug, Display},
};

use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

use crate::{parse_latex::TokenTree, resource};

/// Process LaTeX macros.
pub fn expand_latex(
    src: &str,
    src_name: &str,
    latex: Vec<(SourceSpan, TokenTree)>,
) -> Result<Vec<(SourceSpan, TokenTree)>, Vec<ExpandError>> {
    let mut commands = DefinedCommands::default();
    State::new(src, src_name, &mut commands, VecDeque::from(latex)).process_all()
}

#[derive(Error, Debug, Diagnostic)]
pub enum ExpandError {
    #[error("unknown command (current mode is {mode})")]
    UnknownCommand {
        #[source_code]
        src: NamedSource,
        #[label("unknown command found here")]
        span: SourceSpan,
        mode: Mode,
    },
    #[error("star not allowed")]
    StarNotAllowed,
    #[error("expected argument")]
    ExpectedArgument,
    #[error("unclosed optional argument")]
    UnclosedOptionalArgument,
    #[error("(bug) incorrect arity")]
    IncorrectArity,
    #[error("expected new command name")]
    ExpectedCommandName,
    #[error("expected number of parameters")]
    ExpectedNumberOfParameters,
    #[error("invalid number of parameters")]
    InvalidNumberOfParameters,
    #[error("not enough parameters for this default argument")]
    NotEnoughParameters,
    #[error("mismatched math mode delimiters")]
    MismatchedMathDelimiters,
    #[error("cannot apply diacritic")]
    CannotApplyDiacritic,
}

#[derive(Debug)]
struct State<'a> {
    src: &'a str,
    src_name: &'a str,
    commands: &'a mut DefinedCommands,
    mode: Mode,
    input: VecDeque<(SourceSpan, TokenTree)>,
    output: Vec<(SourceSpan, TokenTree)>,
}

/// Tracks the commands currently defined with `\newcommand` and others.
#[derive(Debug)]
struct DefinedCommands {
    /// The set of commands in text mode that we will not process.
    /// These are things like `\section` that the bluecode processor already knows about.
    ignore_text_mode: BTreeSet<String>,
    /// The set of commands in math mode that we will not process.
    /// These are things like `\int` that MathJax already knows about.
    ignore_math_mode: BTreeSet<String>,
    /// The set of defined commands, sorted alphabetically and indexed by name.
    commands: BTreeMap<String, Command>,
}

impl Default for DefinedCommands {
    fn default() -> Self {
        let mut output = Self {
            ignore_text_mode: resource!("commands_text_mode.txt")
                .into_iter()
                .map(|s| s.to_owned())
                .collect(),
            ignore_math_mode: resource!("commands_math_mode.txt")
                .into_iter()
                .map(|s| s.to_owned())
                .collect(),
            commands: BTreeMap::new(),
        };
        output
            .new_command(
                NewCommandBehaviour::New,
                "newcommand".to_owned(),
                CallingConvention {
                    star: true,
                    params: vec![
                        Parameter::Mandatory,
                        Parameter::Optional(Vec::new()),
                        Parameter::Optional(Vec::new()),
                        Parameter::Mandatory,
                    ],
                },
                CommandAction::NewCommand(NewCommandBehaviour::New),
            )
            .unwrap();
        output
            .new_command(
                NewCommandBehaviour::New,
                "renewcommand".to_owned(),
                CallingConvention {
                    star: true,
                    params: vec![
                        Parameter::Mandatory,
                        Parameter::Optional(Vec::new()),
                        Parameter::Optional(Vec::new()),
                        Parameter::Mandatory,
                    ],
                },
                CommandAction::NewCommand(NewCommandBehaviour::Renew),
            )
            .unwrap();
        output
            .new_command(
                NewCommandBehaviour::New,
                "providecommand".to_owned(),
                CallingConvention {
                    star: true,
                    params: vec![
                        Parameter::Mandatory,
                        Parameter::Optional(Vec::new()),
                        Parameter::Optional(Vec::new()),
                        Parameter::Mandatory,
                    ],
                },
                CommandAction::NewCommand(NewCommandBehaviour::Provide),
            )
            .unwrap();
        output
            .new_command(
                NewCommandBehaviour::New,
                "DeclareMathOperator".to_owned(),
                CallingConvention {
                    star: true,
                    params: vec![Parameter::Mandatory, Parameter::Mandatory],
                },
                CommandAction::DeclareMathOperator,
            )
            .unwrap();

        output
            .new_command(
                NewCommandBehaviour::New,
                "(".to_owned(),
                CallingConvention::no_args(),
                CommandAction::MathMode {
                    enable: true,
                    display: false,
                },
            )
            .unwrap();
        output
            .new_command(
                NewCommandBehaviour::New,
                "[".to_owned(),
                CallingConvention::no_args(),
                CommandAction::MathMode {
                    enable: true,
                    display: true,
                },
            )
            .unwrap();
        output
            .new_command(
                NewCommandBehaviour::New,
                ")".to_owned(),
                CallingConvention::no_args(),
                CommandAction::MathMode {
                    enable: false,
                    display: false,
                },
            )
            .unwrap();
        output
            .new_command(
                NewCommandBehaviour::New,
                "]".to_owned(),
                CallingConvention::no_args(),
                CommandAction::MathMode {
                    enable: false,
                    display: true,
                },
            )
            .unwrap();

        // Map each LaTeX diacritic symbol to the corresponding Unicode combining character.
        let accents = [
            ('`', '\u{300}'),
            ('\'', '\u{301}'),
            ('^', '\u{302}'),
            ('~', '\u{303}'),
            ('=', '\u{304}'),
            ('u', '\u{306}'),
            ('.', '\u{307}'),
            ('"', '\u{308}'),
            ('H', '\u{30b}'),
            ('v', '\u{30c}'),
            ('d', '\u{323}'),
            ('c', '\u{327}'),
            ('k', '\u{328}'),
            ('b', '\u{331}'),
            ('t', '\u{361}'),
        ];
        for (diacritic, replacement) in accents {
            output
                .new_command(
                    NewCommandBehaviour::New,
                    diacritic.to_string(),
                    CallingConvention {
                        star: false,
                        params: vec![Parameter::Mandatory],
                    },
                    CommandAction::CombiningCharacter(replacement),
                )
                .unwrap();
        }

        output
    }
}

impl DefinedCommands {
    pub fn new_command(
        &mut self,
        behaviour: NewCommandBehaviour,
        name: String,
        call_conv: CallingConvention,
        action: CommandAction,
    ) -> Result<(), ExpandError> {
        if self.ignore_text_mode.contains(&name) || self.ignore_math_mode.contains(&name) {
            todo!()
        }

        match self.commands.entry(name.clone()) {
            Entry::Vacant(entry) => {
                if behaviour == NewCommandBehaviour::Renew {
                    todo!()
                } else {
                    entry.insert(Command { call_conv, action });
                    Ok(())
                }
            }
            Entry::Occupied(mut entry) => match behaviour {
                NewCommandBehaviour::New => todo!(),
                NewCommandBehaviour::Renew => {
                    entry.insert(Command { call_conv, action });
                    Ok(())
                }
                NewCommandBehaviour::Provide => Ok(()),
            },
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum NewCommandBehaviour {
    /// `\newcommand`: error if the command already exists
    New,
    /// `\renewcommand`: overwrite the command
    Renew,
    /// `\providecommand`: do nothing if the command already exists
    Provide,
}

#[derive(Debug, Clone)]
struct Command {
    call_conv: CallingConvention,
    action: CommandAction,
}

/// The calling convention for a command.
/// See <https://texdoc.org/serve/xparse/0> for a generalised version of this idea.
#[derive(Debug, Clone)]
struct CallingConvention {
    /// Whether an optional star is allowed.
    star: bool,
    params: Vec<Parameter>,
}

impl CallingConvention {
    pub fn no_args() -> Self {
        Self {
            star: false,
            params: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
enum Parameter {
    Mandatory,
    /// An optional argument.
    /// If the argument is not given, it is replaced with the given token trees.
    Optional(Vec<TokenTree>),
}

/// The actual arguments supplied to a command.
#[derive(Debug)]
struct Arguments {
    star: bool,
    args: Vec<Vec<(SourceSpan, TokenTree)>>,
}

impl Arguments {
    #[allow(clippy::type_complexity)]
    fn into_array<const N: usize>(
        self,
    ) -> Result<(bool, [Vec<(SourceSpan, TokenTree)>; N]), ExpandError> {
        match self.args.try_into() {
            Ok(array) => Ok((self.star, array)),
            Err(_) => Err(ExpandError::IncorrectArity),
        }
    }
}

#[derive(Debug, Clone)]
enum CommandAction {
    NewCommand(NewCommandBehaviour),
    DeclareMathOperator,
    MathOperator { limits: bool, body: Vec<TokenTree> },
    CombiningCharacter(char),
    MathMode { enable: bool, display: bool },
    UserDefined { body: Vec<TokenTree> },
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Mode {
    Text,
    Math {
        /// True if this block was delimited by `$$` or `\[` and `\]`;
        /// false if this block was delimited by `$` or `\(` and `/)`.
        display: bool,
        /// True if this block was delimited by `$` or `$$`;
        /// false if this block was delimited by LaTeX-style `\(` or `\[`.
        dollars: bool,
    },
}

impl Display for Mode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Mode::Text => write!(f, "text mode"),
            Mode::Math { display: true, .. } => write!(f, "display math mode"),
            Mode::Math { display: false, .. } => write!(f, "inline math mode"),
        }
    }
}

impl<'a> State<'a> {
    fn new(
        src: &'a str,
        src_name: &'a str,
        commands: &'a mut DefinedCommands,
        input: VecDeque<(SourceSpan, TokenTree)>,
    ) -> Self {
        Self {
            src,
            src_name,
            commands,
            mode: Mode::Text,
            input,
            output: Vec::new(),
        }
    }

    fn process_inner(
        &mut self,
        input: VecDeque<(SourceSpan, TokenTree)>,
    ) -> Result<Vec<(SourceSpan, TokenTree)>, Vec<ExpandError>> {
        let inner = State {
            src: self.src,
            src_name: self.src_name,
            commands: self.commands,
            mode: self.mode,
            input,
            output: Vec::new(),
        };
        inner.process_all()
    }

    fn src(&self) -> NamedSource {
        NamedSource::new(self.src_name, self.src.to_owned())
    }

    fn process_all(mut self) -> Result<Vec<(SourceSpan, TokenTree)>, Vec<ExpandError>> {
        let mut errors = Vec::new();

        while let Some((span, tree)) = self.input.pop_front() {
            if let Err(err) = self.process_input(span, tree) {
                errors.extend(err);
                // TODO: Remove this break
                break;
            }
        }

        if errors.is_empty() {
            Ok(self.output)
        } else {
            Err(errors)
        }
    }

    /// Process a single token.
    /// This may read extra token trees from `input`, may add additional trees to the front of `input`, and may push to `output`.
    fn process_input(&mut self, span: SourceSpan, tree: TokenTree) -> Result<(), Vec<ExpandError>> {
        match tree {
            TokenTree::Named(name) => {
                match self.mode {
                    Mode::Text => {
                        if self.commands.ignore_text_mode.contains(&name) {
                            self.output.push((span, TokenTree::Named(name)));
                            return Ok(());
                        }
                    }
                    Mode::Math { .. } => {
                        if self.commands.ignore_math_mode.contains(&name) {
                            self.output.push((span, TokenTree::Named(name)));
                            return Ok(());
                        }
                    }
                }

                match self.commands.commands.get(&name) {
                    Some(command) => {
                        let command = command.clone();
                        let args = self
                            .process_calling_convention(span, command.call_conv)
                            .map_err(|err| vec![err])?;
                        self.execute(span, args, command.action)
                    }
                    None => Err(vec![ExpandError::UnknownCommand {
                        src: self.src(),
                        span,
                        mode: self.mode,
                    }]),
                }
            }
            TokenTree::Symbol(symbol) => {
                let name = symbol.to_string();

                match self.mode {
                    Mode::Text => {
                        if self.commands.ignore_text_mode.contains(&name) {
                            self.output.push((span, TokenTree::Named(name)));
                            return Ok(());
                        }
                    }
                    Mode::Math { .. } => {
                        if self.commands.ignore_math_mode.contains(&name) {
                            self.output.push((span, TokenTree::Named(name)));
                            return Ok(());
                        }
                    }
                }

                match self.commands.commands.get(&name) {
                    Some(command) => {
                        let command = command.clone();
                        let args = self
                            .process_calling_convention(span, command.call_conv)
                            .map_err(|err| vec![err])?;
                        self.execute(span, args, command.action)
                    }
                    None => Err(vec![ExpandError::UnknownCommand {
                        src: self.src(),
                        span,
                        mode: self.mode,
                    }]),
                }
            }
            TokenTree::Char(c) => {
                match c {
                    '$' => {
                        // Enable or disable math mode.
                        if matches!(self.input.front(), Some((_, TokenTree::Char('$')))) {
                            match self.mode {
                                Mode::Text => {
                                    self.mode = Mode::Math {
                                        display: true,
                                        dollars: true,
                                    };
                                    Ok(())
                                }
                                Mode::Math {
                                    display: true,
                                    dollars: true,
                                } => {
                                    self.mode = Mode::Text;
                                    Ok(())
                                }
                                Mode::Math { .. } => {
                                    Err(vec![ExpandError::MismatchedMathDelimiters])
                                }
                            }
                        } else {
                            match self.mode {
                                Mode::Text => {
                                    self.mode = Mode::Math {
                                        display: true,
                                        dollars: false,
                                    };
                                    Ok(())
                                }
                                Mode::Math {
                                    display: true,
                                    dollars: false,
                                } => {
                                    self.mode = Mode::Text;
                                    Ok(())
                                }
                                Mode::Math { .. } => {
                                    Err(vec![ExpandError::MismatchedMathDelimiters])
                                }
                            }
                        }
                    }
                    _ => {
                        // Print this character to the output verbatim.
                        self.output.push((span, TokenTree::Char(c)));
                        Ok(())
                    }
                }
            }
            TokenTree::Block(trees) => {
                let result = self.process_inner(trees.into())?;
                self.output.push((span, TokenTree::Block(result)));
                Ok(())
            }
        }
    }

    /// Tries to read the arguments for the command with the given calling convention.
    /// The result's `args` is guaranteed to be a vector of the same length as `call_conv.params`.
    fn process_calling_convention(
        &mut self,
        span: SourceSpan,
        call_conv: CallingConvention,
    ) -> Result<Arguments, ExpandError> {
        let star = matches!(self.input.front(), Some((_, TokenTree::Char('*'))));

        if star {
            if call_conv.star {
                self.input.pop_front();
            } else {
                return Err(ExpandError::StarNotAllowed);
            }
        }

        let args = call_conv
            .params
            .into_iter()
            .map(|parameter| self.process_parameter(span, parameter))
            .collect::<Result<_, _>>()?;

        Ok(Arguments { star, args })
    }

    fn process_parameter(
        &mut self,
        span: SourceSpan,
        parameter: Parameter,
    ) -> Result<Vec<(SourceSpan, TokenTree)>, ExpandError> {
        match parameter {
            Parameter::Mandatory => match self.input.pop_front() {
                Some((_, TokenTree::Named(name))) => Ok(vec![(span, TokenTree::Named(name))]),
                Some((_, TokenTree::Symbol(symbol))) => Ok(vec![(span, TokenTree::Symbol(symbol))]),
                Some((_, TokenTree::Char(_))) => todo!(),
                Some((_, TokenTree::Block(block))) => Ok(block
                    .into_iter()
                    .map(|(_, tree)| (span, tree.overwrite_spans(span)))
                    .collect()),
                None => Err(ExpandError::ExpectedArgument),
            },
            Parameter::Optional(replacement) => {
                if matches!(self.input.front(), Some((_, TokenTree::Char('[')))) {
                    self.input.pop_front();
                    let mut open_brackets = 1;
                    let mut tokens = Vec::new();
                    while let Some((_, tree)) = self.input.pop_front() {
                        match tree {
                            TokenTree::Char('[') => open_brackets += 1,
                            TokenTree::Char(']') => {
                                open_brackets -= 1;
                                if open_brackets == 0 {
                                    break;
                                }
                                tokens.push((span, tree));
                            }
                            _ => tokens.push((span, tree)),
                        }
                    }
                    if open_brackets == 0 {
                        Ok(tokens)
                    } else {
                        Err(ExpandError::UnclosedOptionalArgument)
                    }
                } else {
                    Ok(replacement.into_iter().map(|tree| (span, tree)).collect())
                }
            }
        }
    }

    fn execute(
        &mut self,
        span: SourceSpan,
        args: Arguments,
        action: CommandAction,
    ) -> Result<(), Vec<ExpandError>> {
        match action {
            CommandAction::NewCommand(behaviour) => self
                .process_newcommand(behaviour, args)
                .map_err(|err| vec![err]),
            CommandAction::DeclareMathOperator => self
                .process_declare_math_operator(args)
                .map_err(|err| vec![err]),
            CommandAction::MathOperator { limits, body } => {
                if limits {
                    self.output
                        .push((span, TokenTree::Named("operatorname*".to_owned())));
                } else {
                    self.output
                        .push((span, TokenTree::Named("operatorname".to_owned())));
                }
                self.output.push((
                    span,
                    TokenTree::Block(body.into_iter().map(|tree| (span, tree)).collect()),
                ));
                Ok(())
            }
            CommandAction::CombiningCharacter(combining_character) => {
                self.process_combining_character(span, args, combining_character)
            }
            CommandAction::MathMode { enable, display } => self
                .process_math_mode(enable, display)
                .map_err(|err| vec![err]),
            CommandAction::UserDefined { body } => self
                .process_user_macro(span, body, args)
                .map_err(|err| vec![err]),
        }
    }

    fn process_command_name(
        &mut self,
        mut tokens: Vec<(SourceSpan, TokenTree)>,
    ) -> Result<(SourceSpan, String), ExpandError> {
        if tokens.len() != 1 {
            return Err(ExpandError::ExpectedCommandName);
        }

        // The name of the new command.
        match tokens.pop() {
            Some((span, TokenTree::Named(name))) => Ok((span, name)),
            Some((_, _)) => Err(ExpandError::ExpectedCommandName),
            None => unreachable!(),
        }
    }

    fn process_newcommand(
        &mut self,
        behaviour: NewCommandBehaviour,
        args: Arguments,
    ) -> Result<(), ExpandError> {
        let (_star, [name, parameters, default, body]) = args.into_array()?;

        // The name of the new command.
        let (_, name) = self.process_command_name(name)?;

        // The number of parameters of this command.
        let parameters = if parameters.is_empty() {
            0
        } else {
            match parameters
                .iter()
                .map(|(_, tree)| tree.to_string())
                .collect::<String>()
                .parse::<i32>()
            {
                Ok(parameters) => parameters,
                Err(_) => return Err(ExpandError::ExpectedNumberOfParameters),
            }
        };

        if !(0..=9).contains(&parameters) {
            return Err(ExpandError::InvalidNumberOfParameters);
        }

        let call_conv = if default.is_empty() {
            CallingConvention {
                star: false,
                params: (0..parameters).map(|_| Parameter::Mandatory).collect(),
            }
        } else {
            if parameters < 1 {
                return Err(ExpandError::NotEnoughParameters);
            }
            CallingConvention {
                star: false,
                params: (1..parameters).map(|_| Parameter::Mandatory).collect(),
            }
        };

        self.commands.new_command(
            behaviour,
            name,
            call_conv,
            CommandAction::UserDefined {
                body: body.into_iter().map(|(_, tree)| tree).collect(),
            },
        )
    }

    fn process_declare_math_operator(&mut self, args: Arguments) -> Result<(), ExpandError> {
        let (star, [name, body]) = args.into_array()?;

        let (_, name) = self.process_command_name(name)?;
        self.commands.new_command(
            NewCommandBehaviour::New,
            name,
            CallingConvention::no_args(),
            CommandAction::MathOperator {
                limits: star,
                body: body.into_iter().map(|(_, tree)| tree).collect(),
            },
        )
    }

    fn process_combining_character(
        &mut self,
        span: SourceSpan,
        args: Arguments,
        combining_character: char,
    ) -> Result<(), Vec<ExpandError>> {
        let (_, [body]) = args.into_array().map_err(|err| vec![err])?;
        let processed_body = self.process_inner(body.into())?;

        if processed_body
            .iter()
            .any(|(_, tree)| !matches!(tree, TokenTree::Char(_)))
        {
            return Err(vec![ExpandError::CannotApplyDiacritic]);
        }

        let result = processed_body
            .iter()
            .flat_map(|(_, tree)| tree.to_string().chars().collect::<Vec<char>>());

        self.output.extend(
            result
                .chain(std::iter::once(combining_character))
                .map(|c| (span, TokenTree::Char(c))),
        );

        Ok(())
    }

    fn process_math_mode(&mut self, enable: bool, display: bool) -> Result<(), ExpandError> {
        match enable {
            true => match self.mode {
                Mode::Text => {
                    self.mode = Mode::Math {
                        display,
                        dollars: false,
                    };
                    Ok(())
                }
                Mode::Math { .. } => Err(ExpandError::MismatchedMathDelimiters),
            },
            false => match self.mode {
                Mode::Text => Err(ExpandError::MismatchedMathDelimiters),
                Mode::Math {
                    display: current_display,
                    dollars: false,
                } => {
                    if current_display == display {
                        self.mode = Mode::Text;
                        Ok(())
                    } else {
                        Err(ExpandError::MismatchedMathDelimiters)
                    }
                }
                Mode::Math { dollars: true, .. } => Err(ExpandError::MismatchedMathDelimiters),
            },
        }
    }

    fn process_user_macro(
        &mut self,
        span: SourceSpan,
        body: Vec<TokenTree>,
        args: Arguments,
    ) -> Result<(), ExpandError> {
        let result = self.replace_parameters(span, body, &args.args)?;
        self.output
            .extend(result.into_iter().map(|tree| (span, tree)));
        Ok(())
    }

    fn replace_parameters(
        &self,
        span: SourceSpan,
        body: Vec<TokenTree>,
        args: &[Vec<(SourceSpan, TokenTree)>],
    ) -> Result<Vec<TokenTree>, ExpandError> {
        let mut output = Vec::new();
        let mut iter = body.into_iter();
        while let Some(tree) = iter.next() {
            match tree {
                TokenTree::Char('#') => match iter.next() {
                    Some(TokenTree::Char(number @ '1'..='9')) => {
                        let index = number as usize - '1' as usize;
                        match args.get(index) {
                            Some(content) => {
                                output.extend(content.iter().map(|(_, tree)| tree.clone()))
                            }
                            None => todo!(),
                        }
                    }
                    _ => todo!(),
                },
                TokenTree::Block(block) => output.push(TokenTree::Block(
                    self.replace_parameters(
                        span,
                        block.into_iter().map(|(_, tree)| tree).collect(),
                        args,
                    )?
                    .into_iter()
                    .map(|tree| (span, tree))
                    .collect(),
                )),
                _ => output.push(tree),
            }
        }
        Ok(output)
    }
}
