use std::{fmt::Display, iter::Peekable};

use miette::{Diagnostic, NamedSource, SourceOffset, SourceSpan};
use thiserror::Error;

pub fn parse_latex(
    src: &str,
    src_name: &str,
) -> Result<Vec<(SourceSpan, TokenBlock)>, Vec<ParseError>> {
    let (tokens, errors) = tokenise(src.to_owned(), src_name.to_owned());

    if errors.is_empty() {
        match to_trees(tokens).and_then(to_blocks) {
            Ok(blocks) => Ok(blocks),
            Err(error) => Err(vec![error]),
        }
    } else {
        Err(errors)
    }
}

#[derive(Error, Debug, Diagnostic)]
pub enum ParseError {
    #[error("expected symbol")]
    ExpectedSymbol {
        #[source_code]
        src: NamedSource,
        #[label("backslash found here")]
        backslash: SourceSpan,
    },
    #[error("unmatched left brace")]
    UnmatchedLeftBrace,
    #[error("unmatched right brace")]
    UnmatchedRightBrace,
    #[error("expected environment name after \\begin")]
    ExpectedEnvironmentBegin,
    #[error("expected environment name after \\end")]
    ExpectedEnvironmentEnd,
    #[error("found \\begin and \\end with mismatched environments")]
    MismatchedEnvironments,
    #[error("unmatched \\begin")]
    UnmatchedBegin,
    #[error("unmatched \\end")]
    UnmatchedEnd,
    #[error("mismatched math mode delimiters")]
    MismatchedMathDelimiters,
}

#[derive(Debug)]
enum Token {
    /// A backslash `\` followed by the given string of letter characters.
    Named(String),
    /// A backslash `\` followed by the given non-letter character.
    Symbol(char),
    Char(char),
}

struct Tokeniser {
    src: String,
    src_name: String,
    output: Vec<(SourceSpan, Token)>,
    errors: Vec<ParseError>,
}

impl Tokeniser {
    fn new(src: String, src_name: String) -> Self {
        Self {
            src,
            src_name,
            output: Vec::new(),
            errors: Vec::new(),
        }
    }

    fn advance(&mut self, iter: &mut Peekable<impl Iterator<Item = (usize, char)>>) {
        match iter.next() {
            None => {}
            Some((_, '%')) => {
                // This is an unescaped comment character.
                while iter.next().is_some_and(|(_, c)| c != '\n') {}
            }
            Some((offset, '\\')) => {
                if iter.peek().is_some_and(|(_, c)| c.is_alphabetic()) {
                    // Parse a named command.
                    let mut result = String::new();
                    while iter.peek().is_some_and(|(_, c)| c.is_alphabetic()) {
                        result.push(iter.next().unwrap().1);
                    }
                    self.output.push((
                        SourceSpan::new(offset.into(), (result.len() + 1).into()),
                        Token::Named(result),
                    ));
                } else {
                    // Parse a command symbol.
                    match iter.next() {
                        None => self.errors.push(ParseError::ExpectedSymbol {
                            src: NamedSource::new(&self.src_name, self.src.clone()),
                            backslash: SourceSpan::new(offset.into(), 1.into()),
                        }),
                        Some((_, c)) => self.output.push((
                            SourceSpan::new(offset.into(), (c.to_string().len() + 1).into()),
                            Token::Symbol(c),
                        )),
                    }
                }
            }
            Some((offset, c)) => self.output.push((
                SourceSpan::new(offset.into(), c.to_string().len().into()),
                Token::Char(c),
            )),
        }
    }
}

fn tokenise(src: String, src_name: String) -> (Vec<(SourceSpan, Token)>, Vec<ParseError>) {
    let mut tokeniser = Tokeniser::new(src, src_name);
    let mut iter = tokeniser
        .src
        .char_indices()
        .collect::<Vec<_>>()
        .into_iter()
        .peekable();
    while iter.peek().is_some() {
        tokeniser.advance(&mut iter);
    }
    (tokeniser.output, tokeniser.errors)
}

#[derive(Debug)]
enum TokenTree {
    /// A backslash `\` followed by the given string of letter characters.
    Named(String),
    /// A backslash `\` followed by the given non-letter character.
    Symbol(char),
    Char(char),
    /// A block of token trees encased in brace brackets.
    Block(Vec<(SourceSpan, TokenTree)>),
}

/// Render this TokenTree to a LaTeX string.
impl Display for TokenTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenTree::Named(name) => write!(f, "\\{name}"),
            TokenTree::Symbol(symbol) => write!(f, "\\{symbol}"),
            TokenTree::Char(c) => write!(f, "{c}"),
            TokenTree::Block(contents) => {
                write!(f, "{{")?;
                for (_, tree) in contents {
                    write!(f, "{tree}")?;
                }
                write!(f, "}}")
            }
        }
    }
}

fn to_trees(tokens: Vec<(SourceSpan, Token)>) -> Result<Vec<(SourceSpan, TokenTree)>, ParseError> {
    // The stack is a collection of regions delimited by braces that we're currently inside.
    // This should always be nonempty.
    // The SourceOffset is the start of the region.
    // Parsing a `{` causes us to push a new region onto the stack.
    // Parsing a `}` causes us to pop a region off the stack and push it onto the previous one as a block.
    let mut stack: Vec<(SourceOffset, Vec<(SourceSpan, TokenTree)>)> = Vec::new();
    stack.push((0.into(), Vec::new()));

    for (span, token) in tokens {
        match token {
            Token::Char('{') => stack.push((span.offset().into(), Vec::new())),
            Token::Char('}') => {
                let (start, block) = stack.pop().unwrap();
                if stack.is_empty() {
                    return Err(ParseError::UnmatchedRightBrace);
                }
                stack.last_mut().unwrap().1.push((
                    SourceSpan::new(start, (span.offset() - start.offset() + 1).into()),
                    TokenTree::Block(block),
                ))
            }
            Token::Named(name) => stack
                .last_mut()
                .unwrap()
                .1
                .push((span, TokenTree::Named(name))),
            Token::Symbol(symbol) => stack
                .last_mut()
                .unwrap()
                .1
                .push((span, TokenTree::Symbol(symbol))),
            Token::Char(c) => stack.last_mut().unwrap().1.push((span, TokenTree::Char(c))),
        }
    }

    if stack.len() > 1 {
        return Err(ParseError::UnmatchedLeftBrace);
    }

    Ok(stack.pop().unwrap().1)
}

#[derive(Debug)]
pub enum TokenBlock {
    /// A backslash `\` followed by the given string of letter characters.
    Named(String),
    /// A backslash `\` followed by the given non-letter character.
    Symbol(char),
    Char(char),
    /// A block of token trees encased in brace brackets.
    Block(Vec<(SourceSpan, TokenBlock)>),
    /// Some tokens to be rendered in math mode.
    Mathematics {
        /// True if this block was delimited by `$$` or `\[` and `\]`;
        /// false if this block was delimited by `$` or `\(` and `/)`.
        display: bool,
        /// The raw contents of the mathematics, with comments removed.
        contents: Vec<(SourceSpan, TokenBlock)>,
    },
    /// An environment, delimited by `\begin` and `\end` commands.
    Environment {
        begin_span: SourceSpan,
        end_span: SourceSpan,
        environment_span: SourceSpan,
        environment: String,
        body: Vec<(SourceSpan, TokenBlock)>,
    },
}

/// Render this TokenBlock to a LaTeX string.
impl Display for TokenBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenBlock::Named(name) => write!(f, "\\{name}"),
            TokenBlock::Symbol(symbol) => write!(f, "\\{symbol}"),
            TokenBlock::Char(c) => write!(f, "{c}"),
            TokenBlock::Block(contents) => {
                write!(f, "{{")?;
                for (_, tree) in contents {
                    write!(f, "{tree}")?;
                }
                write!(f, "}}")
            }
            TokenBlock::Mathematics { display, contents } => {
                if *display {
                    write!(f, "\\[ ")?;
                    for (_, tree) in contents {
                        write!(f, "{tree}")?;
                    }
                    write!(f, " \\]")
                } else {
                    write!(f, "\\( ")?;
                    for (_, tree) in contents {
                        write!(f, "{tree}")?;
                    }
                    write!(f, " \\)")
                }
            }
            TokenBlock::Environment {
                environment, body, ..
            } => {
                write!(f, "\\begin{{{environment}}} ")?;
                for (_, tree) in body {
                    write!(f, "{tree}")?;
                }
                write!(f, " \\end{{{environment}}}")
            }
        }
    }
}

#[derive(PartialEq, Eq)]
enum PartialEnvironment {
    Environment(String),
    MathMode {
        /// True if we're in display math mode.
        display: bool,
        /// True if we're using `$`/`$$` instead of `\(\)`/`\[\]`.
        dollars: bool,
    },
}

/// A block under construction.
/// This is either an environment or a math mode block.
struct PartialBlock {
    /// The span of the `\begin` or `$`/`$$`/`\(`/`\[` token.
    begin_span: SourceSpan,
    /// The span of the environment name.
    environment_span: SourceSpan,
    /// The name of the environment.
    environment: PartialEnvironment,
    contents: Vec<(SourceSpan, TokenBlock)>,
}

/// A collection of environments delimited by `\begin`/`\end` that we're currently inside.
/// This should always be nonempty.
/// The SourceSpan is the span of the `\begin` token, and the String is the name of the environment.
/// Parsing a `\begin` causes us to push a new environment onto the stack.
/// Parsing an `\end` causes us to pop a environment off the stack and push it onto the previous one.
struct BlockStack {
    stack: Vec<PartialBlock>,
}

impl BlockStack {
    fn new() -> Self {
        Self {
            stack: vec![PartialBlock {
                begin_span: 0.into(),
                environment_span: 0.into(),
                environment: PartialEnvironment::Environment("<document>".to_owned()),
                contents: Vec::new(),
            }],
        }
    }

    fn push(&mut self, span: SourceSpan, block: TokenBlock) {
        self.stack.last_mut().unwrap().contents.push((span, block))
    }

    fn finalise(mut self) -> Result<Vec<(SourceSpan, TokenBlock)>, ParseError> {
        if self.stack.len() > 1 {
            return Err(ParseError::UnmatchedBegin);
        }

        Ok(self.stack.pop().unwrap().contents)
    }

    fn parse_begin(
        &mut self,
        span: SourceSpan,
        iter: &mut impl Iterator<Item = (SourceSpan, TokenTree)>,
    ) -> Result<(), ParseError> {
        match iter.next() {
            Some((environment_span, TokenTree::Block(contents))) => {
                let environment = contents
                    .iter()
                    .map(|(_, tree)| tree.to_string())
                    .collect::<String>();
                self.begin(
                    span,
                    environment_span,
                    PartialEnvironment::Environment(environment),
                );
                Ok(())
            }
            _ => Err(ParseError::ExpectedEnvironmentBegin),
        }
    }

    fn begin(
        &mut self,
        begin_span: SourceSpan,
        environment_span: SourceSpan,
        environment: PartialEnvironment,
    ) {
        self.stack.push(PartialBlock {
            begin_span,
            environment_span,
            environment,
            contents: Vec::new(),
        });
    }

    fn parse_end(
        &mut self,
        span: SourceSpan,
        iter: &mut impl Iterator<Item = (SourceSpan, TokenTree)>,
    ) -> Result<(), ParseError> {
        match iter.next() {
            Some((_, TokenTree::Block(contents))) => {
                let expected_env_name = contents
                    .iter()
                    .map(|(_, tree)| tree.to_string())
                    .collect::<String>();
                let partial_block = self.stack.pop().unwrap();
                if self.stack.is_empty() {
                    Err(ParseError::UnmatchedEnd)
                } else if PartialEnvironment::Environment(expected_env_name.clone())
                    != partial_block.environment
                {
                    Err(ParseError::MismatchedEnvironments)
                } else {
                    self.end(partial_block, span, expected_env_name);
                    Ok(())
                }
            }
            _ => Err(ParseError::ExpectedEnvironmentEnd),
        }
    }

    fn end(&mut self, partial_block: PartialBlock, end_span: SourceSpan, environment: String) {
        self.push(
            SourceSpan::new(
                partial_block.begin_span.offset().into(),
                (end_span.offset() + end_span.len() - partial_block.begin_span.offset()).into(),
            ),
            TokenBlock::Environment {
                begin_span: partial_block.begin_span,
                end_span,
                environment_span: partial_block.environment_span,
                environment,
                body: partial_block.contents,
            },
        );
    }

    fn end_math(&mut self, end_span: SourceSpan) -> Result<(), ParseError> {
        let partial_block = self.stack.pop().unwrap();
        if self.stack.is_empty() {
            return Err(ParseError::UnmatchedEnd);
        }

        let display = match partial_block.environment {
            PartialEnvironment::Environment(_) => return Err(ParseError::UnmatchedEnd),
            PartialEnvironment::MathMode { display, .. } => display,
        };

        self.push(
            SourceSpan::new(
                partial_block.begin_span.offset().into(),
                (end_span.offset() + end_span.len() - partial_block.begin_span.offset()).into(),
            ),
            TokenBlock::Mathematics {
                display,
                contents: partial_block.contents,
            },
        );
        Ok(())
    }
}

fn to_blocks(
    token_trees: Vec<(SourceSpan, TokenTree)>,
) -> Result<Vec<(SourceSpan, TokenBlock)>, ParseError> {
    let mut stack = BlockStack::new();

    let mut iter = token_trees.into_iter().peekable();
    while let Some((span, tree)) = iter.next() {
        match tree {
            TokenTree::Named(name) => {
                if name == "begin" {
                    stack.parse_begin(span, &mut iter)?;
                } else if name == "end" {
                    stack.parse_end(span, &mut iter)?;
                } else {
                    stack.push(span, TokenBlock::Named(name));
                }
            }
            TokenTree::Char('$') => {
                if let PartialEnvironment::MathMode { display, dollars } =
                    stack.stack.last().unwrap().environment
                {
                    // End a math mode block.
                    if let Some((_, TokenTree::Char('$'))) = iter.peek() {
                        iter.next();
                        if !display || !dollars {
                            return Err(ParseError::MismatchedMathDelimiters);
                        }
                        stack.end_math(span)?;
                    } else {
                        if display || !dollars {
                            return Err(ParseError::MismatchedMathDelimiters);
                        }
                        stack.end_math(span)?;
                    }
                } else {
                    // Start a math mode block.
                    if let Some((_, TokenTree::Char('$'))) = iter.peek() {
                        iter.next();
                        stack.begin(
                            SourceSpan::new(span.offset().into(), 2.into()),
                            SourceSpan::new(span.offset().into(), 2.into()),
                            PartialEnvironment::MathMode {
                                display: true,
                                dollars: true,
                            },
                        );
                    } else {
                        stack.begin(
                            span,
                            span,
                            PartialEnvironment::MathMode {
                                display: false,
                                dollars: true,
                            },
                        );
                    }
                }
            }
            TokenTree::Symbol('[') => {
                stack.begin(
                    span,
                    span,
                    PartialEnvironment::MathMode {
                        display: true,
                        dollars: false,
                    },
                );
            }
            TokenTree::Symbol('(') => {
                stack.begin(
                    span,
                    span,
                    PartialEnvironment::MathMode {
                        display: true,
                        dollars: false,
                    },
                );
            }
            TokenTree::Symbol(']') => {
                if let PartialEnvironment::MathMode { display, dollars } =
                    stack.stack.last().unwrap().environment
                {
                    if !display || dollars {
                        return Err(ParseError::MismatchedMathDelimiters);
                    }
                    stack.end_math(span)?;
                } else {
                    return Err(ParseError::MismatchedMathDelimiters);
                }
            }
            TokenTree::Symbol(')') => {
                if let PartialEnvironment::MathMode { display, dollars } =
                    stack.stack.last().unwrap().environment
                {
                    if display || dollars {
                        return Err(ParseError::MismatchedMathDelimiters);
                    }
                    stack.end_math(span)?;
                } else {
                    return Err(ParseError::MismatchedMathDelimiters);
                }
            }
            TokenTree::Symbol(symbol) => stack.push(span, TokenBlock::Symbol(symbol)),
            TokenTree::Char(c) => stack.push(span, TokenBlock::Char(c)),
            TokenTree::Block(block) => {
                stack.push(span, TokenBlock::Block(to_blocks(block)?));
            }
        }
    }

    stack.finalise().map(|blocks| {
        blocks
            .into_iter()
            .map(|(span, block)| (span, wrap_math_environments(block)))
            .collect()
    })
}

/// Wraps math-mode environments like `align` or `multline` in a display-math block.
fn wrap_math_environments(block: TokenBlock) -> TokenBlock {
    // The following list is taken from <https://docs.mathjax.org/en/latest/input/tex/macros/index.html#environments>.
    let math_mode_environments = [
        "align",
        "align*",
        "alignat",
        "alignat*",
        "aligned",
        "alignedat",
        "array",
        "bmatrix",
        "Bmatrix",
        "bmatrix*",
        "Bmatrix*",
        "bsmallmatrix",
        "Bsmallmatrix",
        "bsmallmatrix*",
        "cases",
        "cases*",
        "CD",
        "crampedsubarray",
        "dcases",
        "dcases*",
        "drcases",
        "drcases*",
        "empheq",
        "eqnarray",
        "eqnarray*",
        "equation",
        "equation*",
        "flalign",
        "flalign*",
        "gather",
        "gather*",
        "gathered",
        "lgathered",
        "matrix",
        "matrix*",
        "multline",
        "multline*",
        "multlined",
        "numcases",
        "pmatrix",
        "pmatrix*",
        "prooftree",
        "psmallmatrix",
        "psmallmatrix*",
        "rcases",
        "rcases*",
        "rgathered",
        "smallmatrix",
        "smallmatrix*",
        "split",
        "spreadlines",
        "subarray",
        "subnumcases",
        "vmatrix",
        "Vmatrix",
        "vmatrix*",
        "Vmatrix*",
        "vsmallmatrix",
        "Vsmallmatrix",
        "vsmallmatrix*",
        "Vsmallmatrix*",
        "xalignat",
        "xalignat*",
        "xxalignat",
    ];

    match block {
        TokenBlock::Block(block) => TokenBlock::Block(
            block
                .into_iter()
                .map(|(span, block)| (span, wrap_math_environments(block)))
                .collect(),
        ),
        TokenBlock::Environment {
            begin_span,
            environment_span,
            end_span,
            environment,
            body,
        } => {
            if math_mode_environments.contains(&environment.as_str()) {
                TokenBlock::Mathematics {
                    display: true,
                    contents: vec![(
                        SourceSpan::new(
                            begin_span.offset().into(),
                            (end_span.offset() + end_span.len() - begin_span.offset()).into(),
                        ),
                        TokenBlock::Environment {
                            begin_span,
                            environment_span,
                            end_span,
                            environment,
                            body,
                        },
                    )],
                }
            } else {
                TokenBlock::Environment {
                    begin_span,
                    environment_span,
                    end_span,
                    environment,
                    body: body
                        .into_iter()
                        .map(|(span, block)| (span, wrap_math_environments(block)))
                        .collect(),
                }
            }
        }
        _ => block,
    }
}
