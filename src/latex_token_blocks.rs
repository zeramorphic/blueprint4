use std::fmt::Display;

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::{parse_latex::TokenTree, resource};

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

#[derive(Error, Debug, Diagnostic)]
pub enum BlockError {
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

    fn finalise(mut self) -> Result<Vec<(SourceSpan, TokenBlock)>, BlockError> {
        if self.stack.len() > 1 {
            return Err(BlockError::UnmatchedBegin);
        }

        Ok(self.stack.pop().unwrap().contents)
    }

    fn parse_begin(
        &mut self,
        span: SourceSpan,
        iter: &mut impl Iterator<Item = (SourceSpan, TokenTree)>,
    ) -> Result<(), BlockError> {
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
            _ => Err(BlockError::ExpectedEnvironmentBegin),
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
    ) -> Result<(), BlockError> {
        match iter.next() {
            Some((_, TokenTree::Block(contents))) => {
                let expected_env_name = contents
                    .iter()
                    .map(|(_, tree)| tree.to_string())
                    .collect::<String>();
                let partial_block = self.stack.pop().unwrap();
                if self.stack.is_empty() {
                    Err(BlockError::UnmatchedEnd)
                } else if PartialEnvironment::Environment(expected_env_name.clone())
                    != partial_block.environment
                {
                    Err(BlockError::MismatchedEnvironments)
                } else {
                    self.end(partial_block, span, expected_env_name);
                    Ok(())
                }
            }
            _ => Err(BlockError::ExpectedEnvironmentEnd),
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

    fn end_math(&mut self, end_span: SourceSpan) -> Result<(), BlockError> {
        let partial_block = self.stack.pop().unwrap();
        if self.stack.is_empty() {
            return Err(BlockError::UnmatchedEnd);
        }

        let display = match partial_block.environment {
            PartialEnvironment::Environment(_) => return Err(BlockError::UnmatchedEnd),
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

pub fn trees_to_blocks(
    _src: &str,
    _src_name: &str,
    token_trees: Vec<(SourceSpan, TokenTree)>,
) -> Result<Vec<(SourceSpan, TokenBlock)>, BlockError> {
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
                            return Err(BlockError::MismatchedMathDelimiters);
                        }
                        stack.end_math(span)?;
                    } else {
                        if display || !dollars {
                            return Err(BlockError::MismatchedMathDelimiters);
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
                        return Err(BlockError::MismatchedMathDelimiters);
                    }
                    stack.end_math(span)?;
                } else {
                    return Err(BlockError::MismatchedMathDelimiters);
                }
            }
            TokenTree::Symbol(')') => {
                if let PartialEnvironment::MathMode { display, dollars } =
                    stack.stack.last().unwrap().environment
                {
                    if display || dollars {
                        return Err(BlockError::MismatchedMathDelimiters);
                    }
                    stack.end_math(span)?;
                } else {
                    return Err(BlockError::MismatchedMathDelimiters);
                }
            }
            TokenTree::Symbol(symbol) => stack.push(span, TokenBlock::Symbol(symbol)),
            TokenTree::Char(c) => stack.push(span, TokenBlock::Char(c)),
            TokenTree::Block(block) => {
                stack.push(
                    span,
                    TokenBlock::Block(trees_to_blocks(_src, _src_name, block)?),
                );
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
    let math_mode_environments: Vec<&str> = resource!("environments_math_mode.txt");

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
