use std::{fmt::Display, iter::Peekable};

use miette::{Diagnostic, NamedSource, SourceOffset, SourceSpan};
use thiserror::Error;

pub fn parse_latex(
    src: &str,
    src_name: &str,
) -> Result<Vec<(SourceSpan, TokenTree)>, Vec<ParseError>> {
    let (tokens, errors) = tokenise(src.to_owned(), src_name.to_owned());

    if errors.is_empty() {
        match to_trees(tokens) {
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

#[derive(Debug, Clone)]
pub enum TokenTree {
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

impl TokenTree {
    /// Overwrites all `SourceSpan` fields with `span`.
    pub fn overwrite_spans(self, span: SourceSpan) -> Self {
        match self {
            TokenTree::Block(block) => TokenTree::Block(
                block
                    .into_iter()
                    .map(|(_, inner)| (span, inner.overwrite_spans(span)))
                    .collect(),
            ),
            _ => self,
        }
    }

    /// Converts a string to a vector of `Char` token trees.
    pub fn from_str(s: &str) -> Vec<Self> {
        s.chars().map(Self::Char).collect()
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
