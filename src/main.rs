use std::collections::BTreeSet;

use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

fn main() {
    let (tokens, errors) = Tokeniser::new(
        std::fs::read_to_string("test/LeanAPAP/ap.tex").unwrap(),
        "test/LeanAPAP/ap.tex".to_owned(),
    )
    .parse();

    let names = tokens
        .iter()
        .filter_map(|token| match token {
            Token::Named(name) => Some(name),
            _ => None,
        })
        .collect::<BTreeSet<_>>();

    println!("{names:?}");

    let symbols = tokens
        .iter()
        .filter_map(|token| match token {
            Token::Symbol(symbol) => Some(symbol),
            _ => None,
        })
        .collect::<BTreeSet<_>>();

    println!("{symbols:?}");

    for error in errors {
        println!("{:?}", miette::Report::new(error));
    }
}

#[derive(Debug)]
enum Token {
    /// A backslash `\` followed by the given string of letter characters.
    Named(String),
    /// A backslash `\` followed by the given non-letter character.
    Symbol(char),
    Char(char),
}

#[derive(Error, Debug, Diagnostic)]
enum TokeniserError {
    #[error("expected symbol")]
    ExpectedSymbol {
        #[source_code]
        src: NamedSource,
        #[label("backslash found here")]
        backslash: SourceSpan,
    },
}

struct Tokeniser {
    src: String,
    src_name: String,
    input: std::iter::Peekable<std::vec::IntoIter<(usize, char)>>,
    output: Vec<Token>,
    errors: Vec<TokeniserError>,
}

impl Tokeniser {
    pub fn new(src: String, src_name: String) -> Self {
        let input = src
            .char_indices()
            .collect::<Vec<_>>()
            .into_iter()
            .peekable();
        Self {
            src,
            src_name,
            input,
            output: Vec::new(),
            errors: Vec::new(),
        }
    }

    fn advance(&mut self) {
        match self.input.next() {
            None => {}
            Some((offset, '\\')) => {
                if self.input.peek().is_some_and(|(_, c)| c.is_alphabetic()) {
                    // Parse a named command.
                    let mut result = String::new();
                    while self.input.peek().is_some_and(|(_, c)| c.is_alphabetic()) {
                        result.push(self.input.next().unwrap().1);
                    }
                    self.output.push(Token::Named(result));
                } else {
                    // Parse a command symbol.
                    match self.input.next() {
                        None => self.errors.push(TokeniserError::ExpectedSymbol {
                            src: NamedSource::new(&self.src_name, self.src.clone()),
                            backslash: SourceSpan::new(offset.into(), 1.into()),
                        }),
                        Some((_, c)) => self.output.push(Token::Symbol(c)),
                    }
                }
            }
            Some((_, c)) => self.output.push(Token::Char(c)),
        }
    }

    pub fn parse(mut self) -> (Vec<Token>, Vec<TokeniserError>) {
        while self.input.peek().is_some() {
            self.advance();
        }
        (self.output, self.errors)
    }
}
