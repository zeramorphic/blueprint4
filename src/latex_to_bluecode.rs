use std::iter::Peekable;

use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

use crate::{bluecode::*, latex_token_blocks::TokenBlock};

pub fn latex_to_bluecode(
    src: &str,
    src_name: &str,
    latex: Vec<(SourceSpan, TokenBlock)>,
) -> Result<Vec<Item>, Vec<ConvertError>> {
    let mut iter = latex.into_iter().peekable();
    let mut items = Vec::new();
    let mut errors = Vec::new();

    let mut previous_span = None;
    while iter.peek().is_some() {
        // If we consumed no token blocks, consume one anyway.
        let span = iter.peek().unwrap().0;
        if previous_span == Some(span) {
            iter.next();
        }
        previous_span = Some(span);

        match parse_item(src, src_name, &mut iter) {
            Ok(item) => items.push(item),
            Err(more_errors) => errors.extend(more_errors),
        }
    }

    if errors.is_empty() {
        Ok(items)
    } else {
        Err(errors)
    }
}

#[derive(Error, Debug, Diagnostic)]
pub enum ConvertError {
    #[error("(bug) unknown command")]
    UnknownCommand {
        #[source_code]
        src: NamedSource,
        #[label("unknown command found here")]
        span: SourceSpan,
    },
    #[error("unknown environment")]
    UnknownEnvironment {
        #[source_code]
        src: NamedSource,
        #[label("unknown environment found here")]
        span: SourceSpan,
    },
    #[error("expected argument")]
    ExpectedArgument,
    #[error("unrecognised input in text mode")]
    UnrecognisedText {
        #[source_code]
        src: NamedSource,
        #[label("unrecognised input found here")]
        span: SourceSpan,
    },
}

fn parse_item(
    src: &str,
    src_name: &str,
    iter: &mut Peekable<impl Iterator<Item = (SourceSpan, TokenBlock)>>,
) -> Result<Item, Vec<ConvertError>> {
    match iter.peek() {
        Some((_, TokenBlock::Named(name))) => {
            let name = name.clone();
            iter.next();
            match name.as_str() {
                "part" => Ok(Item::Header(Header {
                    name: consume_arg(iter)?,
                    level: -1,
                })),
                "chapter" => Ok(Item::Header(Header {
                    name: consume_arg(iter)?,
                    level: 0,
                })),
                "section" => Ok(Item::Header(Header {
                    name: consume_arg(iter)?,
                    level: 1,
                })),
                "subsection" => Ok(Item::Header(Header {
                    name: consume_arg(iter)?,
                    level: 2,
                })),
                "subsubsection" => Ok(Item::Header(Header {
                    name: consume_arg(iter)?,
                    level: 3,
                })),
                "paragraph" => Ok(Item::Header(Header {
                    name: consume_arg(iter)?,
                    level: 4,
                })),
                "subparagraph" => Ok(Item::Header(Header {
                    name: consume_arg(iter)?,
                    level: 5,
                })),
                _ => parse_paragraph(src, src_name, iter).map(Item::Paragraph),
            }
        }
        Some((
            _,
            TokenBlock::Environment {
                environment_span, ..
            },
        )) => {
            let environment_span = *environment_span;
            match iter.next() {
                Some((
                    _,
                    TokenBlock::Environment {
                        environment, body, ..
                    },
                )) => match environment.as_str() {
                    "lemma" | "theorem" | "proof" => {
                        parse_theorem(src, src_name, environment, body).map(Item::Theorem)
                    }
                    _ => Err(vec![ConvertError::UnknownEnvironment {
                        src: NamedSource::new(src_name, src.to_owned()),
                        span: environment_span,
                    }]),
                },
                _ => unreachable!(),
            }
        }
        _ => parse_paragraph(src, src_name, iter).map(Item::Paragraph),
    }
}

/// Consume a single argument.
/// This can be a block, a command, or a single character.
fn consume_arg(
    iter: &mut Peekable<impl Iterator<Item = (SourceSpan, TokenBlock)>>,
) -> Result<String, Vec<ConvertError>> {
    match iter.next() {
        Some((_, TokenBlock::Block(contents))) => Ok(contents
            .iter()
            .map(|(_, block)| block.to_string())
            .collect()),
        Some((_, tree)) => Ok(tree.to_string()),
        None => Err(vec![ConvertError::ExpectedArgument]),
    }
}

fn parse_paragraph(
    src: &str,
    src_name: &str,
    iter: &mut Peekable<impl Iterator<Item = (SourceSpan, TokenBlock)>>,
) -> Result<Paragraph, Vec<ConvertError>> {
    if let Some((
        _,
        TokenBlock::Mathematics {
            display: true,
            contents,
        },
    )) = iter.peek()
    {
        let content = contents
            .iter()
            .map(|(_, block)| block.to_string())
            .collect();
        iter.next();
        return Ok(Paragraph::DisplayMath(Mathematics {
            mathjax_compatible: true,
            content,
        }));
    }

    let mut spans = Vec::new();

    loop {
        match iter.peek() {
            Some((span, TokenBlock::Named(name))) => {
                let span = *span;
                let name = name.clone();
                iter.next();
                match name.as_str() {
                    "label" => spans.push(Span::Label(Label {
                        name: consume_arg(iter)?,
                    })),
                    "ref" => spans.push(Span::Reference(Reference {
                        name: consume_arg(iter)?,
                    })),
                    "lean" => spans.push(Span::LeanLink(LeanLink {
                        lean_name: consume_arg(iter)?,
                    })),
                    "uses" => spans.push(Span::Uses(Uses {
                        uses: consume_arg(iter)?
                            .split(',')
                            .map(|s| s.trim().to_owned())
                            .collect(),
                    })),
                    "leanok" => spans.push(Span::LeanOk),
                    _ => {
                        return Err(vec![ConvertError::UnknownCommand {
                            src: NamedSource::new(src_name, src.to_owned()),
                            span,
                        }])
                    }
                }
            }
            Some((span, TokenBlock::Symbol(_))) => {
                let span = *span;
                iter.next();
                return Err(vec![ConvertError::UnknownCommand {
                    src: NamedSource::new(src_name, src.to_owned()),
                    span,
                }]);
            }
            Some((
                _,
                TokenBlock::Mathematics {
                    display: false,
                    contents,
                },
            )) => {
                let content = contents
                    .iter()
                    .map(|(_, block)| block.to_string())
                    .collect();
                spans.push(Span::InlineMath(Mathematics {
                    mathjax_compatible: true,
                    content,
                }));
                iter.next();
            }
            Some((_, TokenBlock::Char(_))) => spans.push(Span::Text(parse_text_span(iter)?)),
            _ => break,
        }
    }

    if spans.is_empty() {
        Err(vec![ConvertError::UnrecognisedText {
            src: NamedSource::new(src_name, src.to_owned()),
            span: {
                match iter.peek() {
                    Some((span, _)) => *span,
                    None => 0.into(),
                }
            },
        }])
    } else {
        Ok(Paragraph::Text(spans))
    }
}

fn parse_text_span(
    iter: &mut Peekable<impl Iterator<Item = (SourceSpan, TokenBlock)>>,
) -> Result<String, Vec<ConvertError>> {
    let mut string = String::new();
    loop {
        match iter.peek() {
            Some((_, TokenBlock::Char('\n'))) => {
                iter.next();
                break;
            }
            Some((_, TokenBlock::Char(c))) => {
                string.push(*c);
                iter.next();
            }
            _ => break,
        }
    }
    Ok(string.trim_matches(&['\r', '\n']).to_owned())
}

/// Parse this iterator to its end, turning it into a list of paragraphs.
fn parse_paragraphs(
    src: &str,
    src_name: &str,
    iter: &mut Peekable<impl Iterator<Item = (SourceSpan, TokenBlock)>>,
) -> Result<Vec<Paragraph>, Vec<ConvertError>> {
    let mut result = Vec::new();
    while iter.peek().is_some() {
        result.push(parse_paragraph(src, src_name, iter)?);
    }
    Ok(result)
}

/// Parse a theorem environment.
fn parse_theorem(
    src: &str,
    src_name: &str,
    kind: String,
    body: Vec<(SourceSpan, TokenBlock)>,
) -> Result<Theorem, Vec<ConvertError>> {
    // TODO: Parse options to the theorem.
    parse_paragraphs(src, src_name, &mut body.into_iter().peekable())
        .map(|paragraphs| Theorem { kind, paragraphs })
}
