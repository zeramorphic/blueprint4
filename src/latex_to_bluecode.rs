use std::iter::Peekable;

use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

use crate::{bluecode::*, latex_sections::TokenSection, latex_token_blocks::TokenBlock};

pub fn latex_to_bluecode(
    src: &str,
    src_name: &str,
    section: TokenSection,
) -> Result<Section, Vec<ConvertError>> {
    let section = convert_section(src, src_name, section)?;
    // TODO: Check for things like links with incorrect targets.
    Ok(section.split_newlines())
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
    #[error("(bug) unknown environment")]
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
    #[error("expected text")]
    ExpectedText,
}

fn convert_section(
    src: &str,
    src_name: &str,
    section: TokenSection,
) -> Result<Section, Vec<ConvertError>> {
    let mut blue_section = Section {
        name: section.name,
        level: section.level,
        numbered: section.numbered,
        label: None,
        contents: Vec::new(),
    };

    convert_all_text(
        src,
        src_name,
        &mut Container::Section(&mut blue_section),
        section.text,
    )?;

    for subsection in section.subsections {
        blue_section
            .contents
            .push(Item::Section(convert_section(src, src_name, subsection)?));
    }

    Ok(blue_section)
}

/// The item that we are currently writing text inside.
enum Container<'a> {
    Section(&'a mut Section),
    Theorem(&'a mut Theorem),
}

impl<'a> Container<'a> {
    pub fn push_paragraph(&mut self, paragraph: Paragraph) {
        match self {
            Container::Section(section) => section.contents.push(Item::Paragraph(paragraph)),
            Container::Theorem(theorem) => theorem.contents.push(paragraph),
        }
    }

    pub fn push_theorem(&mut self, theorem: Theorem) -> Result<(), ConvertError> {
        match self {
            Container::Section(section) => {
                section.contents.push(Item::Theorem(theorem));
                Ok(())
            }
            Container::Theorem(_) => todo!(),
        }
    }

    pub fn set_label(&mut self, label: String) -> Result<(), ConvertError> {
        match self {
            Container::Section(section) => match section.label {
                Some(_) => todo!(),
                None => {
                    section.label = Some(label);
                    Ok(())
                }
            },
            Container::Theorem(theorem) => match theorem.label {
                Some(_) => todo!(),
                None => {
                    theorem.label = Some(label);
                    Ok(())
                }
            },
        }
    }

    pub fn set_theorem_kind(&mut self, kind: String) -> Result<(), ConvertError> {
        match self {
            Container::Section(_) => todo!(),
            Container::Theorem(theorem) => {
                theorem.kind = kind;
                Ok(())
            }
        }
    }

    pub fn set_lean_name(&mut self, lean_name: String) -> Result<(), ConvertError> {
        match self {
            Container::Section(_) => todo!(),
            Container::Theorem(theorem) => match theorem.lean_name {
                Some(_) => todo!(),
                None => {
                    theorem.lean_name = Some(lean_name);
                    Ok(())
                }
            },
        }
    }

    pub fn set_proven(&mut self, proven: bool) -> Result<(), ConvertError> {
        match self {
            Container::Section(_) => todo!(),
            Container::Theorem(theorem) => {
                theorem.proven = proven;
                Ok(())
            }
        }
    }

    pub fn add_uses(&mut self, uses: impl IntoIterator<Item = String>) -> Result<(), ConvertError> {
        match self {
            Container::Section(_) => todo!(),
            Container::Theorem(theorem) => {
                theorem.uses.extend(uses);
                Ok(())
            }
        }
    }
}

/// Tries to parse some text and add it to the `container`.
fn convert_all_text(
    src: &str,
    src_name: &str,
    container: &mut Container,
    iter: impl IntoIterator<Item = (SourceSpan, TokenBlock)>,
) -> Result<(), Vec<ConvertError>> {
    let mut iter = iter.into_iter().peekable();
    let mut errors = Vec::<ConvertError>::new();
    while iter.peek().is_some() {
        if let Err(more) = convert_text(src, src_name, container, &mut iter) {
            errors.extend(more)
        }
    }
    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

/// Tries to parse some text and add it to the `container`.
fn convert_text(
    src: &str,
    src_name: &str,
    container: &mut Container,
    iter: &mut Peekable<impl Iterator<Item = (SourceSpan, TokenBlock)>>,
) -> Result<(), Vec<ConvertError>> {
    // Parse a span.
    if let Some(span) = convert_span(src, src_name, iter)? {
        container.push_paragraph(Paragraph::Text(span));
        return Ok(());
    }

    // Try to parse some non-span content.
    match iter.next() {
        Some((_, TokenBlock::Named(name))) => match name.as_str() {
            "label" => container
                .set_label(process_string_block(iter)?)
                .map_err(|err| vec![err]),
            "theoremkind" => container
                .set_theorem_kind(process_string_block(iter)?)
                .map_err(|err| vec![err]),
            "lean" => container
                .set_lean_name(process_string_block(iter)?)
                .map_err(|err| vec![err]),
            "leanok" => container.set_proven(true).map_err(|err| vec![err]),
            "uses" => container
                .add_uses(
                    process_string_block(iter)?
                        .split(',')
                        .map(|s| s.trim().to_owned()),
                )
                .map_err(|err| vec![err]),
            _ => todo!("{name}"),
        },
        Some((
            _,
            TokenBlock::Environment {
                begin_span,
                end_span,
                environment_span,
                environment,
                body,
            },
        )) => match environment.as_str() {
            "theorem" => {
                let mut theorem = Theorem {
                    kind: "<theorem>".to_owned(),
                    label: None,
                    lean_name: None,
                    uses: Vec::new(),
                    proven: false,
                    contents: Vec::new(),
                };
                convert_all_text(src, src_name, &mut Container::Theorem(&mut theorem), body)?;
                container.push_theorem(theorem).map_err(|err| vec![err])?;
                Ok(())
            }
            _ => todo!(),
        },
        Some((
            _,
            TokenBlock::Mathematics {
                display: true,
                contents,
            },
        )) => {
            container.push_paragraph(Paragraph::DisplayMath(blocks_to_mathematics(&contents)));
            Ok(())
        }
        Some((_, block)) => todo!("{block:?}"),
        None => Ok(()),
    }
}

fn convert_span(
    src: &str,
    src_name: &str,
    iter: &mut Peekable<impl Iterator<Item = (SourceSpan, TokenBlock)>>,
) -> Result<Option<Span>, Vec<ConvertError>> {
    let mut spans = Vec::new();
    loop {
        match iter.peek() {
            Some((_, TokenBlock::Named(name))) => match name.as_str() {
                "ref" => {
                    iter.next();
                    spans.push(Span::Reference(Reference {
                        name: process_string_block(iter)?,
                    }));
                }
                _ => break,
            },
            Some((_, TokenBlock::Char(c))) => {
                spans.push(Span::Text(c.to_string()));
                iter.next();
            }
            Some((
                _,
                TokenBlock::Mathematics {
                    display: false,
                    contents,
                },
            )) => {
                spans.push(Span::InlineMath(blocks_to_mathematics(contents)));
                iter.next();
            }
            _ => break,
        }
    }
    if spans.is_empty() {
        Ok(None)
    } else {
        Ok(Some(Span::Concatenate(spans)))
    }
}

fn blocks_to_mathematics<'a>(
    contents: impl IntoIterator<Item = &'a (SourceSpan, TokenBlock)>,
) -> Mathematics {
    Mathematics {
        mathjax_compatible: true,
        content: contents
            .into_iter()
            .map(|(_, block)| block.to_string())
            .collect(),
    }
}

fn process_string_block(
    iter: &mut Peekable<impl Iterator<Item = (SourceSpan, TokenBlock)>>,
) -> Result<String, Vec<ConvertError>> {
    match iter.next() {
        Some((_, TokenBlock::Block(tokens))) => process_string(tokens),
        _ => Err(vec![ConvertError::ExpectedText]),
    }
}

fn process_string(tokens: Vec<(SourceSpan, TokenBlock)>) -> Result<String, Vec<ConvertError>> {
    if tokens
        .iter()
        .any(|(_, tree)| !matches!(tree, TokenBlock::Char(_)))
    {
        return Err(vec![ConvertError::ExpectedText]);
    }

    Ok(tokens.iter().map(|(_, tree)| tree.to_string()).collect())
}
