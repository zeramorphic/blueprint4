use std::{fmt::Display, iter::Peekable};

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::latex_token_blocks::TokenBlock;

pub fn process_sections(
    src: &str,
    src_name: &str,
    latex: Vec<(SourceSpan, TokenBlock)>,
) -> Result<TokenSection, Vec<SectionError>> {
    let mut iter = latex.into_iter().peekable();
    let mut converter = Converter::default();
    let mut errors = Vec::new();

    while let Some((span, block)) = iter.next() {
        match converter.process(span, block, &mut iter) {
            Ok(()) => {}
            Err(err) => errors.push(err),
        }
    }

    while converter.active_sections.len() > 1 {
        let section = converter.active_sections.pop().unwrap();
        converter.active_section().subsections.push(section);
    }

    if errors.is_empty() {
        Ok(converter.active_sections.pop().unwrap())
    } else {
        Err(errors)
    }
}

#[derive(Debug)]
pub struct TokenSection {
    pub span: SourceSpan,
    pub name: String,
    /// The header level:
    ///
    /// | number | name          |
    /// |--------|---------------|
    /// | -1     | part          |
    /// | 0      | chapter       |
    /// | 1      | section       |
    /// | 2      | subsection    |
    /// | 3      | subsubsection |
    /// | 4      | paragraph     |
    /// | 5      | subparagraph  |
    pub level: i32,
    pub numbered: bool,
    /// The text that appears before any subsection heading.
    pub text: Vec<(SourceSpan, TokenBlock)>,
    /// The subsections contained inside this one, positioned after the first subsection heading.
    pub subsections: Vec<TokenSection>,
}

#[derive(Error, Debug, Diagnostic)]
pub enum SectionError {
    #[error("expected text")]
    ExpectedText,
}

struct Converter {
    /// A list of the sections we're currently inside, from lowest level to highest.
    /// This always contains a minimal section for the entire document.
    active_sections: Vec<TokenSection>,
}

impl Default for Converter {
    fn default() -> Self {
        Self {
            active_sections: vec![TokenSection {
                span: 0.into(),
                name: "<document>".to_owned(),
                level: i32::MIN,
                numbered: false,
                text: Vec::new(),
                subsections: Vec::new(),
            }],
        }
    }
}

impl Converter {
    pub fn process(
        &mut self,
        span: SourceSpan,
        block: TokenBlock,
        iter: &mut Peekable<impl Iterator<Item = (SourceSpan, TokenBlock)>>,
    ) -> Result<(), SectionError> {
        match block {
            TokenBlock::Named(name) => {
                let level = match name.as_str() {
                    "part" => Some(-1),
                    "chapter" => Some(0),
                    "section" => Some(1),
                    "subsection" => Some(2),
                    "subsubsection" => Some(3),
                    "paragraph" => Some(4),
                    "subparagraph," => Some(5),
                    _ => None,
                };

                if let Some(level) = level {
                    self.begin_section(span, level, iter)
                } else {
                    self.active_section()
                        .text
                        .push((span, TokenBlock::Named(name)));
                    Ok(())
                }
            }
            _ => {
                self.active_section().text.push((span, block));
                Ok(())
            }
        }
    }

    fn active_section(&mut self) -> &mut TokenSection {
        self.active_sections.last_mut().unwrap()
    }

    fn begin_section(
        &mut self,
        span: SourceSpan,
        level: i32,
        iter: &mut Peekable<impl Iterator<Item = (SourceSpan, TokenBlock)>>,
    ) -> Result<(), SectionError> {
        // This is the beginning of a section.
        while self.active_section().level >= level {
            // The currently active section is now finished.
            let section = self.active_sections.pop().unwrap();
            self.active_section().subsections.push(section);
        }

        // Process the star if it exists, as well as the section name.
        let star = matches!(iter.peek(), Some((_, TokenBlock::Char('*'))));
        if star {
            iter.next();
        }
        let name = self.process_string_block(iter)?;

        // Push this new section onto the section stack.
        self.active_sections.push(TokenSection {
            span,
            name,
            level,
            numbered: !star,
            text: Vec::new(),
            subsections: Vec::new(),
        });
        Ok(())
    }

    fn process_string_block(
        &mut self,
        iter: &mut Peekable<impl Iterator<Item = (SourceSpan, TokenBlock)>>,
    ) -> Result<String, SectionError> {
        match iter.next() {
            Some((_, TokenBlock::Block(tokens))) => self.process_string(tokens),
            _ => Err(SectionError::ExpectedText),
        }
    }

    fn process_string(
        &mut self,
        tokens: Vec<(SourceSpan, TokenBlock)>,
    ) -> Result<String, SectionError> {
        if tokens
            .iter()
            .any(|(_, tree)| !matches!(tree, TokenBlock::Char(_)))
        {
            return Err(SectionError::ExpectedText);
        }

        Ok(tokens.iter().map(|(_, tree)| tree.to_string()).collect())
    }
}
