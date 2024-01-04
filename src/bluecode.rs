//! A format for blueprint code, independent of the source material (e.g. LaTeX/Markdown).

use std::collections::VecDeque;

#[derive(Debug)]
pub enum Item {
    Section(Section),
    Paragraph(Paragraph),
    Theorem(Theorem),
}

impl Item {
    pub fn split_newlines(self) -> Vec<Item> {
        match self {
            Item::Section(section) => vec![Item::Section(section.split_newlines())],
            Item::Paragraph(paragraph) => Paragraph::split_newlines(vec![paragraph])
                .into_iter()
                .map(Item::Paragraph)
                .collect(),
            Item::Theorem(theorem) => vec![Item::Theorem(theorem.split_newlines())],
        }
    }
}

#[derive(Debug)]
pub struct Section {
    pub name: String,
    /// The header level. In LaTeX, these are:
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
    pub label: Option<String>,
    pub contents: Vec<Item>,
}

impl Section {
    pub fn split_newlines(self) -> Self {
        Self {
            contents: self
                .contents
                .into_iter()
                .flat_map(|item| item.split_newlines())
                .collect(),
            ..self
        }
    }
}

#[derive(Debug)]
pub enum Paragraph {
    Text(Span),
    DisplayMath(Mathematics),
}

impl Paragraph {
    /// Merge adjacent text spans, delete empty text spans, and split newlines.
    pub fn split_newlines(paragraphs: Vec<Paragraph>) -> Vec<Paragraph> {
        // First, split newlines.
        let paragraphs = paragraphs
            .into_iter()
            .flat_map(|paragraph| match paragraph {
                Paragraph::Text(span) => span
                    .split_newlines()
                    .into_iter()
                    .map(Paragraph::Text)
                    .collect(),
                _ => vec![paragraph],
            });

        // Then, merge together any consecutive nonempty text paragraphs.
        let mut output = Vec::new();
        for paragraph in paragraphs {
            if let Paragraph::Text(next_text) = paragraph {
                if next_text.is_empty() {
                    output.push(Paragraph::Text(next_text));
                } else {
                    match output.pop() {
                        Some(Paragraph::Text(previous_text)) => output.push(Paragraph::Text(
                            Span::Concatenate(vec![
                                previous_text,
                                Span::Text(" ".to_owned()),
                                next_text,
                            ])
                            .normalise(),
                        )),
                        Some(other) => {
                            output.push(other);
                            output.push(Paragraph::Text(next_text))
                        }
                        None => output.push(Paragraph::Text(next_text)),
                    }
                }
            } else {
                output.push(paragraph);
            }
        }

        // Finally, reduce spacing and then delete empty text paragraphs.
        output
            .into_iter()
            .map(|paragraph| match paragraph {
                Paragraph::Text(span) => Paragraph::Text(span.reduce_spacing().trim()),
                _ => paragraph,
            })
            .filter(|paragraph| match paragraph {
                Paragraph::Text(span) => !span.is_empty(),
                _ => true,
            })
            .collect()
    }
}

#[derive(Debug)]
pub enum Span {
    Concatenate(Vec<Span>),
    Text(String),
    Reference(Reference),
    InlineMath(Mathematics),
}

impl Span {
    pub fn is_empty(&self) -> bool {
        match self {
            Span::Concatenate(spans) => spans.iter().all(|span| span.is_empty()),
            Span::Text(text) => text.is_empty(),
            _ => false,
        }
    }

    /// Normalises consecutive `Text` spans and nested `Concatenate` spans.
    /// After calling `normalise`, this span contains at most one `Concatenate`.
    pub fn normalise(self) -> Span {
        match self {
            Span::Concatenate(spans) => {
                let mut spans = VecDeque::from(spans);
                let mut output = Vec::<Span>::new();

                while let Some(span) = spans.pop_front() {
                    match span {
                        Span::Concatenate(inner_spans) => {
                            // Flatten inner concatenations.
                            for span in inner_spans.into_iter().rev() {
                                spans.push_front(span);
                            }
                        }
                        Span::Text(text) => {
                            if let Some(Span::Text(previous_text)) = output.last_mut() {
                                // Merge consecutive text spans.
                                *previous_text += &text;
                            } else {
                                output.push(Span::Text(text));
                            }
                        }
                        _ => output.push(span),
                    }
                }

                if output.len() == 1 {
                    output.pop().unwrap()
                } else {
                    Span::Concatenate(output)
                }
            }
            _ => self,
        }
    }

    /// Splits any `Text` spans by newline characters.
    pub fn split_newlines(self) -> Vec<Span> {
        let span = self.normalise();
        match span {
            Span::Concatenate(spans) => {
                let mut result = Vec::<Span>::new();
                for span in spans {
                    let mut split_spans = span.split_newlines();
                    match result.pop() {
                        Some(last) => {
                            result.push(Span::Concatenate(vec![last, split_spans.remove(0)]));
                            result.extend(split_spans);
                        }
                        None => {
                            result.extend(split_spans);
                        }
                    }
                }
                result.into_iter().map(|span| span.normalise()).collect()
            }
            Span::Text(text) => text
                .lines()
                .map(|line| Span::Text(line.to_owned()))
                .collect(),
            _ => vec![span],
        }
    }

    /// Converts any sequence of whitespace characters to a single space.
    pub fn reduce_spacing(self) -> Self {
        match self {
            Span::Concatenate(spans) => Span::Concatenate(
                spans
                    .into_iter()
                    .map(|span| span.reduce_spacing())
                    .collect(),
            ),
            Span::Text(text) => {
                let mut result = String::new();
                let mut previous_whitespace = false;
                for char in text.chars() {
                    if char.is_whitespace() {
                        if !previous_whitespace {
                            previous_whitespace = true;
                            result.push(' ');
                        }
                    } else {
                        previous_whitespace = false;
                        result.push(char);
                    }
                }
                Span::Text(result)
            }
            _ => self,
        }
    }

    /// If this span starts or ends with whitespace, remove it.
    /// This span is assumed to be normalised.
    pub fn trim(self) -> Self {
        match self {
            Span::Concatenate(mut spans) => {
                let last = spans.pop().unwrap();
                let mut spans = spans.into_iter();
                let first = spans.next().unwrap();
                Span::Concatenate(
                    std::iter::once(match first {
                        Span::Text(text) => Span::Text(text.trim_start().to_owned()),
                        _ => first,
                    })
                    .chain(spans)
                    .chain(std::iter::once(match last {
                        Span::Text(text) => Span::Text(text.trim_end().to_owned()),
                        _ => last,
                    }))
                    .collect(),
                )
            }
            Span::Text(text) => Span::Text(text.trim().to_owned()),
            _ => self,
        }
    }
}

#[derive(Debug)]
pub struct Reference {
    pub name: String,
}

#[derive(Debug)]
pub struct Mathematics {
    /// If this is true, the supplied mathematics is probably compatible with MathJax.
    /// If not, we'll need to render it to an SVG instead.
    pub mathjax_compatible: bool,
    pub content: String,
}

/// A definition, theorem, or proof.
#[derive(Debug)]
pub struct Theorem {
    pub display_name_upper: String,
    pub display_name_lower: String,
    pub label: Option<String>,
    pub lean_name: Option<String>,
    pub uses: Vec<String>,
    pub proven: bool,
    /// The contents of the theorem.
    pub contents: Vec<Paragraph>,
}

impl Theorem {
    pub fn split_newlines(self) -> Self {
        Self {
            contents: Paragraph::split_newlines(self.contents),
            ..self
        }
    }
}
