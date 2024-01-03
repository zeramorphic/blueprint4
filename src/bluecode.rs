//! A format for blueprint code, independent of the source material (e.g. LaTeX/Markdown).

use std::collections::VecDeque;

#[derive(Debug)]
pub enum Item {
    Section(Section),
    Paragraph(Paragraph),
    Theorem(Theorem),
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

#[derive(Debug)]
pub enum Paragraph {
    Text(Span),
    DisplayMath(Mathematics),
}

#[derive(Debug)]
pub enum Span {
    Concatenate(Vec<Span>),
    Text(String),
    Reference(Reference),
    InlineMath(Mathematics),
}

impl Span {
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
    /// The kind of theorem.
    pub kind: String,
    pub label: Option<String>,
    pub lean_name: Option<String>,
    pub uses: Vec<String>,
    pub proven: bool,
    /// The contents of the theorem.
    pub contents: Vec<Paragraph>,
}
