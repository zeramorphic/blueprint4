//! A format for blueprint code, independent of the source material (e.g. LaTeX/Markdown).

#[derive(Debug)]
pub enum Item {
    Header(Header),
    Paragraph(Paragraph),
    Theorem(Theorem),
}

#[derive(Debug)]
pub struct Header {
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
}

#[derive(Debug)]
pub enum Paragraph {
    Text(Vec<Span>),
    DisplayMath(Mathematics),
}

#[derive(Debug)]
pub enum Span {
    Text(String),
    Label(Label),
    Reference(Reference),
    LeanLink(LeanLink),
    Uses(Uses),
    LeanOk,
    InlineMath(Mathematics),
}

#[derive(Debug)]
pub struct Label {
    pub name: String,
}

#[derive(Debug)]
pub struct Reference {
    pub name: String,
}

#[derive(Debug)]
pub struct LeanLink {
    pub lean_name: String,
}

#[derive(Debug)]
pub struct Uses {
    pub uses: Vec<String>,
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
    /// The contents of the theorem.
    pub paragraphs: Vec<Paragraph>,
}
