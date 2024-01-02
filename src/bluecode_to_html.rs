use crate::bluecode::*;

pub fn bluecode_to_html(bluecode: &[Item]) -> String {
    bluecode
        .iter()
        .map(convert_item)
        .collect::<String>()
        .trim()
        .to_owned()
        + "\n"
}

fn convert_item(item: &Item) -> String {
    match item {
        Item::Header(header) => convert_header(header),
        Item::Paragraph(paragraph) => convert_paragraph(paragraph),
        Item::Theorem(theorem) => convert_theorem(theorem),
    }
}

fn convert_header(header: &Header) -> String {
    match header.level {
        0 => format!("<h1>{}</h1>", header.name),
        1 => format!("<h2>{}</h2>", header.name),
        2 => format!("<h3>{}</h3>", header.name),
        _ => todo!(),
    }
}

fn convert_paragraph(paragraph: &Paragraph) -> String {
    match paragraph {
        Paragraph::Text(spans) => format!(
            "<p>{}</p>\n",
            spans.iter().map(convert_span).collect::<String>()
        ),
        Paragraph::DisplayMath(math) => format!("<p>\\[ {} \\]</p>\n", math.content),
    }
}

fn convert_span(span: &Span) -> String {
    match span {
        Span::Text(text) => text.to_owned(),
        Span::Label(label) => format!("<i>{}</i>", label.name),
        Span::Reference(reference) => format!("<i>{}</i>", reference.name),
        Span::LeanLink(lean_link) => format!("<code>{}</code>", lean_link.lean_name),
        Span::Uses(_) => String::new(),
        Span::LeanOk => String::new(),
        Span::InlineMath(math) => format!("\\( {} \\)", math.content),
    }
}

fn convert_theorem(theorem: &Theorem) -> String {
    // Convert the theorem kind to title case.
    let name = theorem
        .kind
        .chars()
        .next()
        .unwrap()
        .to_uppercase()
        .to_string()
        + &theorem.kind.chars().skip(1).collect::<String>();

    format!("<b>{}.</b>\n", name)
        + &theorem
            .paragraphs
            .iter()
            .map(convert_paragraph)
            .collect::<String>()
}
