use std::io::Write;

use askama::Template;
use quick_xml::{
    events::{BytesText, Event},
    Error, Reader, Writer,
};

use crate::bluecode::*;

pub fn bluecode_to_html(document: &Section, writer: impl Write) -> quick_xml::Result<()> {
    let mut writer = Writer::new_with_indent(writer, b' ', 2);

    writer.write_event(Event::DocType(BytesText::new("html")))?;
    writer
        .create_element("html")
        .write_inner_content::<_, Error>(|writer| {
            writer
                .create_element("head")
                .write_inner_content(|writer| write_head(writer))?;

            writer
                .create_element("body")
                .write_inner_content(|writer| write_body(document, writer))?;

            Ok(())
        })?;

    Ok(())
}

#[derive(Template)]
#[template(path = "head.html")]
struct HeadTemplate {
    title: String,
}

fn write_head(writer: &mut Writer<impl Write>) -> Result<(), Error> {
    let head_template = HeadTemplate {
        title: "LeanAPAP".to_owned(),
    };
    let head = head_template.render().unwrap();

    let mut reader = Reader::from_str(&head);
    reader.trim_text(true);

    loop {
        match reader.read_event() {
            Ok(Event::Eof) => break,
            Ok(e) => writer.write_event(e)?,
            Err(err) => panic!("error at position {}: {:?}", reader.buffer_position(), err),
        }
    }

    writer
        .create_element("style")
        .write_text_content(BytesText::from_escaped(include_str!(
            "../templates/normalize.css"
        )))?;

    writer
        .create_element("style")
        .write_text_content(BytesText::from_escaped(include_str!(
            "../templates/styles.css"
        )))?;

    Ok(())
}

fn write_body(document: &Section, writer: &mut Writer<impl Write>) -> Result<(), Error> {
    write_section_body(document, writer)
}

fn write_item(item: &Item, writer: &mut Writer<impl Write>) -> Result<(), Error> {
    match item {
        Item::Section(section) => write_section(section, writer),
        Item::Paragraph(paragraph) => write_paragraph(paragraph, writer),
        Item::Theorem(theorem) => write_theorem(theorem, writer),
    }
}

fn write_section(section: &Section, writer: &mut Writer<impl Write>) -> Result<(), Error> {
    write_section_header(section, writer)?;
    write_section_body(section, writer)?;
    Ok(())
}

fn write_section_header(section: &Section, writer: &mut Writer<impl Write>) -> Result<(), Error> {
    writer
        .create_element("h1")
        .write_text_content(BytesText::new(&section.name))?;
    Ok(())
}

fn write_section_body(section: &Section, writer: &mut Writer<impl Write>) -> Result<(), Error> {
    for item in &section.contents {
        write_item(item, writer)?;
    }
    Ok(())
}

fn write_theorem(theorem: &Theorem, writer: &mut Writer<impl Write>) -> Result<(), Error> {
    writer
        .create_element("div")
        .with_attribute(("class", "theorem"))
        .write_inner_content::<_, Error>(|writer| {
            writer
                .create_element("div")
                .with_attribute(("class", "theorem-heading"))
                .write_inner_content::<_, Error>(|writer| {
                    writer
                        .create_element("span")
                        .with_attribute(("class", "theorem-kind"))
                        .write_text_content(BytesText::new(&theorem.display_name_upper))?;
                    if let Some(name) = &theorem.name {
                        writer
                            .create_element("span")
                            .with_attribute(("class", "theorem-name"))
                            .write_inner_content::<_, Error>(|writer| {
                                writer.write_event(Event::Text(BytesText::new(" (")))?;
                                write_span(name, writer)?;
                                writer.write_event(Event::Text(BytesText::new(")")))?;
                                Ok(())
                            })?;
                    }
                    Ok(())
                })?;

            writer
                .create_element("div")
                .with_attribute(("class", "theorem-content"))
                .write_inner_content::<_, Error>(|writer| {
                    for paragraph in &theorem.contents {
                        write_paragraph(paragraph, writer)?;
                    }
                    Ok(())
                })?;
            Ok(())
        })?;
    Ok(())
}

fn write_paragraph(paragraph: &Paragraph, writer: &mut Writer<impl Write>) -> Result<(), Error> {
    match paragraph {
        Paragraph::Text(span) => writer
            .create_element("p")
            .write_inner_content(|writer| write_span(span, writer))
            .map(|_| ()),
        Paragraph::DisplayMath(math) => writer
            .create_element("div")
            .with_attribute(("class", "display-math"))
            .write_inner_content(|writer| {
                writer.write_event(Event::Text(BytesText::new(&format!(
                    "\\[{}\\]",
                    math.content
                ))))
            })
            .map(|_| ()),
    }
}

fn write_span(span: &Span, writer: &mut Writer<impl Write>) -> Result<(), Error> {
    match span {
        Span::Concatenate(spans) => {
            for span in spans {
                write_span(span, writer)?;
            }
            Ok(())
        }
        Span::Text(text) => writer.write_event(Event::Text(BytesText::new(text))),
        Span::Reference(reference) => writer
            .create_element("i")
            .write_text_content(BytesText::new(&reference.name))
            .map(|_| ()),
        Span::InlineMath(math) => writer.write_event(Event::Text(BytesText::new(&format!(
            "\\({}\\)",
            math.content
        )))),
    }
}
