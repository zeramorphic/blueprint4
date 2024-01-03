use std::iter::Peekable;

use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

use crate::{bluecode::*, latex_sections::TokenSection, latex_token_blocks::TokenBlock};

pub fn latex_to_bluecode(
    src: &str,
    src_name: &str,
    section: TokenSection,
) -> Result<Section, Vec<ConvertError>> {
    todo!()
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
