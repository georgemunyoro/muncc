use std::{fmt::Display, sync::Arc};

use super::token::Location;

#[derive(Debug, Clone)]
pub enum TokenizationErrorKind {
    UnexpectedToken(String),
    ExpectedToken(String),
    UnterminatedString,
    UnterminatedChar
}

#[derive(Debug, Clone)]
pub struct TokenizationError {
    pub kind: TokenizationErrorKind,
    pub source: Arc<String>,
    pub location: Location,
}

impl Display for TokenizationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(&offending_line) = self
            .source
            .lines()
            .collect::<Vec<&str>>()
            .get(self.location.line - 1)
        {
            let formatted_offending_line =
                format!(" {:8} | {}", self.location.line, offending_line);

            let error_message = match &self.kind {
                TokenizationErrorKind::UnterminatedString => "unterminated string",
                TokenizationErrorKind::UnterminatedChar => "unterminated char",
                TokenizationErrorKind::UnexpectedToken(_) => "unexpected token",
                TokenizationErrorKind::ExpectedToken(_) => "expected token",
            };

            let formatted_error = formatted_offending_line.clone()
                + "\n"
                + &format!(
                    "{:>left_pad$}^ {}",
                    " ",
                    error_message,
                    left_pad = 11 + self.location.column
                )
                .to_owned();

            return write!(f, "{}\n", formatted_error);
        } else {
            panic!("ERR: Unable to get offending source line!");
        }
    }
}
