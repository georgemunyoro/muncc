use std::fmt::Display;


#[derive(Debug, Clone)]
pub enum TokenizationErrorKind {
    UnexpectedToken(String),
    ExpectedToken(String),
}

#[derive(Debug, Clone)]
pub struct TokenizationError {
    pub kind: TokenizationErrorKind
}

impl Display for TokenizationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            TokenizationErrorKind::UnexpectedToken(t) => {
                return write!(f, "Unexpected token: {t}");
            }
            TokenizationErrorKind::ExpectedToken(t) => {
                return write!(f, "Expected token: {t}");
            }
        }
    }
}

