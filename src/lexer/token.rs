pub mod keyword;
pub mod punctuator;

use std::fmt::{Display, Formatter};

use keyword::KeywordKind;

use self::punctuator::PunctuatorKind;

#[derive(Debug, Clone, PartialEq)]
pub enum ConstantKind {
    Number,
    Char,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Keyword(KeywordKind),
    Identifier,
    StringLiteral,
    Constant(ConstantKind),
    Punctuator(PunctuatorKind),
}

/// Represents a location in a source string. Note that
/// the values are intended to be used as 1-based, i.e.
/// as they would appear in your editor.
#[derive(Debug, Clone)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}

impl Display for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub location: Location,
    pub lexeme: String,
    pub kind: TokenKind,
}

impl Token {
    pub fn new(kind: TokenKind, lexeme: String, location: Location) -> Self {
        Self {
            kind,
            lexeme,
            location,
        }
    }
}
