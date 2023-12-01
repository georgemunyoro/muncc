pub mod keyword;
pub mod punctuator;

use keyword::KeywordKind;

use self::punctuator::PunctuatorKind;

#[derive(Debug)]
pub enum ConstantKind {
    Number,
    Char,
}

#[derive(Debug)]
pub enum TokenKind {
    Keyword(KeywordKind),
    Identifier,
    StringLiteral,
    Constant(ConstantKind),
    Punctuator(PunctuatorKind)
}


/// Represents a location in a source string. Note that
/// the values are intended to be used as 1-based, i.e.
/// as they would appear in your editor.
#[derive(Debug, Clone)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug)]
pub struct Token {
    location: Location,
    lexeme: String,
    kind: TokenKind,
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
