pub mod keyword;
pub mod punctuator;

use keyword::KeywordKind;

use self::punctuator::PunctuatorKind;

#[derive(Debug)]
pub enum ConstantKind {
    Number,
}

#[derive(Debug)]
pub enum TokenKind {
    Keyword(KeywordKind),
    Identifier,
    StringLiteral,
    Constant(ConstantKind),
    Punctuator(PunctuatorKind)
}

#[derive(Debug, Clone)]
pub struct Location {
    pub line: u8,
    pub column: u8,
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
