pub mod keyword;

use keyword::KeywordKind;

pub enum TokenKind {
    Keyword(KeywordKind),
    Identifier(String),
    StringLiteral(String),
    Constant(String),
}

pub struct Location {
    line: u8,
    column: u8,
}

pub struct Token {
    location: Location,
    lexeme: String,
    kind: TokenKind,
}
