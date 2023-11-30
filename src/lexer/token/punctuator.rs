#[derive(Debug, PartialEq, Clone)]
pub enum PunctuatorKind {
    LParen,
    RParen,
    Deref,
    LT,
    LE,
    EQ,
    GT,
    GE,
    NE,
    LBracket,
    RBracket,
    Assign,
    LBrace,
    RBrace,
    MulAssign,
    Dot,
    Comma,
    DivAssign,
    IncrOp,
    DecrOp,
    RemAssign,
    AddrBitand,
    MultStar,
    PlusAssign,
    Plus,
    Minus,
    MinusAssign,
    BitNot,
    LogNot,
    LShiftAssign,
    Div,
    Rem,
    RShiftAssign,
    LShift,
    RShift,
    BitAndAssign,
    BitOr,
    LogAnd,
    BitXorAssign,
    BitXor,
    LogOr,
    BitOrAssign,
    Query,
    Colon,
    Semicolon,
    Ellipsis,
    BitNotAssign,
}

static PUNCTUATORS: [&str; 47] = [
    "(", ")", "->", "<", "<=", "==", ">", ">=", "!=", "[", "]", "=", "{", "}", "*=", ".", ",",
    "/=", "++", "--", "%=", "&", "*", "+=", "+", "-", "-=", "~", "!", "<<=", "/", "%", ">>=", "<<",
    ">>", "&=", "^", "&&", "^=", "|", "||", "|=", "?", ":", ";", "...", "~=",
];

pub trait IsPunctuator {
    fn is_punctuator(self) -> bool;
}

impl IsPunctuator for String {
    fn is_punctuator(self) -> bool {
        PUNCTUATORS.contains(&self.as_str()) && !self.contains(" ")
    }
}

impl IsPunctuator for char {
    fn is_punctuator(self) -> bool {
        PUNCTUATORS.contains(&String::from(self).as_str())
    }
}

impl From<&str> for PunctuatorKind {
    fn from(value: &str) -> Self {
        match value {
            "(" => PunctuatorKind::LParen,
            ")" => PunctuatorKind::RParen,
            "->" => PunctuatorKind::Deref,
            "<" => PunctuatorKind::LT,
            "<=" => PunctuatorKind::LE,
            "==" => PunctuatorKind::EQ,
            ">" => PunctuatorKind::GT,
            ">=" => PunctuatorKind::GE,
            "!=" => PunctuatorKind::NE,
            "[" => PunctuatorKind::LBracket,
            "]" => PunctuatorKind::RBracket,
            "=" => PunctuatorKind::Assign,
            "{" => PunctuatorKind::LBrace,
            "}" => PunctuatorKind::RBrace,
            "*=" => PunctuatorKind::MulAssign,
            "." => PunctuatorKind::Dot,
            "," => PunctuatorKind::Comma,
            "/=" => PunctuatorKind::DivAssign,
            "++" => PunctuatorKind::IncrOp,
            "--" => PunctuatorKind::DecrOp,
            "%=" => PunctuatorKind::RemAssign,
            "&" => PunctuatorKind::AddrBitand,
            "*" => PunctuatorKind::MultStar,
            "+=" => PunctuatorKind::PlusAssign,
            "+" => PunctuatorKind::Plus,
            "-" => PunctuatorKind::Minus,
            "-=" => PunctuatorKind::MinusAssign,
            "~" => PunctuatorKind::BitNot,
            "!" => PunctuatorKind::LogNot,
            "<<=" => PunctuatorKind::LShiftAssign,
            "/" => PunctuatorKind::Div,
            "%" => PunctuatorKind::Rem,
            ">>=" => PunctuatorKind::RShiftAssign,
            "<<" => PunctuatorKind::LShift,
            ">>" => PunctuatorKind::RShift,
            "&=" => PunctuatorKind::BitAndAssign,
            "^" => PunctuatorKind::BitOr,
            "&&" => PunctuatorKind::LogAnd,
            "^=" => PunctuatorKind::BitXorAssign,
            "|" => PunctuatorKind::BitXor,
            "||" => PunctuatorKind::LogOr,
            "|=" => PunctuatorKind::BitOrAssign,
            "?" => PunctuatorKind::Query,
            ":" => PunctuatorKind::Colon,
            ";" => PunctuatorKind::Semicolon,
            "..." => PunctuatorKind::Ellipsis,
            "~=" => PunctuatorKind::BitNotAssign,

            // Panic if unknown, use `String::is_punctuator` before
            // calling this method.
            _ => panic!("Unknown punctuator '{value}'"),
        }
    }
}

impl From<String> for PunctuatorKind {
    fn from(value: String) -> Self {
        PunctuatorKind::from(value.as_str())
    }
}


