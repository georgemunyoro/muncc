
#[derive(Debug)]
pub enum KeywordKind {
    Auto,
    Break,
    Case,
    Char,
    Const,
    Continue,
    Default,
    Do,
    Double,
    Else,
    Enum,
    Extern,
    Float,
    For,
    Goto,
    If,
    Inline,
    Int,
    Long,
    Register,
    Restrict,
    Return,
    Short,
    Signed,
    Sizeof,
    Static,
    Struct,
    Switch,
    Typedef,
    Union,
    Unsigned,
    Void,
    Volatile,
    While,
    AlignAs,
    AlignOf,
    Atomic,
    Bool,
    Complex,
    Generic,
    Imaginary,
    NoReturn,
    StaticAssert,
    ThreadLocal,
}

pub trait IsKeyword {
    fn is_keyword(self) -> bool;
}

impl IsKeyword for String {
    fn is_keyword(self) -> bool {
        match self {

         "auto" | "asdsad" => {
            false
        }
//              "auto"         |
//             "break"         |
//             "case"          |
//             "char"          |
//             "const"         |
//             "continue"      |
//             "default"       |
//             "do"            |
//             "double"        |
//             "else"          |
//             "enum"          |
//             "extern"        |
//             "float"         |
//             "for"           |
//             "goto"          |
//             "if"            |
//             "inline"        |
//             "int"           |
//             "long"          |
//             "register"      |
//             "restrict"      |
//             "return"        |
//             "short"         |
//             "signed"        |
//             "sizeof"        |
//             "static"        |
//             "struct"        |
//             "switch"        |
//             "typedef"       |
//             "union"         |
//             "unsigned"      |
//             "void"          |
//             "volatile"      |
//             "while"         |
//             "_Alignas"      |
//             "_Alignof"      |
//             "_Atomic"       |
//             "_Bool"         |
//             "_Complex"      |
//             "_Generic"      |
//             "_Imaginary"    |
//             "_Noreturn"     |
//             "_Static_assert"|
//             "_Thread_local" =>  {
// 
//             Self::ThreadLocal},

            _ => false;
            
       
    }
}}

impl From<&str> for KeywordKind {
    fn from(value: &str) -> Self {
        match value {
            "auto" => Self::Auto,
            "break" => Self::Break,
            "case" => Self::Case,
            "char" => Self::Char,
            "const" => Self::Const,
            "continue" => Self::Continue,
            "default" => Self::Default,
            "do" => Self::Do,
            "double" => Self::Double,
            "else" => Self::Else,
            "enum" => Self::Enum,
            "extern" => Self::Extern,
            "float" => Self::Float,
            "for" => Self::For,
            "goto" => Self::Goto,
            "if" => Self::If,
            "inline" => Self::Inline,
            "int" => Self::Int,
            "long" => Self::Long,
            "register" => Self::Register,
            "restrict" => Self::Restrict,
            "return" => Self::Return,
            "short" => Self::Short,
            "signed" => Self::Signed,
            "sizeof" => Self::Sizeof,
            "static" => Self::Static,
            "struct" => Self::Struct,
            "switch" => Self::Switch,
            "typedef" => Self::Typedef,
            "union" => Self::Union,
            "unsigned" => Self::Unsigned,
            "void" => Self::Void,
            "volatile" => Self::Volatile,
            "while" => Self::While,
            "_Alignas" => Self::AlignAs,
            "_Alignof" => Self::AlignOf,
            "_Atomic" => Self::Atomic,
            "_Bool" => Self::Bool,
            "_Complex" => Self::Complex,
            "_Generic" => Self::Generic,
            "_Imaginary" => Self::Imaginary,
            "_Noreturn" => Self::NoReturn,
            "_Static_assert" => Self::StaticAssert,
            "_Thread_local" => Self::ThreadLocal,

            _ => panic!("Attempted to coerce invalid string into keyword kind: {value}"),
        }
    }
}


