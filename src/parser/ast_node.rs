use crate::lexer::token::Token;

// pub enum AstIdentifier {
//     token: Token,
// }
// 
// impl AstIdentifier {
//     pub fn new(token: Token) -> Self {
//         Self { token }
//     }
// }

pub enum TranslationUnit {
    ExternalDeclaration(ExternalDeclaration),
    TranslationUnit(Box<TranslationUnit>, ExternalDeclaration),
}

pub enum ExternalDeclaration {
    FunctionDefinition(FunctionDefinition),
    Declaration(Declaration),
}

pub struct FunctionDefinition {
    declaration_specifiers: DeclarationSpecifier,
    declarator: Declarator,
    declaration_list: Option<DeclarationList>,
    compound_statement: CompoundStatement,
}

pub enum DeclarationSpecifiers {
    StorageClassSpecifier(StorageClassSpecifier, Option<Box<DeclarationSpecifiers>>),
    TypeSpecifier(TypeSpecifier, Option<Box<DeclarationSpecifiers>>),
    TypeQualifier(TypeQualifier, Option<Box<DeclarationSpecifiers>>),
    FunctionSpecifier(FunctionSpecifier, Option<Box<DeclarationSpecifiers>>),
}

pub enum StorageClassSpecifier {
    TypeDef,
    Extern,
    Static,
    Auto,
    Register,
}

pub enum TypeSpecifier {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    Unsigned,
    Bool,
    Complex,
    StructOrUnionSpecifier(StructOrUnionSpecifier),
    EnumSpecifier(EnumSpecifier),
    TypedefName(TypedefName),
}

pub enum StructOrUnionSpecifier {
    Declaration {
        struct_or_union: StructOrUnion,
        identifier: Option<Identifier>,
        struct_declaration_list: StructDeclarationList
    },
    Reference {
        struct_or_union: StructOrUnion,
        identifier: Identifier
    }
}

pub enum StructOrUnion {
    Struct,
    Union,
}

pub enum StructDeclarationList {
    StructDeclaration(StructDeclaration),
    StructDeclarationList(StructDeclarationList, StructDeclaration)
}

pub struct StructDeclaration {
    specifier_qualifier_list: SpecifierQualifierList,
    struct_declarator_list: StructDeclaratorList
}

pub enum SpecifierQualifierList {
    TypeSpecifier(TypeSpecifier, Option<Box<SpecifierQualifierList>>),
    TypeQualifier(TypeQualifier, Option<Box<SpecifierQualifierList>>)
}

pub enum TypeQualifier {
    Const,
    Restrict,
    Volatile
}

pub enum StructDeclaratorList {
    StructDeclarator(StructDeclarator),
    StructDeclaratorList(StructDeclaratorList, StructDeclarator)
}

pub enum StructDeclarator {
    DeclaratorOnly(Declarator),
    DeclaratorWithConstantExpr(Option<Declarator>, ConstantExpr)
}

pub struct Declarator {
    pointer: Option<Pointer>,
    direct_declarator: DirectDeclarator
}

pub enum Pointer {
    Simple(Option<TypeQualifierList>),
    Recursive(Option<TypeQualifierList>, Box<Pointer>),
}

pub enum TypeQualifierList {
    TypeQualifier(TypeQualifier),
    TypeQualifierList(Box<TypeQualifierList>, TypeQualifier)
}

