use crate::lexer::token::{ConstantKind, Token};

pub enum TranslationUnit {
    ExternalDeclaration(ExternalDeclaration),
    TranslationUnit(Box<TranslationUnit>, ExternalDeclaration),
}

pub enum ExternalDeclaration {
    FunctionDefinition(FunctionDefinition),
    Declaration(Declaration),
}

pub struct FunctionDefinition {
    declaration_specifiers: DeclarationSpecifiers,
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
    StructOrUnionSpecifier(Box<StructOrUnionSpecifier>),
    EnumSpecifier(EnumSpecifier),
    TypedefName(TypedefName),
}

pub enum StructOrUnionSpecifier {
    Declaration {
        struct_or_union: StructOrUnion,
        identifier: Option<Identifier>,
        struct_declaration_list: StructDeclarationList,
    },
    Reference {
        struct_or_union: StructOrUnion,
        identifier: Identifier,
    },
}

pub enum StructOrUnion {
    Struct,
    Union,
}

pub enum StructDeclarationList {
    Single(StructDeclaration),
    Multiple(Box<StructDeclarationList>, StructDeclaration),
}

pub struct StructDeclaration {
    specifier_qualifier_list: SpecifierQualifierList,
    struct_declarator_list: StructDeclaratorList,
}

pub enum SpecifierQualifierList {
    TypeSpecifier(TypeSpecifier, Option<Box<SpecifierQualifierList>>),
    TypeQualifier(TypeQualifier, Option<Box<SpecifierQualifierList>>),
}

pub enum TypeQualifier {
    Const,
    Restrict,
    Volatile,
}

pub enum StructDeclaratorList {
    Single(StructDeclarator),
    Multiple(Box<StructDeclaratorList>, StructDeclarator),
}

pub enum StructDeclarator {
    DeclaratorOnly(Declarator),
    DeclaratorWithConstantExpression(Option<Declarator>, ConstantExpression),
}

pub struct Declarator {
    pointer: Option<Pointer>,
    direct_declarator: Box<DirectDeclarator>,
}

pub enum Pointer {
    Simple(Option<TypeQualifierList>),
    Recursive(Option<TypeQualifierList>, Box<Pointer>),
}

pub enum TypeQualifierList {
    Single(TypeQualifier),
    Multiple(Box<TypeQualifierList>, TypeQualifier),
}

enum DirectDeclarator {
    Identifier(String),
    Declarator(Box<Declarator>),
    Array(
        Box<DirectDeclarator>,
        Option<TypeQualifierList>,
        Option<AssignmentExpression>,
    ),
    ArrayWithStatic(
        Box<DirectDeclarator>,
        Option<TypeQualifierList>,
        AssignmentExpression,
    ),
    ArrayWithStaticAfterTypeQualifiers(
        Box<DirectDeclarator>,
        TypeQualifierList,
        AssignmentExpression,
    ),
    ArrayWithAsterisk(Box<DirectDeclarator>, Option<TypeQualifierList>),
    Function(Box<DirectDeclarator>, ParameterTypeList),
    FunctionWithIdentifiers(Box<DirectDeclarator>, Option<IdentifierList>),
}

pub enum AssignmentExpression {
    ConditionalExpression(ConditionalExpression),
    UnaryAssignment {
        unary_expression: Box<UnaryExpression>,
        operator: AssignmentOperator,
        rhs: Box<AssignmentExpression>,
    },
}

pub enum ConditionalExpression {
    LogicalOrExpression(LogicalOrExpression),
    TernaryExpression {
        condition: Box<LogicalOrExpression>,
        true_expr: Box<Expression>,
        false_expr: Box<ConditionalExpression>,
    },
}

pub enum LogicalOrExpression {
    Simple(Box<LogicalAndExpression>),
    Or(Box<LogicalOrExpression>, Box<LogicalAndExpression>),
}

pub enum LogicalAndExpression {
    Simple(LogicalOrExpression),
    And(Box<LogicalAndExpression>, LogicalOrExpression),
}

pub enum InclusiveOrExpression {
    Simple(ExclusiveOrExpression),
    And(Box<InclusiveOrExpression>, ExclusiveOrExpression),
}

pub enum ExclusiveOrExpression {
    Simple(AndExpression),
    And(Box<ExclusiveOrExpression>, AndExpression),
}

pub enum AndExpression {
    Simple(EqualityExpression),
    And(Box<AndExpression>, EqualityExpression),
}

pub enum EqualityExpression {
    Simple(RelationalExpression),
    Equal(Box<EqualityExpression>, RelationalExpression),
    NotEqual(Box<EqualityExpression>, RelationalExpression),
}

pub enum RelationalExpression {
    Simple(ShiftExpression),
    LessThan(Box<RelationalExpression>, ShiftExpression),
    GreaterThan(Box<RelationalExpression>, ShiftExpression),
    LessThanOrEqual(Box<RelationalExpression>, ShiftExpression),
    GreaterThanOrEqual(Box<RelationalExpression>, ShiftExpression),
}

pub enum ShiftExpression {
    Simple(AdditiveExpression),
    LeftShift(Box<ShiftExpression>, AdditiveExpression),
    RightShift(Box<ShiftExpression>, AdditiveExpression),
}

pub enum AdditiveExpression {
    Simple(MultiplicativeExpression),
    Plus(Box<AdditiveExpression>, MultiplicativeExpression),
    Minus(Box<AdditiveExpression>, MultiplicativeExpression),
}

pub enum MultiplicativeExpression {
    Simple(CastExpression),
    Multiply(Box<MultiplicativeExpression>, CastExpression),
    Divide(Box<MultiplicativeExpression>, CastExpression),
    Modulo(Box<MultiplicativeExpression>, CastExpression),
}

pub enum CastExpression {
    Simple(Box<UnaryExpression>),
    Cast {
        type_name: TypeName,
        cast_expression: Box<CastExpression>,
    },
}

pub enum UnaryExpression {
    PostfixExpression(PostfixExpression),
    Increment(Box<UnaryExpression>),
    Decrement(Box<UnaryExpression>),
    UnaryOperator(UnaryOperator, CastExpression),
    SizeOfUnaryExpression(Box<UnaryExpression>),
    SizeOfTypeName(TypeName),
}
pub enum PostfixExpression {
    PrimaryExpression(PrimaryExpression),
    Array(Box<PostfixExpression>, Expression),
    FunctionCall(Box<PostfixExpression>, Option<ArgumentExpressionList>),
    Member(Box<PostfixExpression>, Identifier),
    PointerMember(Box<PostfixExpression>, Identifier),
    Increment(Box<PostfixExpression>),
    Decrement(Box<PostfixExpression>),
    Cast(Box<TypeName>, InitializerList),
}

pub enum PrimaryExpression {
    Identifier(Identifier),
    Constant(Constant),
    StringLiteral(StringLiteral),
    Expression(Box<Expression>),
}

pub enum Expression {
    AssignmentExpression(AssignmentExpression),
    Multiple(Box<Expression>, AssignmentExpression),
}

pub enum ArgumentExpressionList {
    AssignmentExpression(AssignmentExpression),
    Multiple(Box<ArgumentExpressionList>, AssignmentExpression),
}

pub struct TypeName {
    specifier_qualifier_list: SpecifierQualifierList,
    abstract_declarator: Option<AbstractDeclarator>,
}

pub enum AbstractDeclarator {
    Pointer(Option<Pointer>),
    PointerWithDirectDeclarator(Option<Pointer>, Box<DirectAbstractDeclarator>),
}

pub enum DirectAbstractDeclarator {
    Parenthesis(Box<AbstractDeclarator>),
    Array(
        Box<DirectAbstractDeclarator>,
        Option<TypeQualifierList>,
        Option<AssignmentExpression>,
    ),
    ArrayWithStatic(
        Box<DirectAbstractDeclarator>,
        Option<TypeQualifierList>,
        AssignmentExpression,
    ),
    ArrayWithStaticAfterTypeQualifiers(
        Box<DirectAbstractDeclarator>,
        TypeQualifierList,
        AssignmentExpression,
    ),
    ArrayWithAsterisk(Box<DirectAbstractDeclarator>, Option<TypeQualifierList>),
    Function(Box<DirectAbstractDeclarator>, ParameterTypeList),
}

pub enum ParameterTypeList {
    ParameterList(ParameterList),
    ParameterListWithEllipsis(ParameterList),
}

pub enum ParameterList {
    Single(ParameterDeclaration),
    Multiple(Box<ParameterList>, ParameterDeclaration),
}

pub enum ParameterDeclaration {
    DeclarationSpecifiers(DeclarationSpecifiers, Declarator),
    DeclarationSpecifiersWithAbstractDeclarator(DeclarationSpecifiers, Option<AbstractDeclarator>),
}

enum InitializerList {
    Single(Option<Designation>, Box<Initializer>),
    Multiple(Box<InitializerList>, Option<Designation>, Box<Initializer>),
}

pub enum Designation {
    DesignatorList(DesignatorList),
}

pub enum DesignatorList {
    Single(Designator),
    Multiple(Box<DesignatorList>, Designator),
}

pub enum Designator {
    Array(ConstantExpression),
    Member(Identifier),
}

pub enum ConstantExpression {
    ConditionalExpression(ConditionalExpression),
}

pub enum Initializer {
    AssignmentExpression(AssignmentExpression),
    InitializerList(InitializerList),
    InitializerListWithComma(InitializerList),
}
pub enum UnaryOperator {
    And,
    Multiply,
    Plus,
    Minus,
    BitwiseNot,
    LogicalNot,
}

#[derive(Debug)]
pub enum AssignmentOperator {
    Assign,
    MultiplyAssign,
    DivideAssign,
    ModuloAssign,
    PlusAssign,
    MinusAssign,
    LeftShiftAssign,
    RightShiftAssign,
    BitwiseAndAssign,
    BitwiseXorAssign,
    BitwiseOrAssign,
}

pub enum IdentifierList {
    Single(Identifier),
    Multiple(Box<IdentifierList>, Identifier),
}

pub enum EnumSpecifier {
    Declaration {
        identifier: Option<Identifier>,
        enumerator_list: EnumeratorList,
    },
    Reference {
        identifier: Identifier,
    },
}

pub enum EnumeratorList {
    Single(Enumerator),
    Multiple(Box<EnumeratorList>, Enumerator),
}

pub enum Enumerator {
    Constant(EnumerationConstant),
    ConstantWithExpression(EnumerationConstant, ConstantExpression),
}

pub struct EnumerationConstant {
    identifier: Identifier,
}

pub enum TypedefName {
    Identifier(Identifier),
}

pub enum FunctionSpecifier {
    Inline,
}

pub enum DeclarationList {
    Single(Declaration),
    Multiple(Box<DeclarationList>, Declaration),
}

pub struct Declaration {
    declaration_specifiers: DeclarationSpecifiers,
    init_declarator_list: Option<InitDeclaratorList>,
}

pub enum InitDeclaratorList {
    Single(InitDeclarator),
    Multiple(Box<InitDeclaratorList>, InitDeclarator),
}

pub enum InitDeclarator {
    Declarator(Declarator),
    DeclaratorWithInitializer(Declarator, Initializer),
}

pub struct CompoundStatement {
    block_item_list: Option<BlockItemList>,
}

pub enum BlockItemList {
    Single(BlockItem),
    Multiple(Box<BlockItemList>, BlockItem),
}

pub enum BlockItem {
    Declaration(Declaration),
    Statement(Statement),
}

pub enum Statement {
    Labeled(Box<LabeledStatement>),
    Compound(Box<CompoundStatement>),
    Expression(Box<ExpressionStatement>),
    Selection(Box<SelectionStatement>),
    Iteration(Box<IterationStatement>),
    Jump(Box<JumpStatement>),
}

pub enum LabeledStatement {
    Identifier(Identifier, Statement),
    Case(ConstantExpression, Statement),
    Default(Statement),
}

pub enum ExpressionStatement {
    Expression(Option<Expression>),
}

pub enum SelectionStatement {
    If(Expression, Box<Statement>),
    IfElse(Expression, Box<Statement>, Box<Statement>),
    Switch(Expression, Box<Statement>),
}

pub enum IterationStatement {
    While(Expression, Box<Statement>),
    DoWhile(Box<Statement>, Expression),
    For(
        Option<Expression>,
        Option<Expression>,
        Option<Expression>,
        Box<Statement>,
    ),
    ForWithDeclaration(
        Declaration,
        Option<Expression>,
        Option<Expression>,
        Box<Statement>,
    ),
}

pub enum JumpStatement {
    Goto(Identifier),
    Continue,
    Break,
    Return(Option<Expression>),
}

pub struct Identifier {
    pub name: String,
}

pub struct Constant {
    pub kind: ConstantKind,
    pub value: String,
}

pub struct StringLiteral {
    pub value: String,
}
