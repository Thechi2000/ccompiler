#![allow(unused)]

#[derive(Debug, Clone)]
pub enum PrimitiveType {
    Char,
    Int,
    Long,
    Float,
    Double,
    Void,
}
#[derive(Debug, Clone)]
pub enum TypeSpecifier {
    Long,
    Short,
    Unsigned,
    Signed,
}
#[derive(Debug, Clone)]
pub enum TypeQualifier {
    Const,
    Restrict,
    Volatile,
}

#[derive(Debug, Clone)]
pub struct Type {
    pub primitive: PrimitiveType,
    pub specifiers: Vec<TypeSpecifier>,
    pub qualifiers: Vec<TypeQualifier>,
}

#[derive(Debug, Clone)]
pub enum PreUnOp {
    Minus,
    BNot,
    LNot,
    Ref,
    Deref,
    Plus,
    Incr,
    Decr,
}

#[derive(Debug, Clone)]
pub enum PostUnOp {
    Incr,
    Decr,
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    ShiftLeft,
    ShiftRight,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
    Equal,
    Different,
    BAnd,
    BOr,
    Xor,
    LAnd,
    LOr,
    Access,
    DerefAccess,
    Assign,
}

#[derive(Debug, Clone)]
pub enum Litteral {
    String(String),
    Integer(i128),
}

#[derive(Debug, Clone)]
pub enum Expr {
    BinaryOperation {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        op: BinOp,
    },
    PreUnaryOperation {
        hs: Box<Expr>,
        op: PreUnOp,
    },
    PostUnaryOperation {
        hs: Box<Expr>,
        op: PostUnOp,
    },
    Identifier(String),
    Litteral(Litteral),
    FunctionCall {
        name: String,
        parameters: Vec<Expr>,
    },
}

pub type Identifier = String;

#[derive(Debug, Clone)]
pub struct IfElseStruct {
    pub condition: Expr,
    pub true_case: Box<Statement>,
    pub false_case: Option<Box<Statement>>,
}
#[derive(Debug, Clone)]
pub struct WhileStruct {
    pub condition: Expr,
    pub body: Box<Statement>,
}
#[derive(Debug, Clone)]
pub struct DoWhileStruct {
    pub condition: Expr,
    pub body: Box<Statement>,
}
#[derive(Debug, Clone)]
pub struct ReturnStruct {
    pub value: Option<Expr>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Declaration {
        type_: Type,
        variables: Vec<(Identifier, Option<Expr>)>,
    },
    Expression(Expr),
    Block(Vec<Statement>),
    IfElse(IfElseStruct),
    While(WhileStruct),
    DoWhile(DoWhileStruct),
    Return(ReturnStruct),
}

#[derive(Debug, Clone)]
pub enum TopLevelDeclaration {
    Function {
        name: Identifier,
        return_type: Type,
        parameters: Vec<(Type, Identifier)>,
        body: Vec<Statement>,
    },
}
