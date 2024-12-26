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
    Litteral(i32),
}
