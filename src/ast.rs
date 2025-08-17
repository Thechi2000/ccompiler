#![allow(unused)]

pub use typing::*;
mod typing {
    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
    pub enum PrimitiveTypeSpecifier {
        Long,
        Short,
        Unsigned,
        Signed,
    }
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub enum PrimitiveType {
        Char,
        Int,
        Long,
        Float,
        Double,
    }
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
    pub struct ItemPrimitive {
        pub ty: PrimitiveType,
        pub specs: Vec<PrimitiveTypeSpecifier>,
    }

    #[derive(Debug, Clone)]
    pub struct ItemStruct {
        pub ident: Option<Identifier>,
        pub fields: Vec<(Identifier, Type)>,
    }

    #[derive(Debug, Clone)]
    pub struct ItemUnion {
        pub ident: Option<Identifier>,
        pub fields: Vec<(Identifier, Type)>,
    }

    #[derive(Debug, Clone)]
    pub struct ItemPointer {
        pub ty: Box<Type>,
        pub quals: Vec<TypeQualifier>,
    }

    #[derive(Debug, Clone)]
    pub enum ItemType {
        Primitive(ItemPrimitive),
        Struct(ItemStruct),
        Union(ItemUnion),
        Pointer(ItemPointer),
        Void,
    }

    #[derive(Debug, Clone)]
    pub enum TypeQualifier {
        Restrict,
        Volatile,
        Const,
    }
    #[derive(Debug, Clone)]
    pub struct Type {
        pub ty: ItemType,
        pub quals: Vec<TypeQualifier>,
    }
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
