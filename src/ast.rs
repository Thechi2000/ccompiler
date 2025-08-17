#![allow(unused)]

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum PrimitiveType {
    UInteger,
    SInteger,
    Float,
    Void,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct PrimitiveSizedType(pub PrimitiveType, pub usize);
#[derive(Debug, Clone, Copy)]
pub enum TypeSpecifier {
    Long,
    Short,
    Unsigned,
    Signed,
}
#[derive(Debug, Clone, Copy)]
pub enum TypeQualifier {
    Const,
    Restrict,
    Volatile,
}

impl PrimitiveSizedType {
    fn min_size(&self) -> usize {
        match &self.0 {
            PrimitiveType::UInteger | PrimitiveType::SInteger => 1,
            PrimitiveType::Float => 8,
            PrimitiveType::Void => 0,
        }
    }
    fn max_size(&self) -> usize {
        match &self.0 {
            PrimitiveType::UInteger | PrimitiveType::SInteger | PrimitiveType::Float => 8,
            PrimitiveType::Void => 0,
        }
    }
    fn map_size<F: FnOnce(usize) -> usize>(self, f: F) -> Self {
        let new_size = f(self.1);
        if !(self.min_size()..=self.max_size()).contains(&new_size) {
            panic!();
        }
        Self(self.0, new_size)
    }

    pub fn with_specifier(self, spec: TypeSpecifier) -> Self {
        match spec {
            TypeSpecifier::Long => self.map_size(|s| s * 2),
            TypeSpecifier::Short => self.map_size(|s| s / 2),
            TypeSpecifier::Unsigned => {
                assert!(matches!(
                    self.0,
                    PrimitiveType::UInteger | PrimitiveType::SInteger
                ));
                Self(PrimitiveType::UInteger, self.1)
            }
            TypeSpecifier::Signed => {
                assert!(matches!(
                    self.0,
                    PrimitiveType::UInteger | PrimitiveType::SInteger
                ));
                Self(PrimitiveType::SInteger, self.1)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Type {
    pub primitive: PrimitiveSizedType,
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
