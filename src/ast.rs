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
