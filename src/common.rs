//! Structures and enumeration that are used accross multiple compilation stages.

/// Represents a simple variable that can be manipulated through assembly instructions.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Variable {
    /// Original identifier of the variable, or `None` in case the variable was algorithmically generated.
    name: Option<String>,
    /// Declination index, when there is a name clash, e.g. due to scoping, or when a single variable is deconstructed
    /// into multiple instance (during SSA generation).
    variant: usize,
    // TODO: add type and memory size.
}

/// Iterator utilitary used to generated multiple declinations of a variable from a single name.
struct VariableGenerator {
    name: Option<String>,
    variant: usize,
}
impl Iterator for VariableGenerator {
    type Item = Variable;

    fn next(&mut self) -> Option<Self::Item> {
        let next = Some(Variable {
            name: self.name.clone(),
            variant: self.variant,
        });

        self.variant += 1;

        next
    }
}

impl Variable {
    /// Creates an iterator yielding `Variables` composed from the given `ident` and different declination indexes.
    pub fn generator(ident: Option<String>) -> impl Iterator<Item = Self> {
        VariableGenerator {
            name: ident,
            variant: 0,
        }
    }

    /// Creates an iterator yielding `Variables` composed from the given variable's name and different declination
    /// indexes. Resulting instances all have a different index than the original variable.
    pub fn generator_from_var(var: &Variable) -> impl Iterator<Item = Self> {
        VariableGenerator {
            name: var.name.clone(),
            variant: var.variant + 1,
        }
    }

    pub fn from_ident(ident: &str) -> Self {
        Variable {
            name: Some(ident.to_owned()),
            variant: 0,
        }
    }

    /// Format the variable into a readable String.
    pub fn default_fmt(&self) -> String {
        match (&self.name, self.variant) {
            (Some(n), 0) => n.to_owned(),
            (Some(n), i) => format!("{n}#{i}"),
            (None, i) => format!("#r{i}"),
        }
    }

    /// Format the variable into a readable HTML String.
    pub fn html_fmt(&self) -> String {
        match (&self.name, self.variant) {
            (Some(n), 0) => n.to_owned(),
            (Some(n), i) => format!("{n}<sub>{i}</sub>"),
            (None, i) => format!("#r<sub>{i}</sub>"),
        }
    }

    /// Whether the variable originated from a variable in the original code or was fully generated during compilation.
    pub fn is_generated(&self) -> bool {
        self.name.is_none()
    }
}

pub type Litteral = i32;

/// Holds any value that may be used in an operation, either a litteral or a variable.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Var(Variable),
    Lit(Litteral),
}

impl Value {
    /// Formats the value into a readable String, using the provided formatter in case it is a variable.
    pub fn fmt<F: Fn(&Variable) -> String>(&self, varfmt: &F) -> String {
        match self {
            Value::Var(r) => varfmt(r),
            Value::Lit(l) => l.to_string(),
        }
    }
}

#[derive(Debug)]
pub enum BinaryOperator {
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
}

#[derive(Debug)]
pub enum UnaryOperator {
    Assign,
    Ref,
    Deref,
    BNot,
}
