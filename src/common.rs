#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Variable {
    name: Option<String>,
    variant: usize,
}

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
    pub fn generator(ident: Option<String>) -> impl Iterator<Item = Self> {
        VariableGenerator {
            name: ident,
            variant: 0,
        }
    }

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

    pub fn default_fmt(&self) -> String {
        match (&self.name, self.variant) {
            (Some(n), 0) => n.to_owned(),
            (Some(n), i) => format!("{n}#{i}"),
            (None, i) => format!("#r{i}"),
        }
    }

    pub fn html_fmt(&self) -> String {
        match (&self.name, self.variant) {
            (Some(n), 0) => n.to_owned(),
            (Some(n), i) => format!("{n}<sub>{i}</sub>"),
            (None, i) => format!("#r<sub>{i}</sub>"),
        }
    }

    pub fn is_generated(&self) -> bool {
        self.name.is_none()
    }
}

pub type Litteral = i32;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Var(Variable),
    Lit(Litteral),
}

impl Value {
    pub fn fmt<F: Fn(&Variable) -> String>(&self, regfmt: &F) -> String {
        match self {
            Value::Var(r) => regfmt(r),
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
