use std::collections::{BTreeMap, BTreeSet};

use crate::{
    graph::{self, NodeHandle},
    rtl::{self, BinaryOperator, RegLit, Register, UnaryOperator},
};

pub struct Graph {
    pub nodes: Vec<Node>,
    register_generator: Box<dyn Iterator<Item = Register>>,
}

impl std::fmt::Debug for Graph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Graph").field("nodes", &self.nodes).finish()
    }
}

impl Graph {
    fn new() -> Graph {
        Graph {
            nodes: Default::default(),
            register_generator: Box::new((0..).map(|v| format!("r#{v}"))),
        }
    }
}

#[derive(Debug)]
pub enum Node {
    Start {
        next: NodeHandle,
    },
    UnOp {
        prev: NodeHandle,
        next: NodeHandle,
        op: UnaryOperator,
        hs: RegLit,
        dst: Register,
    },
    BinOp {
        prev: NodeHandle,
        next: NodeHandle,
        op: BinaryOperator,
        lhs: RegLit,
        rhs: RegLit,
        dst: Register,
    },
    Fork {
        prev: NodeHandle,
        location: NodeHandle,
        next: NodeHandle,
        cond: Register,
    },
    Join {
        joins: BTreeMap<Register, Vec<Register>>,
        prev: Vec<NodeHandle>,
        next: NodeHandle,
    },
    Ret {
        prev: NodeHandle,
        value: Option<RegLit>,
    },
}

impl graph::Node for Node {
    fn prev(&self) -> Vec<NodeHandle> {
        match self {
            Node::Start { .. } => vec![],
            Node::UnOp { prev, .. }
            | Node::BinOp { prev, .. }
            | Node::Fork { prev, .. }
            | Node::Ret { prev, .. } => vec![*prev],
            Node::Join { prev, .. } => prev.clone(),
        }
    }
    fn next(&self) -> Vec<NodeHandle> {
        match self {
            Node::Ret { .. } => vec![],
            Node::Start { next, .. }
            | Node::UnOp { next, .. }
            | Node::BinOp { next, .. }
            | Node::Join { next, .. } => vec![*next],
            Node::Fork { next, location, .. } => vec![*next, *location],
        }
    }

    fn label(&self) -> String {
        fn format_reg(r: &RegLit) -> String {
            match r {
                RegLit::Reg(r) => r.to_owned(),
                RegLit::Lit(l) => l.to_string(),
            }
        }

        match self {
            Node::Start { .. } => "Start".to_owned(),
            Node::UnOp { op, hs, dst, .. } => format!(
                "{dst} <- {}{}",
                match op {
                    UnaryOperator::Assign => "",
                    UnaryOperator::Ref => "&",
                    UnaryOperator::Deref => "*",
                    UnaryOperator::BNot => "~",
                },
                format_reg(hs)
            ),
            Node::BinOp {
                op, lhs, rhs, dst, ..
            } => format!(
                "{dst} <- {} {} {}",
                format_reg(lhs),
                match op {
                    BinaryOperator::Mul => "*",
                    BinaryOperator::Div => "/",
                    BinaryOperator::Mod => "%",
                    BinaryOperator::Add => "+",
                    BinaryOperator::Sub => "-",
                    BinaryOperator::ShiftLeft => "<<",
                    BinaryOperator::ShiftRight => ">>",
                    BinaryOperator::LessThan => "<",
                    BinaryOperator::LessOrEqual => "<=",
                    BinaryOperator::GreaterThan => ">",
                    BinaryOperator::GreaterOrEqual => ">=",
                    BinaryOperator::Equal => "==",
                    BinaryOperator::Different => "!=",
                    BinaryOperator::BAnd => "&",
                    BinaryOperator::BOr => "|",
                    BinaryOperator::Xor => "^",
                    BinaryOperator::LAnd => "&&",
                    BinaryOperator::LOr => "||",
                },
                format_reg(rhs)
            ),
            Node::Fork { cond, .. } => format!("Fork {cond}"),
            Node::Join { .. } => "Join".to_owned(),
            Node::Ret { value, .. } => {
                format!("Ret {}", value.as_ref().map(format_reg).unwrap_or_default())
            }
        }
    }
}

impl graph::Graph<Node> for Graph {
    fn entrypoints(&self) -> Vec<NodeHandle> {
        vec![
            self.nodes()
                .iter()
                .enumerate()
                .find(|n| matches!(n.1, Node::Start { .. }))
                .unwrap()
                .0,
        ]
    }

    fn nodes(&self) -> &[Node] {
        &self.nodes
    }
}

pub fn compile(graph: rtl::Graph) -> Graph {
    let variables = graph
        .nodes
        .iter()
        .filter_map(|n| match n {
            rtl::Node::BinOp { dst, .. } if dst.starts_with("i#") => Some(dst),
            rtl::Node::UnOp { dst, .. } if dst.starts_with("i#") => Some(dst),
            _ => None,
        })
        .collect::<BTreeSet<_>>();

    dbg!(&variables);

    todo!()
}
