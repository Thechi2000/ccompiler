use std::collections::{BTreeMap, BTreeSet};

use crate::{
    common::*,
    graph::{self, Graph as _, Node as _, NodeHandle},
    rtl::{self},
};

pub struct Graph {
    pub nodes: Vec<Node>,
}

impl std::fmt::Debug for Graph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Graph").field("nodes", &self.nodes).finish()
    }
}

#[derive(Debug)]
pub enum Node {
    Start {
        name: String,
        next: NodeHandle,
    },
    UnOp {
        prev: NodeHandle,
        next: NodeHandle,
        op: UnaryOperator,
        hs: Value,
        dst: Variable,
    },
    BinOp {
        prev: NodeHandle,
        next: NodeHandle,
        op: BinaryOperator,
        lhs: Value,
        rhs: Value,
        dst: Variable,
    },
    Fork {
        prev: NodeHandle,
        location: NodeHandle,
        next: NodeHandle,
        cond: Variable,
    },
    Join {
        joins: BTreeMap<Variable, (Variable, Vec<Variable>)>,
        prev: Vec<NodeHandle>,
        next: NodeHandle,
    },
    Ret {
        prev: NodeHandle,
        value: Option<Value>,
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

    fn label<F: Fn(&Variable) -> String>(&self, varfmt: F) -> String {
        match self {
            Node::Start { name, .. } => format!("{name}()"),
            Node::UnOp { op, hs, dst, .. } => format!(
                "{} <- {}{}",
                varfmt(dst),
                match op {
                    UnaryOperator::Assign => "",
                    UnaryOperator::Ref => "&",
                    UnaryOperator::Deref => "*",
                    UnaryOperator::BNot => "~",
                },
                hs.fmt(&varfmt)
            ),
            Node::BinOp {
                op, lhs, rhs, dst, ..
            } => format!(
                "{} <- {} {} {}",
                varfmt(dst),
                lhs.fmt(&varfmt),
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
                rhs.fmt(&varfmt)
            ),
            Node::Fork { cond, .. } => format!("Fork {}", varfmt(cond)),
            Node::Join { joins, .. } => joins
                .values()
                .map(|(dst, srcs)| {
                    format!(
                        "{} <- Φ({})",
                        varfmt(dst),
                        srcs.iter()
                            .map(|r| varfmt(r))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                })
                .collect::<Vec<_>>()
                .join("\n"),
            Node::Ret { value, .. } => {
                format!(
                    "Ret {}",
                    value.as_ref().map(|s| s.fmt(&varfmt)).unwrap_or_default()
                )
            }
        }
    }
}

impl graph::Graph<Node> for Graph {
    fn entrypoints(&self) -> Vec<NodeHandle> {
        self.nodes()
            .iter()
            .enumerate()
            .filter(|n| matches!(n.1, Node::Start { .. }))
            .map(|(hdx, _)| hdx)
            .collect()
    }

    fn nodes(&self) -> &[Node] {
        &self.nodes
    }
}

pub fn compile(graph: rtl::Graph) -> Graph {
    fn walk_variable(graph: &mut Graph, hdx: NodeHandle, old_name: &Variable) {
        let mut generator = Variable::generator_from_var(old_name);
        let mut visisted = BTreeSet::new();
        let mut to_process = vec![(hdx, generator.next().unwrap())];

        while let Some((hdx, mut name)) = to_process.pop() {
            let node = &mut graph.nodes[hdx];
            visisted.insert(hdx);

            match node {
                Node::Join { joins, .. } => {
                    name = match joins.entry(old_name.clone()) {
                        std::collections::btree_map::Entry::Vacant(vacant_entry) => {
                            let dest = generator.next().unwrap();
                            vacant_entry.insert((dest.clone(), vec![name.clone()]));
                            dest
                        }
                        std::collections::btree_map::Entry::Occupied(mut occupied_entry) => {
                            occupied_entry.get_mut().1.push(name.clone());
                            // If the Join already has an entry for this, it means it was already parcoured once, thus
                            // we skip adding the following node to avoid infinite loops.
                            continue;
                        }
                    }
                }
                Node::Start { .. } => {}
                Node::UnOp { hs, dst, .. } => {
                    if dst == old_name {
                        name = generator.next().unwrap();
                        *dst = name.clone();
                    }

                    if let Value::Var(r) = hs
                        && r == old_name
                    {
                        *r = name.clone();
                    }
                }
                Node::BinOp { lhs, rhs, dst, .. } => {
                    if dst == old_name {
                        name = generator.next().unwrap();
                        *dst = name.clone();
                    }

                    if let Value::Var(r) = lhs
                        && r == old_name
                    {
                        *r = name.clone();
                    }
                    if let Value::Var(r) = rhs
                        && r == old_name
                    {
                        *r = name.clone();
                    }
                }
                Node::Fork { .. } => {}
                Node::Ret { value, .. } => {
                    if let Some(Value::Var(r)) = value
                        && r == old_name
                    {
                        *r = name.clone();
                    }
                }
            };

            for next in node.next() {
                if !visisted.contains(&next) || matches!(graph.nodes[next], Node::Join { .. }) {
                    // Join nodes must be traversed multiple times, once per predecessors. The match above takes care of
                    // avoiding infinite loops by continuing the loop for already visited joins.
                    to_process.push((next, name.clone()));
                }
            }
        }
    }

    let variables = graph
        .nodes
        .iter()
        .filter_map(|n| match n {
            rtl::Node::BinOp { dst, .. } if !dst.is_generated() => Some(dst.clone()),
            rtl::Node::UnOp { dst, .. } if !dst.is_generated() => Some(dst.clone()),
            _ => None,
        })
        .collect::<BTreeSet<_>>();

    let mut ssa = Graph {
        nodes: graph
            .nodes
            .into_iter()
            .map(|value| match value {
                rtl::Node::Start { name, next } => Node::Start { name, next },
                rtl::Node::UnOp {
                    prev,
                    next,
                    op,
                    hs,
                    dst,
                } => Node::UnOp {
                    prev,
                    next,
                    op,
                    hs,
                    dst,
                },
                rtl::Node::BinOp {
                    prev,
                    next,
                    op,
                    lhs,
                    rhs,
                    dst,
                } => Node::BinOp {
                    prev,
                    next,
                    op,
                    lhs,
                    rhs,
                    dst,
                },
                rtl::Node::Fork {
                    prev,
                    location,
                    next,
                    cond,
                } => Node::Fork {
                    prev,
                    location,
                    next,
                    cond,
                },
                rtl::Node::Join { prev, next } => Node::Join {
                    joins: Default::default(),
                    prev: prev,
                    next: next,
                },
                rtl::Node::Ret { prev, value } => Node::Ret { prev, value },
            })
            .collect(),
    };

    for var in variables {
        for entry in ssa.entrypoints() {
            walk_variable(&mut ssa, entry, &var);
        }
    }

    ssa
}
