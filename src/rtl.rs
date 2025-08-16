use std::{collections::BTreeMap, vec};

use crate::{
    ast,
    graph::{self, Node as _, NodeHandle},
};

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

pub type Register = String;
pub type Function = NodeHandle;

pub type Litteral = i32;

#[derive(Debug)]
pub enum RegLit {
    Reg(Register),
    Lit(Litteral),
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
        prev: Vec<NodeHandle>,
        next: NodeHandle,
    },
    Ret {
        prev: NodeHandle,
        value: Option<RegLit>,
    },
}

impl Node {
    fn next_mut(&mut self) -> Option<&mut NodeHandle> {
        match self {
            Node::Ret { .. } => None,
            Node::Start { next, .. }
            | Node::UnOp { next, .. }
            | Node::BinOp { next, .. }
            | Node::Fork { next, .. }
            | Node::Join { next, .. } => Some(next),
        }
    }
}

pub struct Graph {
    pub nodes: Vec<Node>,
    functions: BTreeMap<String, Function>,
    pub(crate) register_generator: Box<dyn Iterator<Item = Register>>,
}

impl std::fmt::Debug for Graph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Graph")
            .field("nodes", &self.nodes)
            .field("functions", &self.functions)
            .finish()
    }
}

impl Graph {
    fn new() -> Graph {
        Graph {
            nodes: Default::default(),
            functions: Default::default(),
            register_generator: Box::new((0..).map(|v| format!("r#{v}"))),
        }
    }

    fn add(&mut self, n: Node) -> NodeHandle {
        self.nodes.push(n);
        self.nodes.len() - 1
    }

    fn add_block(&mut self, prev: NodeHandle, i: &[ast::Statement]) -> NodeHandle {
        i.iter()
            .fold(prev, |acc, statement| self.add_statement(acc, statement))
    }
    fn add_statement(&mut self, prev: NodeHandle, statement: &ast::Statement) -> NodeHandle {
        match statement {
            ast::Statement::Declaration { variables, .. } => {
                variables.iter().fold(prev, |acc, (r, v)| {
                    if let Some(v) = v {
                        self.add_declaration(acc, r, v)
                    } else {
                        acc
                    }
                })
            }
            ast::Statement::Expression(expr) => self.add_expr(prev, expr).1,
            ast::Statement::Block(statements) => self.add_block(prev, statements),
            ast::Statement::IfElse(if_else_struct) => self.add_if(prev, if_else_struct),
            ast::Statement::While(while_struct) => self.add_while(prev, while_struct),
            ast::Statement::DoWhile(do_while_struct) => self.add_dowhile(prev, do_while_struct),
            ast::Statement::Return(return_struct) => self.add_return(prev, return_struct),
        }
    }

    fn add_expr(&mut self, prev: NodeHandle, expr: &ast::Expr) -> (Register, NodeHandle) {
        if let ast::Expr::Identifier(reg) = expr {
            if !reg.starts_with("r#") {
                (format!("i#{reg}"), prev)
            } else {
                (reg.clone(), prev)
            }
        } else {
            let reg = self.alloc_reg();
            let hdx = self.add_declaration(prev, &reg, expr);
            (reg, hdx)
        }
    }

    fn add_declaration(
        &mut self,
        prev: NodeHandle,
        r: &ast::Identifier,
        v: &ast::Expr,
    ) -> NodeHandle {
        let r = if !r.starts_with("r#") {
            format!("i#{r}")
        } else {
            r.clone()
        };

        match v {
            ast::Expr::BinaryOperation { lhs, rhs, op } => {
                let (lhs_reg, hdx) = self.add_expr(prev, lhs);
                let (rhs_reg, hdx) = self.add_expr(hdx, rhs);

                let op = match op {
                    ast::BinOp::Mul => BinaryOperator::Mul,
                    ast::BinOp::Div => BinaryOperator::Div,
                    ast::BinOp::Mod => BinaryOperator::Mod,
                    ast::BinOp::Add => BinaryOperator::Add,
                    ast::BinOp::Sub => BinaryOperator::Sub,
                    ast::BinOp::ShiftLeft => BinaryOperator::ShiftLeft,
                    ast::BinOp::ShiftRight => BinaryOperator::ShiftRight,
                    ast::BinOp::LessThan => BinaryOperator::LessThan,
                    ast::BinOp::LessOrEqual => BinaryOperator::LessOrEqual,
                    ast::BinOp::GreaterThan => BinaryOperator::GreaterThan,
                    ast::BinOp::GreaterOrEqual => BinaryOperator::GreaterOrEqual,
                    ast::BinOp::Equal => BinaryOperator::Equal,
                    ast::BinOp::Different => BinaryOperator::Different,
                    ast::BinOp::BAnd => BinaryOperator::BAnd,
                    ast::BinOp::BOr => BinaryOperator::BOr,
                    ast::BinOp::Xor => BinaryOperator::Xor,
                    ast::BinOp::LAnd => BinaryOperator::LAnd,
                    ast::BinOp::LOr => BinaryOperator::LOr,
                    ast::BinOp::Assign => {
                        return self.add(Node::UnOp {
                            prev: hdx,
                            next: 0,
                            op: UnaryOperator::Assign,
                            hs: RegLit::Reg(rhs_reg),
                            dst: lhs_reg,
                        });
                    }
                    ast::BinOp::Access | ast::BinOp::DerefAccess => panic!(),
                };

                self.add(Node::BinOp {
                    prev: hdx,
                    next: 0,
                    op,
                    lhs: RegLit::Reg(lhs_reg),
                    rhs: RegLit::Reg(rhs_reg),
                    dst: r.clone(),
                })
            }
            ast::Expr::PreUnaryOperation { hs, op } => {
                let (reg, expr_hdx) = self.add_expr(prev, hs);

                self.add(match op {
                    ast::PreUnOp::Minus => Node::BinOp {
                        prev: expr_hdx,
                        next: 0,
                        op: BinaryOperator::Sub,
                        lhs: RegLit::Lit(0),
                        rhs: RegLit::Reg(reg),
                        dst: r.clone(),
                    },
                    ast::PreUnOp::BNot => Node::UnOp {
                        prev: expr_hdx,
                        next: 0,
                        op: UnaryOperator::BNot,
                        hs: RegLit::Reg(reg),
                        dst: r.clone(),
                    },
                    ast::PreUnOp::LNot => Node::BinOp {
                        prev: expr_hdx,
                        next: 0,
                        op: BinaryOperator::Equal,
                        lhs: RegLit::Reg(reg),
                        rhs: RegLit::Lit(0),
                        dst: r.clone(),
                    },
                    ast::PreUnOp::Ref => Node::UnOp {
                        prev: expr_hdx,
                        next: 0,
                        op: UnaryOperator::Ref,
                        hs: RegLit::Reg(reg),
                        dst: r.clone(),
                    },
                    ast::PreUnOp::Deref => Node::UnOp {
                        prev: expr_hdx,
                        next: 0,
                        op: UnaryOperator::Deref,
                        hs: RegLit::Reg(reg),
                        dst: r.clone(),
                    },
                    ast::PreUnOp::Plus => Node::UnOp {
                        prev: expr_hdx,
                        next: 0,
                        op: UnaryOperator::Assign,
                        hs: RegLit::Reg(reg),
                        dst: r.clone(),
                    },
                    ast::PreUnOp::Incr | ast::PreUnOp::Decr => todo!(),
                })
            }
            ast::Expr::PostUnaryOperation { .. } => todo!(),
            ast::Expr::Identifier(id) => self.add(Node::UnOp {
                prev,
                next: 0,
                op: UnaryOperator::Assign,
                hs: RegLit::Reg(format!("i#{id}")),
                dst: r.clone(),
            }),
            ast::Expr::Litteral(ast::Litteral::String(_)) => panic!(),
            ast::Expr::Litteral(ast::Litteral::Integer(i)) => self.add(Node::UnOp {
                prev,
                next: 0,
                op: UnaryOperator::Assign,
                hs: RegLit::Lit(*i as i32),
                dst: r.clone(),
            }),
            ast::Expr::FunctionCall { .. } => todo!(),
        }
    }

    fn add_if(&mut self, prev: NodeHandle, i: &ast::IfElseStruct) -> NodeHandle {
        let (cond, cond_hdx) = self.add_expr(prev, &i.condition);

        let inv_cond = self.alloc_reg();
        let inv_cond_hdx = self.add(Node::BinOp {
            prev: cond_hdx,
            next: 0,
            op: BinaryOperator::Equal,
            lhs: RegLit::Reg(cond),
            rhs: RegLit::Lit(0),
            dst: inv_cond.clone(),
        });

        let branch_hdx = self.add(Node::Fork {
            prev: inv_cond_hdx,
            location: 0,
            cond: inv_cond,
            next: 0,
        });

        let then_hdx = self.add_statement(branch_hdx, &i.true_case);

        if let Some(fc) = &i.false_case {
            let else_start_hdx = self.add(Node::Join {
                prev: vec![branch_hdx],
                next: 0,
            });
            let else_hdx = self.add_statement(else_start_hdx, fc);

            let Node::Fork { location, .. } = &mut self.nodes[branch_hdx] else {
                panic!()
            };

            *location = else_start_hdx;

            self.add(Node::Join {
                prev: vec![then_hdx, else_hdx],
                next: 0,
            })
        } else {
            let end_hdx = self.add(Node::Join {
                prev: vec![then_hdx, branch_hdx],
                next: 0,
            });

            let Node::Fork { location, .. } = &mut self.nodes[branch_hdx] else {
                panic!()
            };

            *location = end_hdx;

            end_hdx
        }
    }

    fn add_while(&mut self, prev: NodeHandle, w: &ast::WhileStruct) -> NodeHandle {
        let join_hdx = self.add(Node::Join {
            prev: vec![prev],
            next: 0,
        });

        let (cond, cond_hdx) = self.add_expr(join_hdx, &w.condition);

        let inv_cond = self.alloc_reg();
        let inv_cond_hdx = self.add(Node::BinOp {
            prev: cond_hdx,
            next: 0,
            op: BinaryOperator::Equal,
            lhs: RegLit::Reg(cond),
            rhs: RegLit::Lit(0),
            dst: inv_cond.clone(),
        });

        let branch_hdx = self.add(Node::Fork {
            prev: inv_cond_hdx,
            location: 0,
            cond: inv_cond,
            next: 0,
        });

        let body_hdx = self.add_statement(branch_hdx, &w.body);

        let Node::Join { prev, .. } = &mut self.nodes[join_hdx] else {
            panic!()
        };
        prev.push(body_hdx);

        let end_hdx = self.add(Node::Join {
            prev: vec![branch_hdx],
            next: 0,
        });

        let Node::Fork { location, .. } = &mut self.nodes[branch_hdx] else {
            panic!()
        };
        *location = end_hdx;

        end_hdx
    }

    fn add_dowhile(&mut self, prev: NodeHandle, dw: &ast::DoWhileStruct) -> NodeHandle {
        let join_hdx = self.add(Node::Join {
            prev: vec![prev],
            next: 0,
        });

        let post_body = self.add_statement(join_hdx, &dw.body);

        let (cond, cond_hdx) = self.add_expr(post_body, &dw.condition);

        let end_hdx = self.add(Node::Fork {
            prev: cond_hdx,
            location: join_hdx,
            cond,
            next: 0,
        });

        let Node::Join { prev, .. } = &mut self.nodes[join_hdx] else {
            panic!()
        };
        prev.push(end_hdx);

        end_hdx
    }

    fn add_return(&mut self, prev: NodeHandle, ret: &ast::ReturnStruct) -> NodeHandle {
        let (ret_value, new_prev) = ret
            .value
            .as_ref()
            .map(|expr| self.add_expr(prev, expr))
            .unzip();

        self.add(Node::Ret {
            prev: new_prev.unwrap_or(prev),
            value: ret_value.map(RegLit::Reg),
        })
    }

    fn alloc_reg(&mut self) -> Register {
        self.register_generator.next().unwrap()
    }

    fn populate_forward_edge(&mut self) {
        for hdx in 0..self.nodes.len() {
            for prev in self.nodes[hdx].prev() {
                if let Node::Fork { location, .. } = &self.nodes[prev]
                    && *location == hdx
                {
                    continue;
                }

                if let Some(next) = self.nodes[prev].next_mut() {
                    *next = hdx
                }
            }
        }
    }

    fn clean(&mut self) {
        // Locate and remove Join nodes with exactly one predecessor.
        for hdx in 0..self.nodes.len() {
            if let Node::Join { prev, next } = &self.nodes[hdx]
                && prev.len() == 1
            {
                let prev = prev[0];
                let next = *next;

                // If the next of the Join's predecessor points to join then replace it with the next of Join.
                if let Some(old_next) = self.nodes[prev].next_mut()
                    && *old_next == hdx
                {
                    *old_next = next;
                }

                // If the Join's predecessor is a Fork to the Join then replace it with the next of Join.
                if let Node::Fork { location, .. } = &mut self.nodes[prev]
                    && *location == hdx
                {
                    *location = next;
                }
            }
        }
    }
}

pub fn compile(tlds: Vec<ast::TopLevelDeclaration>) -> Graph {
    let mut graph = Graph::new();

    for tld in tlds {
        let ast::TopLevelDeclaration::Function {
            name,
            body: statements,
            ..
        } = tld;

        let start = graph.add(Node::Start { name, next: 0 });
        graph.add_block(start, &statements);
    }
    graph.populate_forward_edge();
    graph.clean();

    graph
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
            Node::Start { name, .. } => format!("{name}()"),
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
