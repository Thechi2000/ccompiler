use std::{collections::BTreeMap, vec};

use crate::ast;

#[derive(Debug)]
enum BinaryOperator {
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
enum UnaryOperator {
    Assign,
    Ref,
    Deref,
    BNot,
}

type NodeHandle = usize;
type Register = String;
type Function = NodeHandle;

type Litteral = i32;

#[derive(Debug)]
enum RegLit {
    Reg(Register),
    Lit(Litteral),
}

#[derive(Debug)]
enum Node {
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
        prev: Vec<NodeHandle>,
        next: NodeHandle,
    },
    Ret {
        prev: NodeHandle,
        value: Option<RegLit>,
    },
}

impl Node {
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
    fn next(&self) -> Option<NodeHandle> {
        match self {
            Node::Ret { .. } => None,
            Node::Start { next, .. }
            | Node::UnOp { next, .. }
            | Node::BinOp { next, .. }
            | Node::Fork { next, .. }
            | Node::Join { next, .. } => Some(*next),
        }
    }
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
    nodes: Vec<Node>,
    functions: BTreeMap<String, Function>,
    register_generator: Box<dyn Iterator<Item = Register>>,
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
            ast::Statement::Expression(expr) => self.add_expr(prev, expr),
            ast::Statement::Block(statements) => self.add_block(prev, statements),
            ast::Statement::IfElse(if_else_struct) => self.add_if(prev, if_else_struct),
            ast::Statement::While(while_struct) => self.add_while(prev, while_struct),
            ast::Statement::DoWhile(do_while_struct) => self.add_dowhile(prev, do_while_struct),
            ast::Statement::Return(return_struct) => self.add_return(prev, return_struct),
        }
    }

    fn add_declaration(
        &mut self,
        prev: NodeHandle,
        r: &ast::Identifier,
        v: &ast::Expr,
    ) -> NodeHandle {
        match v {
            ast::Expr::BinaryOperation { lhs, rhs, op } => {
                let (lhs_reg, hdx) = if let ast::Expr::Identifier(i) = lhs.as_ref() {
                    (i.clone(), prev)
                } else {
                    let reg = self.alloc_reg();
                    let hdx = self.add_declaration(prev, &reg, lhs);
                    (reg, hdx)
                };

                let (rhs_reg, hdx) = if let ast::Expr::Identifier(i) = rhs.as_ref() {
                    (i.clone(), hdx)
                } else {
                    let reg = self.alloc_reg();
                    let hdx = self.add_declaration(hdx, &reg, rhs);
                    (reg, hdx)
                };

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
                let reg = self.alloc_reg();
                let expr_hdx = self.add_declaration(prev, &reg, hs);

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
    fn add_expr(&mut self, prev: NodeHandle, expr: &ast::Expr) -> NodeHandle {
        let reg = self.alloc_reg();
        self.add_declaration(prev, &reg, expr)
    }

    fn add_if(&mut self, prev: NodeHandle, i: &ast::IfElseStruct) -> NodeHandle {
        let cond = self.alloc_reg();
        let cond_hdx = self.add_declaration(prev, &cond, &i.condition);

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
        let cond = self.alloc_reg();
        let cond_hdx = self.add_declaration(prev, &cond, &w.condition);

        let inv_cond = self.alloc_reg();
        let inv_cond_hdx = self.add(Node::BinOp {
            prev: cond_hdx,
            next: 0,
            op: BinaryOperator::Equal,
            lhs: RegLit::Reg(cond),
            rhs: RegLit::Lit(0),
            dst: inv_cond.clone(),
        });

        let join_hdx = self.add(Node::Join {
            prev: vec![inv_cond_hdx],
            next: 0,
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

        let cond = self.alloc_reg();
        let post_cond = self.add_declaration(post_body, &cond, &dw.condition);

        let end_hdx = self.add(Node::Fork {
            prev: post_cond,
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
            .map(|expr| {
                let val = self.alloc_reg();
                let nh = self.add_declaration(prev, &val, expr);
                (val, nh)
            })
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
                if let Node::Fork { location, .. } = &self.nodes[hdx]
                    && *location == prev
                {
                    continue;
                }

                if let Some(next) = self.nodes[prev].next_mut() {
                    *next = hdx
                }
            }
        }
    }
}

pub fn compile(tld: ast::TopLevelDeclaration) -> Graph {
    let ast::TopLevelDeclaration::Function {
        body: statements, ..
    } = tld;

    let mut graph = Graph::new();

    let start = graph.add(Node::Start { next: 0 });
    graph.add_block(start, &statements);
    graph.populate_forward_edge();

    graph
}

pub mod visualisation {
    struct Node {
        id: String,
        children: Vec<Node>,
        backward_edges: Vec<String>,
    }
}
