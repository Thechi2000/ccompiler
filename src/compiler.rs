#![allow(unused)]

use std::{collections::BTreeMap, rc::Weak};

use asm::AssemblyOutput;

use crate::ast::{BinOp, Expr, Identifier, Litteral};

// Let's define: R*x for temporary storage, R\d+ for variables. Other are reserved for other uses (stack-pointer, etc.).

pub mod asm {
    use std::{
        collections::BTreeMap,
        fmt::{Display, Write},
    };

    #[derive(Debug, Clone, Copy)]
    pub enum Register {
        Rax,
        Rcx,
        Rdx,
        Rbx,
        Rsi,
        Rdi,
        Rsp,
        Rbp,
        R8,
        R9,
        R10,
        R11,
        R12,
        R13,
        R14,
        R15,
        Al,
        Bl,
        Cl,
        Dl,
    }

    impl Display for Register {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str(match self {
                Register::Rax => "rax",
                Register::Rcx => "rcx",
                Register::Rdx => "rdx",
                Register::Rbx => "rbx",
                Register::Rsi => "rsi",
                Register::Rdi => "rdi",
                Register::Rsp => "rsp",
                Register::Rbp => "rbp",
                Register::R8 => "r8",
                Register::R9 => "r9",
                Register::R10 => "r10",
                Register::R11 => "r11",
                Register::R12 => "r12",
                Register::R13 => "r13",
                Register::R14 => "r14",
                Register::R15 => "r15",
                Register::Al => "al",
                Register::Bl => "bl",
                Register::Cl => "cl",
                Register::Dl => "dl",
            })
        }
    }

    #[derive(Debug, Clone)]
    pub enum Operand {
        Reg(Register),
        Imm(i32),
        Mem(Register, i32),
    }

    impl Display for Operand {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Operand::Reg(register) => f.write_fmt(format_args!("%{}", register)),
                Operand::Imm(i) => f.write_fmt(format_args!("$0x{:x}", i)),
                Operand::Mem(register, 0) => f.write_fmt(format_args!("(%{})", register)),
                Operand::Mem(register, i) => f.write_fmt(format_args!("{}(%{})", i, register)),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Inst {
        name: &'static str,
        operands: Vec<Operand>,
    }

    pub fn inst(name: &'static str, operands: Vec<Operand>) -> Inst {
        return Inst { name, operands };
    }

    impl Display for Inst {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str(self.name)?;
            f.write_char(' ')?;
            f.write_str(
                &self
                    .operands
                    .iter()
                    .map(|o| o.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
            )
        }
    }

    #[derive(Debug, Clone, Default)]
    pub struct AssemblyOutput {
        instructions: Vec<Inst>,
        labels: BTreeMap<String, usize>,
    }

    impl AssemblyOutput {
        pub fn add0(&mut self, name: &'static str) {
            self.instructions.push(inst(name, vec![]));
        }

        pub fn add1(&mut self, name: &'static str, operand: Operand) {
            self.instructions.push(inst(name, vec![operand]));
        }

        pub fn add2(&mut self, name: &'static str, lhs: Operand, rhs: Operand) {
            self.instructions.push(inst(name, vec![lhs, rhs]));
        }
    }

    impl Display for AssemblyOutput {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            for i in self.instructions.iter() {
                f.write_fmt(format_args!("{}\n", i))?;
            }

            Ok(())
        }
    }
}

pub struct Context {
    parent: Option<Weak<Context>>,
    /// Offset relative to function rbp
    variables: BTreeMap<Identifier, usize>,
    /// Offset relative to function rbp. Used to correctly initialize variable local to this context.
    scope_rbp: usize,
}

impl Context {
    pub fn dummy() -> Self {
        let mut map = BTreeMap::<Identifier, usize>::default();
        map.insert("a".to_owned(), 0);
        map.insert("b".to_owned(), 4);

        Self {
            parent: None,
            variables: map,
            scope_rbp: 0,
        }
    }

    fn get_var(&self, name: &Identifier) -> Option<usize> {
        self.variables.get(name).copied().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|w| w.upgrade())
                .and_then(|c| c.get_var(name))
        })
    }
}

pub type VariableMap = BTreeMap<String, asm::Register>;

pub fn compile_expr(expr: Expr, context: &Context) -> AssemblyOutput {
    use asm::inst;
    use asm::Operand::*;
    use asm::Register::*;

    let mut out = Default::default();

    fn inner(expr: &Expr, out: &mut AssemblyOutput, context: &Context) {
        match expr {
            Expr::BinaryOperation { lhs, rhs, op } => {
                inner(&lhs, out, context);
                inner(&rhs, out, context);

                match op {
                    BinOp::Div | BinOp::Mod => {
                        out.add1("pop", Reg(R9));
                        out.add1("pop", Reg(Rax));

                        out.add2("xor", Reg(Rbx), Reg(Rbx));
                        out.add2("xor", Reg(Rcx), Reg(Rcx));
                        out.add2("xor", Reg(Rdx), Reg(Rdx));

                        out.add1("idivq", Reg(R9));

                        out.add1("push", Reg(if let BinOp::Div = op { Rax } else { Rdx }));
                    }
                    _ => {
                        out.add1("pop", Reg(Rbx));
                        out.add1("pop", Reg(Rax));

                        match op {
                            BinOp::Add => out.add2("add", Reg(Rbx), Reg(Rax)),
                            BinOp::Sub => out.add2("sub", Reg(Rbx), Reg(Rax)),
                            BinOp::Mul => out.add2("imul", Reg(Rbx), Reg(Rax)),
                            BinOp::ShiftLeft => out.add2("sal", Reg(Rax), Reg(Rbx)),
                            BinOp::ShiftRight => out.add2("shr", Reg(Rax), Reg(Rbx)),

                            BinOp::LessThan
                            | BinOp::LessOrEqual
                            | BinOp::GreaterThan
                            | BinOp::GreaterOrEqual
                            | BinOp::Equal
                            | BinOp::Different => {
                                out.add2("cmp", Reg(Rbx), Reg(Rax));
                                out.add1(
                                    match op {
                                        BinOp::LessThan => "setl",
                                        BinOp::LessOrEqual => "setle",
                                        BinOp::GreaterThan => "setg",
                                        BinOp::GreaterOrEqual => "setge",
                                        BinOp::Equal => "sete",
                                        BinOp::Different => "setne",
                                        _ => panic!(),
                                    },
                                    Reg(Al),
                                );
                                out.add2("movzx", Reg(Al), Reg(Rax));
                            }

                            BinOp::BAnd => out.add2("and", Reg(Rbx), Reg(Rax)),
                            BinOp::BOr => out.add2("or", Reg(Rbx), Reg(Rax)),
                            BinOp::Xor => out.add2("xor", Reg(Rbx), Reg(Rax)),
                            BinOp::LAnd => todo!(),
                            BinOp::LOr => todo!(),
                            BinOp::Access => todo!(),
                            BinOp::DerefAccess => todo!(),
                            _ => panic!(),
                        };

                        out.add1("push", Reg(Rax));
                    }
                }
            }
            Expr::PreUnaryOperation { hs, op } => todo!(),
            Expr::PostUnaryOperation { hs, op } => todo!(),
            Expr::Identifier(i) => {
                let offset = context.get_var(i).expect("Unknown variable name");
                out.add1("push", Mem(Rbp, offset as i32));
            },
            Expr::Litteral(Litteral::Integer(i)) => out.add1("push", Imm(*i as i32)),
            Expr::Litteral(Litteral::String(s)) => todo!(),
            Expr::FunctionCall { name, parameters } => todo!(),
        }
    }

    inner(&expr, &mut out, context);

    out
}
