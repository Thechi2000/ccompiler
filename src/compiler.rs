#![allow(unused)]

use std::collections::BTreeMap;

use crate::ast::{BinOp, Expr, Litteral};

// Let's define: R*x for temporary storage, R\d+ for variables. Other are reserved for other uses (stack-pointer, etc.).

pub mod asm {
    use std::fmt::{Display, Write};

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
            })
        }
    }

    pub enum Operand {
        Reg(Register),
        Imm(i32),
    }

    impl Display for Operand {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Operand::Reg(register) => f.write_fmt(format_args!("%{}", register)),
                Operand::Imm(i) => f.write_fmt(format_args!("$0x{:x}", i)),
            }
        }
    }

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
}

pub type VariableMap = BTreeMap<String, asm::Register>;

pub fn compile_expr(expr: Expr, variables: &VariableMap) -> Vec<asm::Inst> {
    use asm::inst;
    use asm::Operand::*;
    use asm::Register::*;

    let mut instructions = vec![];

    fn inner(expr: &Expr, variables: &VariableMap, instructions: &mut Vec<asm::Inst>) {
        match expr {
            Expr::BinaryOperation { lhs, rhs, op } => {
                inner(&lhs, variables, instructions);
                inner(&rhs, variables, instructions);

                match op {
                    BinOp::Div => {
                        instructions.push(inst("pop", vec![Reg(R9)]));
                        instructions.push(inst("pop", vec![Reg(Rax)]));

                        instructions.push(inst("xor", vec![Reg(Rbx), Reg(Rbx)]));
                        instructions.push(inst("xor", vec![Reg(Rcx), Reg(Rcx)]));
                        instructions.push(inst("xor", vec![Reg(Rdx), Reg(Rdx)]));

                        instructions.push(inst("idivq", vec![Reg(R9)]));

                        instructions.push(inst("push", vec![Reg(Rax)]));
                    }
                    BinOp::Mod => {
                        instructions.push(inst("pop", vec![Reg(R9)]));
                        instructions.push(inst("pop", vec![Reg(Rax)]));

                        instructions.push(inst("xor", vec![Reg(Rbx), Reg(Rbx)]));
                        instructions.push(inst("xor", vec![Reg(Rcx), Reg(Rcx)]));
                        instructions.push(inst("xor", vec![Reg(Rdx), Reg(Rdx)]));

                        instructions.push(inst("idivq", vec![Reg(R9)]));

                        instructions.push(inst("push", vec![Reg(Rdx)]));
                    }
                    _ => {
                        instructions.push(inst("pop", vec![Reg(Rbx)]));
                        instructions.push(inst("pop", vec![Reg(Rax)]));

                        instructions.push(match op {
                            BinOp::Add => inst("add", vec![Reg(Rbx), Reg(Rax)]),
                            BinOp::Sub => inst("sub", vec![Reg(Rbx), Reg(Rax)]),
                            BinOp::Mul => inst("imul", vec![Reg(Rbx), Reg(Rax)]),
                            BinOp::ShiftLeft => inst("sal", vec![Reg(Rax), Reg(Rbx)]),
                            BinOp::ShiftRight => inst("shr", vec![Reg(Rax), Reg(Rbx) ]),
                            BinOp::LessThan => todo!(),
                            BinOp::LessOrEqual => todo!(),
                            BinOp::GreaterThan => todo!(),
                            BinOp::GreaterOrEqual => todo!(),
                            BinOp::Equal => todo!(),
                            BinOp::Different => todo!(),
                            BinOp::BAnd => inst("and", vec![Reg(Rbx), Reg(Rax)]),
                            BinOp::BOr => inst("or", vec![Reg(Rbx), Reg(Rax)]),
                            BinOp::Xor => inst("xor", vec![Reg(Rbx), Reg(Rax)]),
                            BinOp::LAnd => todo!(),
                            BinOp::LOr => todo!(),
                            BinOp::Access => todo!(),
                            BinOp::DerefAccess => todo!(),
                            BinOp::Div | BinOp::Mod => panic!(),
                        });

                        instructions.push(inst("push", vec![Reg(Rax)]));
                    }
                }
            }
            Expr::PreUnaryOperation { hs, op } => todo!(),
            Expr::PostUnaryOperation { hs, op } => todo!(),
            Expr::Identifier(_) => todo!(),
            Expr::Litteral(Litteral::Integer(i)) => instructions.push(inst("push", vec![Imm(*i as i32)])),
            Expr::Litteral(Litteral::String(_)) => todo!(),
            Expr::FunctionCall { name, parameters } => todo!(),
        }
    }

    inner(&expr, variables, &mut instructions);

    instructions
}
