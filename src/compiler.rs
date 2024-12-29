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

    pub enum Instruction {
        Mov(Operand, Operand),
        Add(Operand, Operand),
        Sub(Operand, Operand),
        Mul(Operand, Operand),
        Xor(Operand, Operand),
        Div(Operand),
        Push(Operand),
        Pop(Operand),
    }

    impl Display for Instruction {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let (name, operands) = match self {
                Instruction::Mov(lhs, rhs) => ("mov", vec![lhs, rhs]),
                Instruction::Add(lhs, rhs) => ("add", vec![lhs, rhs]),
                Instruction::Sub(lhs, rhs) => ("sub", vec![lhs, rhs]),
                Instruction::Mul(lhs, rhs) => ("imul", vec![lhs, rhs]),
                Instruction::Xor(lhs, rhs) => ("xor", vec![lhs, rhs]),
                Instruction::Div(op) => ("idivq", vec![op]),
                Instruction::Push(operand) => ("push", vec![operand]),
                Instruction::Pop(operand) => ("pop", vec![operand]),
            };

            f.write_str(name)?;
            f.write_char(' ')?;
            f.write_str(
                &operands
                    .iter()
                    .map(|o| o.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
            )
        }
    }
}

pub type VariableMap = BTreeMap<String, asm::Register>;

pub fn compile_expr(expr: Expr, variables: &VariableMap) -> Vec<asm::Instruction> {
    use asm::Instruction::*;
    use asm::Operand::*;
    use asm::Register::*;

    let mut instructions = vec![];

    fn inner(expr: &Expr, variables: &VariableMap, instructions: &mut Vec<asm::Instruction>) {
        match expr {
            Expr::BinaryOperation { lhs, rhs, op } => {
                inner(&lhs, variables, instructions);
                inner(&rhs, variables, instructions);

                match op {
                    BinOp::Div => {
                        instructions.push(Pop(Reg(R9)));
                        instructions.push(Pop(Reg(Rax)));

                        instructions.push(Xor(Reg(Rbx), Reg(Rbx)));
                        instructions.push(Xor(Reg(Rcx), Reg(Rcx)));
                        instructions.push(Xor(Reg(Rdx), Reg(Rdx)));

                        instructions.push(Div(Reg(R9)));

                        instructions.push(Push(Reg(Rax)));
                    }
                    BinOp::Mod => {
                        instructions.push(Pop(Reg(R9)));
                        instructions.push(Pop(Reg(Rax)));

                        instructions.push(Xor(Reg(Rbx), Reg(Rbx)));
                        instructions.push(Xor(Reg(Rcx), Reg(Rcx)));
                        instructions.push(Xor(Reg(Rdx), Reg(Rdx)));

                        instructions.push(Div(Reg(R9)));

                        instructions.push(Push(Reg(Rdx)));
                    }
                    _ => {
                        instructions.push(Pop(Reg(Rbx)));
                        instructions.push(Pop(Reg(Rax)));

                        instructions.push(match op {
                            BinOp::Add => Add(Reg(Rbx), Reg(Rax)),
                            BinOp::Sub => Sub(Reg(Rbx), Reg(Rax)),
                            BinOp::Mul => Mul(Reg(Rbx), Reg(Rax)),
                            BinOp::ShiftLeft => todo!(),
                            BinOp::ShiftRight => todo!(),
                            BinOp::LessThan => todo!(),
                            BinOp::LessOrEqual => todo!(),
                            BinOp::GreaterThan => todo!(),
                            BinOp::GreaterOrEqual => todo!(),
                            BinOp::Equal => todo!(),
                            BinOp::Different => todo!(),
                            BinOp::BAnd => todo!(),
                            BinOp::BOr => todo!(),
                            BinOp::Xor => todo!(),
                            BinOp::LAnd => todo!(),
                            BinOp::LOr => todo!(),
                            BinOp::Access => todo!(),
                            BinOp::DerefAccess => todo!(),
                            BinOp::Div | BinOp::Mod => panic!(),
                        });

                        instructions.push(Push(Reg(Rax)));
                    }
                }
            }
            Expr::PreUnaryOperation { hs, op } => todo!(),
            Expr::PostUnaryOperation { hs, op } => todo!(),
            Expr::Identifier(_) => todo!(),
            Expr::Litteral(Litteral::Integer(i)) => instructions.push(Push(Imm(*i as i32))),
            Expr::Litteral(Litteral::String(_)) => todo!(),
            Expr::FunctionCall { name, parameters } => todo!(),
        }
    }

    inner(&expr, variables, &mut instructions);

    instructions
}
