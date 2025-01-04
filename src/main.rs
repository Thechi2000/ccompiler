use std::{collections::BTreeMap, fs::File, io::Read};

use clap::{command, Parser, Subcommand};
use compiler::Context;
use lalrpop_util::lalrpop_mod;

lalrpop_mod!(grammar);
mod ast;
mod compiler;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Parse { file: String },
    CompileExpr { expr: String },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Parse { file } => {
            let mut str = String::new();
            File::open(file)
                .expect("Unable to open source file")
                .read_to_string(&mut str)
                .expect("Unable to read from source file");

            eprintln!(
                "{:#?}",
                grammar::TopLevelDeclarationParser::new().parse(&str)
            )
        }
        Commands::CompileExpr { expr } => {
            let expr = grammar::ExprParser::new().parse(&expr).unwrap();
            let instructions = compiler::compile_expr(expr, &Context::dummy());

            println!("{}", instructions);
        }
    }
}
