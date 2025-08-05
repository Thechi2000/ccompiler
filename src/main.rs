use std::{
    fs::{self, File},
    io::Read,
    path::PathBuf,
};

use clap::{command, Parser, Subcommand};
use compiler::Context;
use lalrpop_util::lalrpop_mod;

lalrpop_mod!(grammar);
mod ast;
mod compiler;
mod rtl;

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
    CompileFile { path: PathBuf },
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
        Commands::CompileFile { path } => {
            let content = fs::read_to_string(path).unwrap();
            let func = grammar::TopLevelDeclarationParser::new()
                .parse(&content)
                .unwrap();

            let graph = rtl::compile(func);

            dbg!(graph);
        }
    }
}
