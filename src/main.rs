use std::{fs::File, io::Read};

use clap::{Parser, Subcommand, command};
use lalrpop_util::lalrpop_mod;

lalrpop_mod!(grammar);
mod ast;
mod compiler;
mod rtl;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    target: Target,
}

#[derive(Subcommand)]
enum Target {
    Parse { file: String },
    Rtl { file: String },
}

fn main() {
    let cli = Cli::parse();

    let file = match &cli.target {
        Target::Parse { file } | Target::Rtl { file } => file,
    };

    let mut str = String::new();
    File::open(file)
        .expect("Unable to open source file")
        .read_to_string(&mut str)
        .expect("Unable to read from source file");

    match cli.target {
        Target::Parse { .. } => {
            eprintln!(
                "{:#?}",
                grammar::TopLevelDeclarationParser::new().parse(&str)
            )
        }
        Target::Rtl { .. } => {
            let func = grammar::TopLevelDeclarationParser::new()
                .parse(&str)
                .unwrap();

            let graph = rtl::compile(func);
            let chart = rtl::visualisation::generate_flowchart(graph);

            println!("{chart}");
        }
    }
}
