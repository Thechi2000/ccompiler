use std::{fs::File, io::Read};

use clap::{Parser, Subcommand};
use lalrpop_util::lalrpop_mod;

use crate::{
    stages::{rtl, ssa},
    visualization::graph::{GraphType, generate_representation},
};

lalrpop_mod!(grammar);
mod ast;
mod stages;
mod visualization;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    target: Target,
}

#[derive(Subcommand)]
enum Target {
    Parse {
        file: String,
    },
    Rtl {
        file: String,
        #[arg(short, long)]
        output: GraphType,
    },
    Ssa {
        file: String,
        #[arg(short, long)]
        output: GraphType,
    },
}

fn main() {
    let cli = Cli::parse();

    let file = match &cli.target {
        Target::Parse { file } | Target::Rtl { file, .. } | Target::Ssa { file, .. } => file,
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
                grammar::FileParser::new().parse(&str)
            )
        }
        Target::Rtl { output, .. } => {
            let funcs = grammar::FileParser::new().parse(&str).unwrap();

            let graph = rtl::compile(funcs);

            let output = generate_representation(graph, output);
            println!("{output}");
        }

        Target::Ssa { output, .. } => {
            let funcs = grammar::FileParser::new().parse(&str).unwrap();

            let graph = rtl::compile(funcs);
            let graph = ssa::compile(graph);

            let output = generate_representation(graph, output);
            println!("{output}");
        }
    }
}
