use std::{fs::File, io::Read};

use clap::{command, Parser, Subcommand};
use lalrpop_util::lalrpop_mod;

lalrpop_mod!(grammar);
mod ast;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Parse { file: String },
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
    }
}
