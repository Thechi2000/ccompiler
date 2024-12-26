use std::fs::File;

use clap::{command, Parser, Subcommand};
use tokeniser::{IterReader, Tokeniser};

mod tokeniser;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Tokenise { file: String },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Tokenise { file } => {
            let tok = Tokeniser::new(IterReader::new(File::open(file).unwrap()));

            tok.for_each(|t| println!("{}", t));
        }
    }
}
