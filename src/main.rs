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
    Tokenise { file: String },
}

fn main() {
    dbg!(grammar::TopLevelDeclarationParser::new().parse(r#"
    int main(const int argc) {
        while (a < 0) {
            int b = a + 1;
            const char c = b;
        }
    }
    "#));
}
