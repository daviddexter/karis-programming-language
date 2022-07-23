use std::io;
use std::path::Path;
use std::process;

use clap::{arg, Arg, ArgAction, Command};

use lexer::lexer as lex;
use parser::parser::Parser;

const PROMPT: &str = ">>>";
const EXIT: &str = "exit";

const KARIS_WELCOME_MESSAGE: &str = "
▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄
██░█▀▄█░▄▄▀█░▄▄▀██▄██░▄▄
██░▄▀██░▀▀░█░▀▀▄██░▄█▄▄▀
██░██░█▄██▄█▄█▄▄█▄▄▄█▄▄▄
▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
        
";

const KARIS_INTERACTIVE_MESSAGE: &str = "
▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄
██░█▀▄█░▄▄▀█░▄▄▀██▄██░▄▄
██░▄▀██░▀▀░█░▀▀▄██░▄█▄▄▀
██░██░█▄██▄█▄█▄▄█▄▄▄█▄▄▄
▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
        
Welcome to Karis Lang (v0.1.0) Interactive Console";

fn main() -> io::Result<()> {
    let matches = Command::new(KARIS_WELCOME_MESSAGE)
        .version("v0.1.0")
        .propagate_version(true)
        .subcommand_required(true)
        .arg_required_else_help(true)
        .subcommand(
            Command::new("rlpl")
                .about("Read-Lexer-Print-Loop extracts tokens and prints on stdout")
                .arg_required_else_help(true)
                .arg(
                    Arg::new("interactive")
                        .short('i')
                        .long("interactive")
                        .action(ArgAction::SetTrue)
                        .required(false),
                )
                .arg(arg!(-p --filepath <PATH>).required(false)),
        )
        .subcommand(
            Command::new("rppl")
                .about("Read-Parser-Print-Loop parsers program and prints on stdout")
                .arg_required_else_help(true)
                .arg(arg!(-p --filepath <PATH>).required(false))
                .arg(
                    Arg::new("inspect")
                        .short('i')
                        .long("inspect")
                        .action(ArgAction::SetTrue)
                        .required(false),
                ),
        )
        .get_matches();

    match matches.subcommand() {
        Some(("rlpl", sub_matches)) => {
            let interactive = sub_matches.get_one::<bool>("interactive");
            let file_path = sub_matches.get_one::<String>("filepath");

            if let Some(i) = interactive {
                if *i {
                    return lexer_interactive();
                }
            }

            if let Some(file_path) = file_path {
                return lexer_from_file(file_path);
            }
            Ok(())
        }
        Some(("rppl", sub_matches)) => {
            let file_path = sub_matches.get_one::<String>("filepath");
            let inspect = sub_matches.get_one::<bool>("inspect").unwrap();

            if let Some(file_path) = file_path {
                return parser_from_file(file_path, inspect);
            }

            Ok(())
        }
        _ => {
            println!("Nothing to do");
            Ok(())
        }
    }
}

fn lexer_interactive() -> io::Result<()> {
    println!("{}\n", KARIS_INTERACTIVE_MESSAGE);
    println!("Copy-Paste or type your Karis program after the prompt\n",);
    println!("Type exit to close the console\n",);

    let mut input = String::new();

    loop {
        print!("{} ", PROMPT);

        io::stdin().read_line(&mut input)?;

        let text = input.trim();
        if text.is_empty() {
            println!("Nothing to scan \n");
        } else {
            if text == EXIT {
                println!("Closing interactive console. Catch you later :) \n");
                process::exit(0)
            }

            let mut lx = lex::Lexer::new(String::from(text));
            lx.generate_and_print();
        }

        input.clear();
    }
}

fn lexer_from_file(file: &str) -> io::Result<()> {
    let path = Path::new(file);
    let path_str = path.to_str().expect("failed to get file path");
    if file.is_empty() {
        println!("Nothing to scan \n");
    } else {
        let file = std::fs::read_to_string(path_str)?;
        let mut lx = lex::Lexer::new(file);
        lx.generate_and_print();
    }
    Ok(())
}

fn parser_from_file(file: &str, inspect: &bool) -> io::Result<()> {
    let path = Path::new(file);
    let path_str = path.to_str().expect("failed to get file path");
    let file = std::fs::read_to_string(path_str)?;
    if file.is_empty() {
        println!("Nothing to parse \n");
    } else {
        let lx = lex::Lexer::new(file);
        let mut parser = Parser::new(lx);
        let res = parser.parse()?;
        let inspect = *inspect;
        if inspect {
            res.inspect_and_print()?
        } else {
            println!("{:?}", res);
        }
    }
    Ok(())
}
