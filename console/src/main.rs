use std::io;
use std::path::Path;
use std::process;

use clap::{arg, Arg, ArgAction, Command};

use colored::*;
use errors::errors::KarisError;
use evaluator::evaluate::Evaluator;
use lexer::lexer::{self as lex, Lexer};
use parser::parser::Parser;

use rustyline::validate::MatchingBracketValidator;
use rustyline::{
    completion::FilenameCompleter, error::ReadlineError, highlight::MatchingBracketHighlighter,
    hint::HistoryHinter, At, Cmd, CompletionType, Config, EditMode, Editor, KeyCode, KeyEvent,
    Modifiers, Movement, Word,
};
use rustyline_derive::{Completer, Helper, Highlighter, Hinter, Validator};

const PROMPT: &str = ">> ";
const EXIT: &str = ":exit";

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

fn main() -> Result<(), KarisError> {
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
        .subcommand(Command::new("repl").about("Read-Evaluate-Print-Loop for Karis"))
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

        Some(("repl", _sub_matches)) => evaluate_from_input(),

        _ => {
            println!("Nothing to do");
            Ok(())
        }
    }
}

fn lexer_interactive() -> Result<(), KarisError> {
    println!("{}\n", KARIS_INTERACTIVE_MESSAGE);
    println!("Copy-Paste or type your Karis program after the prompt\n",);
    println!("Type :exit to close the console\n",);

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

fn lexer_from_file(file: &str) -> Result<(), KarisError> {
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

fn parser_from_file(file: &str, inspect: &bool) -> Result<(), KarisError> {
    let path = Path::new(file);
    let path_str = path.to_str().expect("failed to get file path");
    let file = std::fs::read_to_string(path_str)?;
    if file.is_empty() {
        println!("Nothing to parse \n");
    } else {
        let lx = lex::Lexer::new(file);
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("program_tree.json"))?;
        let inspect = *inspect;
        if inspect {
            res.inspect_and_print()?
        } else {
            println!("{:?}", res);
        }
    }
    Ok(())
}

#[derive(Helper, Completer, Hinter, Validator, Highlighter)]
struct EditorHelper {
    #[rustyline(Completer)]
    completer: FilenameCompleter,

    #[allow(dead_code)]
    highlighter: MatchingBracketHighlighter,

    #[rustyline(Validator)]
    validator: MatchingBracketValidator,

    #[rustyline(Hinter)]
    hinter: HistoryHinter,

    #[allow(dead_code)]
    colored_prompt: String,
}

fn evaluate_from_input() -> Result<(), KarisError> {
    println!("{}", KARIS_INTERACTIVE_MESSAGE.cyan());
    println!("use SHIFT+DOWN to add a new line");
    println!(" ");

    let config = Config::builder()
        .history_ignore_space(true)
        .completion_type(CompletionType::List)
        .edit_mode(EditMode::Emacs)
        .build();

    let helper = EditorHelper {
        completer: FilenameCompleter::new(),
        highlighter: MatchingBracketHighlighter::new(),
        hinter: HistoryHinter {},
        colored_prompt: "".to_owned(),
        validator: MatchingBracketValidator::new(),
    };

    let mut editor = Editor::with_config(config)?;
    editor.set_helper(Some(helper));

    editor.bind_sequence(
        KeyEvent(KeyCode::Left, Modifiers::CTRL),
        Cmd::Move(Movement::BackwardWord(1, Word::Big)),
    );
    editor.bind_sequence(
        KeyEvent(KeyCode::Right, Modifiers::CTRL),
        Cmd::Move(Movement::ForwardWord(1, At::AfterEnd, Word::Big)),
    );

    editor.bind_sequence(KeyEvent(KeyCode::Down, Modifiers::SHIFT), Cmd::Newline);

    loop {
        let prompt = format!("{}", PROMPT.yellow());

        match editor.readline(prompt.as_str()) {
            Ok(input) => {
                editor.add_history_entry(input.clone());

                let lx = Lexer::new(input);
                let parser = Parser::new(lx);
                let mut evaluator = Evaluator::new(parser);
                evaluator.repl_evaluate_program();
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => break,

            Err(err) => {
                eprintln!("Error: {:?}", err);
                break;
            }
        }
    }

    Ok(())
}
