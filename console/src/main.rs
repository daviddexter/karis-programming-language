use std::io;
use std::process;

use lexer::lexer;

const PROMPT: &str = ">>>";
const EXIT: &str = "exit";

fn start() -> io::Result<()> {
    let mut input = String::new();

    loop {
        println!("{}", PROMPT);

        io::stdin().read_line(&mut input)?;

        let text = input.trim();

        if is_exit(text) {
            println!("Closing interactive console. Catch you later :) ");
            process::exit(0)
        }

        let _ = evaluate(text);

        input.clear();
    }
}

fn evaluate(input: &str) -> io::Result<()> {
    let mut lx = lexer::Lexer::new(String::from(input));
    let token = lx.new_token();
    println!("{:?}", token);

    Ok(())
}

fn is_exit(x: &str) -> bool {
    if x == EXIT {
        true
    } else {
        false
    }
}

fn main() -> io::Result<()> {
    println!("Welcome to Wanja Lang (v0.1.0) Interactive Console");
    start()
}
