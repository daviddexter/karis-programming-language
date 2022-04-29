use std::io;
use std::process;

use lexer::lexer;

const PROMPT: &str = ">>>";
const EXIT: &str = "exit";
const WG: &str = ":g";

fn start(t: &str) -> io::Result<()> {
    let mut input = String::new();

    loop {
        print!("{} ", PROMPT);

        io::stdin().read_line(&mut input)?;

        let text = input.trim();

        if text == EXIT {
            println!("Closing interactive console. Catch you later :) ");
            process::exit(0)
        }

        let with_generator = || {
            let mut lx = lexer::Lexer::new(String::from(text));
            lx.generate();
        };

        let without_generator = || {
            let resp = evaluate(text).err();
            if let Some(err) = resp {
                println!("Error: {}", err)
            }
        };

        if t == WG {
            with_generator();
        } else {
            without_generator();
        }

        input.clear();
    }
}

fn evaluate(input: &str) -> io::Result<()> {
    let mut lx = lexer::Lexer::new(String::from(input));
    let token = lx.new_token()?;
    println!("{:?}", token);
    Ok(())
}

fn main() -> io::Result<()> {
    println!("Welcome to Karis Lang (v0.1.0) Interactive Console\n");
    println!("Type  :g  to extract tokens with generator (useful for long sequences)  \n");
    println!("or  \n");
    println!("Type  :ng  to extract tokens once (returns only a single token from a sequence)  \n");

    let mut input = String::new();
    io::stdin().read_line(&mut input)?;

    println!("Let's begin \n");
    start(input.trim())
}
