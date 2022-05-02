use std::io;
use std::process;
use std::path::Path;

use lexer::lexer as lex;


const PROMPT: &str = ">>>";
const EXIT: &str = "exit";
const WG: &str = ":g";
const WF: &str = ":f";

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
            let mut lx = lex::Lexer::new(String::from(text));
            lx.generate_and_print();
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
    let mut lx = lex::Lexer::new(String::from(input));
    let token = lx.generate()?;
    println!("{:?}", token);
    Ok(())
}

fn start_from_file(file: &str) -> io::Result<()> {
    let path = Path::new(file);
    let path_str = path.to_str().expect("failed to get file path");
    let file = std::fs::read_to_string(path_str)?;
    let mut lx = lex::Lexer::new(file);
    lx.generate_and_print();
    Ok(())    
}

fn main() -> io::Result<()> {
    println!(
        "
        ▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄
        ██░█▀▄█░▄▄▀█░▄▄▀██▄██░▄▄
        ██░▄▀██░▀▀░█░▀▀▄██░▄█▄▄▀
        ██░██░█▄██▄█▄█▄▄█▄▄▄█▄▄▄
        ▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"
    );
    println!("Welcome to Karis Lang (v0.1.0) Interactive Console\n");
    println!("
    Type  :g  to extract tokens with generator (useful for long sequences) or
    Type  :ng  to extract tokens once (returns only a single token from a sequence) or 
    Type  :f  for file input");    

    let mut input = String::new();
    io::stdin().read_line(&mut input)?;
    let trimmed_input = input.trim();    

    if trimmed_input == WF {
        println!("Paste the file path then press enter");
        let mut file_path = String::new();
        io::stdin().read_line(&mut file_path)?;
        start_from_file(file_path.trim())
    }else{        
        println!("Let's begin \n");
        start(trimmed_input)
    }    
}
