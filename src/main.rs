use rocks::{
    ast::{debug::AstFormatter, Parser},
    interpreter::Interpreter,
    token::Tokenizer,
};
use std::env;
use std::error;
use std::fs;
use std::io::{self, BufRead, BufReader};

fn main() -> Result<(), Box<dyn error::Error>> {
    let args = env::args().skip(1).collect::<Vec<_>>();
    if args.len() > 1 {
        println!("Usage: rocks [script]");
        return Ok(());
    }

    if !args.is_empty() {
        run_file(&args[0])?;
    } else {
        run_prompt()?;
    }

    Ok(())
}

fn run_file(path: &str) -> Result<(), io::Error> {
    run(&fs::read_to_string(path)?);
    Ok(())
}

fn run_prompt() -> Result<(), io::Error> {
    let rdr = BufReader::new(io::stdin());
    for line in rdr.lines() {
        run(&line?);
    }
    Ok(())
}

fn run(src: &str) -> Result<(), Box<dyn error::Error>> {
    let tokenizer = Tokenizer::new(src);
    let parser = Parser::new(tokenizer);
    let mut repl = Interpreter;

    for result in parser {
        match &result {
            Ok(expr) => match repl.evaluate(expr) {
                Ok(val) => println!("{}", val),
                Err(err) => println!("{}", err),
            },
            Err(err) => println!("{}", err),
        }
    }

    Ok(())
}

// fn report()
