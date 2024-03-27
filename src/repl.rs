use std::io::{Read, Write};

use crate::lexer;
use crate::parser;

const PROMPT: &[u8] = b">> ";

pub fn start(mut input: Box<dyn Read>, mut output: Box<dyn Write>) {
    let mut buff = vec![0; 1024];
    loop {
        output.write(PROMPT).unwrap();
        output.flush().unwrap();
        let sz = input.read(&mut buff).unwrap();
        if sz == 0 {
            return
        }
        let line = &String::from_utf8_lossy(&buff).to_string();
        let mut lex = lexer::Lexer::new(line);
        let mut p = parser::Parser::new(lex);
        let prog = p.parse_program();
        println!("{}", prog.to_string());
    }
}

