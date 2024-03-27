mod lexer;
mod repl;
mod ast;
mod parser;
mod object;

fn main() {
    // repl::start(Box::new(std::io::stdin()), Box::new(std::io::stdout()));
    let input = r#"
        1 + (2 + 3) + 4;
    "#;
    println!("{}", input);
    let mut l = lexer::Lexer::new(input);
    let mut token = l.next_token();
    while token != lexer::Token::EOF {
        println!("{token:?}");
        token = l.next_token();
    }
    let mut p = parser::Parser::new(lexer::Lexer::new(input));
    let pr = p.parse_program();
    println!("len: {}", pr.statements.len());
    println!("{:?}", pr);
    let err = p.errors();
    println!("{:?}", err);
}
