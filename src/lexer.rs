use std::fmt::Display;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    ILLEGAL,
    EOF,
    // Identifiers + literals
    IDENT(String),
    INT(String),
    // Operators
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,
    EQ,
    NotEq,
    // Delimiters
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    // Keywords
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::ILLEGAL => write!(f, "ILLEGAL"),
            Token::EOF => write!(f, "EOF"),
            Token::IDENT(ident) => write!(f, "IDENT: {}", ident),
            Token::INT(int) => write!(f, "INT: {}", int),
            // Token::STRING(string) => write!(f, "STRING: {}", string),
            Token::ASSIGN => write!(f, "="),
            Token::PLUS => write!(f, "+"),
            Token::MINUS => write!(f, "-"),
            Token::BANG => write!(f, "!"),
            Token::ASTERISK => write!(f, "*"),
            Token::SLASH => write!(f, "/"),
            Token::LT => write!(f, "<"),
            Token::GT => write!(f, ">"),
            // Token::LTE => write!(f, "<="),
            // Token::GTE => write!(f, ">="),
            Token::EQ => write!(f, "=="),
            Token::NotEq=> write!(f, "!="),
            Token::COMMA => write!(f, ","),
            Token::SEMICOLON => write!(f, ";"),
            // Token::COLON => write!(f, ":"),
            Token::LPAREN => write!(f, "("),
            Token::RPAREN => write!(f, ")"),
            Token::LBRACE => write!(f, "{{"),
            Token::RBRACE => write!(f, "}}"),
            // Token::LBRACKET => write!(f, "["),
            // Token::RBRACKET => write!(f, "]"),
            Token::FUNCTION => write!(f, "FUNCTION"),
            Token::LET => write!(f, "LET"),
            Token::TRUE => write!(f, "TRUE"),
            Token::FALSE => write!(f, "FALSE"),
            Token::IF => write!(f, "IF"),
            Token::ELSE => write!(f, "ELSE"),
            Token::RETURN => write!(f, "RETURN"),
        }
    }
}

impl Into<Precedence> for Token {
    fn into(self) -> Precedence {
        match self {
            Token::EQ => Precedence::Equals,
            Token::NotEq => Precedence::Equals,
            Token::LT => Precedence::LessGreater,
            Token::GT => Precedence::LessGreater,
            Token::PLUS => Precedence::Sum,
            Token::MINUS => Precedence::Sum,
            Token::SLASH => Precedence::Product,
            Token::ASTERISK => Precedence::Product,
            Token::LPAREN => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }
}

#[repr(u8)]
#[derive(PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}

impl Precedence {
    fn value(&self) -> u8 {
        unsafe { *(self as *const Self as *const u8) }
    }

    fn is_greater_than(&self, another: &Precedence) -> bool {
        self.value() > another.value()
    }
}

#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a [u8],
    pos: usize,
    readpos: usize,
    ch: u8,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        let mut l = Lexer {
            input: input.as_bytes(),
            pos: 0,
            readpos: 0,
            ch: 0,
        };
        l.read_char();
        l
    }

    fn read_char(&mut self) {
        if self.readpos >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input[self.readpos];
        }
        self.pos = self.readpos;
        self.readpos += 1;
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = match self.ch {
            b'=' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::EQ
                } else {
                    Token::ASSIGN
                }
            }
            b';' => Token::SEMICOLON,
            b'(' => Token::LPAREN,
            b')' => Token::RPAREN,
            b'{' => Token::LBRACE,
            b'}' => Token::RBRACE,
            b',' => Token::COMMA,
            b'+' => Token::PLUS,
            b'-' => Token::MINUS,
            b'!' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::NotEq
                } else {
                    Token::BANG
                }
            }
            b'*' => Token::ASTERISK,
            b'/' => Token::SLASH,
            b'<' => Token::LT,
            b'>' => Token::GT,
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let ident = self.read_ident();
                return match ident.as_str() {
                    "fn" => Token::FUNCTION,
                    "let" => Token::LET,
                    "true" => Token::TRUE,
                    "false" => Token::FALSE,
                    "if" => Token::IF,
                    "else" => Token::ELSE,
                    "return" => Token::RETURN,
                    _ => Token::IDENT(ident),
                };
            }
            b'0'..=b'9' => return Token::INT(self.read_int()),
            0 => Token::EOF,
            _ => Token::ILLEGAL,
        };
        self.read_char();
        token
    }

    fn read_ident(&mut self) -> String {
        let first = self.pos;
        while self.ch.is_ascii_alphabetic() || self.ch == b'_' {
            self.read_char()
        }
        String::from_utf8_lossy(&self.input[first..self.pos]).to_string()
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char()
        }
    }

    fn read_int(&mut self) -> String {
        let first = self.pos;
        while self.ch.is_ascii_digit() {
            self.read_char();
        }
        String::from_utf8_lossy(&self.input[first..self.pos]).to_string()
    }

    fn peek_char(&self) -> u8 {
        if self.readpos >= self.input.len() {
            0
        } else {
            self.input[self.readpos]
        }
    }
}

#[cfg(test)]
mod test {
    use super::{Lexer, Token, Precedence};

    #[test]
    fn test_precedence_compare() {
        let left = Precedence::Lowest;
        let right = Precedence::Equals;
        assert!(left <= right);
    }

    #[test]
    fn test_next_token_basic() {
        let input = "=+(){},;";
        let tokens = vec![
            Token::ASSIGN,
            Token::PLUS,
            Token::LPAREN,
            Token::RPAREN,
            Token::LBRACE,
            Token::RBRACE,
            Token::COMMA,
            Token::SEMICOLON,
            Token::EOF,
        ];

        let mut lexer = Lexer::new(&input);
        for token in tokens {
            let next = lexer.next_token();
            assert_eq!(token, next);
        }
    }

    #[test]
    fn test_next_token() {
        let input = "let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y;
            };
            let result = add(five, ten);
            !-/*5;
            5 < 10 > 5;
            if (5 < 10) {
                return true;
            } else {
                return false;
            }

            10 == 10;
            10 != 9;
        ";
        let tokens = vec![
            Token::LET,
            Token::IDENT(String::from("five")),
            Token::ASSIGN,
            Token::INT(String::from("5")),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT(String::from("ten")),
            Token::ASSIGN,
            Token::INT(String::from("10")),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT(String::from("add")),
            Token::ASSIGN,
            Token::FUNCTION,
            Token::LPAREN,
            Token::IDENT(String::from("x")),
            Token::COMMA,
            Token::IDENT(String::from("y")),
            Token::RPAREN,
            Token::LBRACE,
            Token::IDENT(String::from("x")),
            Token::PLUS,
            Token::IDENT(String::from("y")),
            Token::SEMICOLON,
            Token::RBRACE,
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT(String::from("result")),
            Token::ASSIGN,
            Token::IDENT(String::from("add")),
            Token::LPAREN,
            Token::IDENT(String::from("five")),
            Token::COMMA,
            Token::IDENT(String::from("ten")),
            Token::RPAREN,
            Token::SEMICOLON,
            Token::BANG,
            Token::MINUS,
            Token::SLASH,
            Token::ASTERISK,
            Token::INT(String::from("5")),
            Token::SEMICOLON,
            Token::INT(String::from("5")),
            Token::LT,
            Token::INT(String::from("10")),
            Token::GT,
            Token::INT(String::from("5")),
            Token::SEMICOLON,
            Token::IF,
            Token::LPAREN,
            Token::INT(String::from("5")),
            Token::LT,
            Token::INT(String::from("10")),
            Token::RPAREN,
            Token::LBRACE,
            Token::RETURN,
            Token::TRUE,
            Token::SEMICOLON,
            Token::RBRACE,
            Token::ELSE,
            Token::LBRACE,
            Token::RETURN,
            Token::FALSE,
            Token::SEMICOLON,
            Token::RBRACE,
            Token::INT(String::from("10")),
            Token::EQ,
            Token::INT(String::from("10")),
            Token::SEMICOLON,
            Token::INT(String::from("10")),
            Token::NotEq,
            Token::INT(String::from("9")),
            Token::SEMICOLON,
            Token::EOF,
        ];
        let mut lexer = Lexer::new(&input);
        for token in tokens {
            let next = lexer.next_token();
            assert_eq!(token, next);
        }
    }
}
