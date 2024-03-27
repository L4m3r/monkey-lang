use crate::{
    ast::{
        BlockStatement, Expression, ExpressionStatement, Identifier, LetStatement, Program,
        ReturnStatement, Statement,
    },
    lexer::{Lexer, Precedence, Token},
};

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(l: Lexer<'a>) -> Parser<'a> {
        let mut p = Parser {
            lexer: l,
            cur_token: Token::EOF,
            peek_token: Token::EOF,
            errors: vec![],
        };
        p.next_token();
        p.next_token();
        p
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };
        while self.cur_token != Token::EOF {
            if let Some(stmt) = self.parse_statement() {
                program.statements.push(stmt);
            }
            self.next_token();
        }
        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token {
            Token::LET => self.parse_let_statement(),
            Token::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let ident = if let Token::IDENT(name) = &self.peek_token {
            name.to_string()
        } else {
            self.errors
                .push(format!("Expected IDENT, got {:?}", self.cur_token));
            return None;
        };
        self.next_token();

        if self.peek_token != Token::ASSIGN {
            self.errors
                .push(format!("Expected ASSIGN, got {:?}", self.cur_token));
            return None;
        }

        self.next_token();
        let value = self.parse_expression(Precedence::Lowest)?;

        while self.cur_token == Token::SEMICOLON {
            self.next_token();
        }
        Some(Statement::Let(LetStatement { name: ident, value }))
    }

    pub fn errors(&self) -> &Vec<String> {
        &self.errors
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();
        let value = self.parse_expression(Precedence::Lowest)?;
        if self.cur_token == Token::SEMICOLON {
            self.next_token();
        }
        Some(Statement::Return(ReturnStatement { value }))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expr = if let Some(expr) = self.parse_expression(Precedence::Lowest) {
            expr
        } else {
            return None;
        };

        if self.peek_token == Token::SEMICOLON {
            self.next_token();
        }

        Some(Statement::Expression(ExpressionStatement {
            expression: expr,
        }))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let mut expr = match self.cur_token {
            Token::IDENT(_) => self.parse_identifier(),
            Token::INT(_) => self.parse_integer_literal(),
            Token::TRUE | Token::FALSE => self.parse_boolean(),
            Token::BANG | Token::MINUS => self.parse_prefix_expression(),
            Token::LPAREN => self.parse_grouped_expression(),
            Token::IF => self.parse_if_expression(),
            Token::FUNCTION => self.parse_function_literal(),
            _ => {
                self.errors
                    .push(format!("no prefix parse function for {:?}", self.cur_token));
                None
            }
        }?;

        while self.peek_token != Token::SEMICOLON && precedence < self.peek_token.clone().into() {
            let infix = match self.peek_token {
                Token::PLUS
                | Token::MINUS
                | Token::SLASH
                | Token::ASTERISK
                | Token::EQ
                | Token::NotEq
                | Token::LT
                | Token::GT => {
                    self.next_token();
                    self.parse_infix_expression(expr.clone())
                }
                Token::LPAREN => self.parse_call_expression(expr.clone()),
                _ => None,
            };
            if infix.is_none() {
                return None;
            }
            expr = infix.unwrap();
        }
        Some(expr)
    }

    fn parse_identifier(&self) -> Option<Expression> {
        if let Token::IDENT(value) = &self.cur_token {
            return Some(Expression::Identifier(value.to_string()));
        }
        None
    }

    fn parse_integer_literal(&self) -> Option<Expression> {
        if let Token::INT(value) = &self.cur_token {
            let value = value.parse::<i64>().ok()?;
            return Some(Expression::IntegerLiteral(value));
        }
        None
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let operator = self.cur_token.to_string();
        self.next_token();

        let right = Box::new(self.parse_expression(Precedence::Prefix)?);
        Some(Expression::PrefixExpression { operator, right })
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let operator = self.cur_token.to_string();
        let precedence: Precedence = self.cur_token.clone().into();
        self.next_token();
        let right = self.parse_expression(precedence);
        if right.is_none() {
            return Some(left);
        }
        let right = right.unwrap();
        Some(Expression::InfixExpression {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        })
    }

    fn parse_boolean(&self) -> Option<Expression> {
        match self.cur_token {
            Token::TRUE | Token::FALSE => {
                Some(Expression::BooleanLiteral(self.cur_token == Token::TRUE))
            }
            _ => panic!("parse_boolean called with cur_token: {}", self.cur_token),
        }
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();
        let expr = self.parse_expression(Precedence::Lowest)?;
        println!("peek_token: {}", self.peek_token);
        if self.peek_token != Token::RPAREN {
            return None;
        }
        self.next_token();
        Some(expr)
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        if self.peek_token != Token::LPAREN {
            return None;
        }
        self.next_token();
        let cond = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token != Token::RPAREN && self.peek_token != Token::LBRACE {
            return None;
        }
        let cons = self.parse_block_statement();

        let alt = if self.peek_token == Token::ELSE {
            self.next_token();
            if self.peek_token != Token::LBRACE {
                return None;
            }
            Some(self.parse_block_statement())
        } else {
            None
        };
        Some(Expression::IfExpression {
            condition: Box::new(cond),
            consequence: cons,
            alternative: alt,
        })
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let mut statements: BlockStatement = vec![];
        self.next_token();

        while self.cur_token != Token::RBRACE && self.cur_token != Token::EOF {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
            self.next_token();
        }
        statements
    }

    fn parse_function_literal(&mut self) -> Option<Expression> {
        if self.peek_token != Token::LPAREN {
            return None;
        }
        let parameters = self.parse_function_parameters()?;
        if self.peek_token != Token::LBRACE {
            return None;
        }
        let body = self.parse_block_statement();

        Some(Expression::FunctionLiteral { parameters, body })
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<Identifier>> {
        let mut identifiers: Vec<Identifier> = vec![];
        if self.peek_token == Token::RPAREN {
            self.next_token();
            return Some(identifiers);
        }

        self.next_token();
        let ident: Identifier = self.cur_token.to_string();
        identifiers.push(ident);

        while self.peek_token == Token::COMMA {
            self.next_token();
            self.next_token();
            let ident: Identifier = self.cur_token.to_string();
            identifiers.push(ident);
        }

        if self.peek_token == Token::RPAREN {
            return None;
        }
        Some(identifiers)
    }

    fn parse_call_expression(&mut self, function: Expression) -> Option<Expression> {
        let args = self.parse_call_arguments()?;
        Some(Expression::CallExpression {
            function: Box::new(function),
            arguments: args,
        })
    }

    fn parse_call_arguments(&mut self) -> Option<Vec<Expression>> {
        let mut args = vec![];
        if self.peek_token == Token::RPAREN {
            self.next_token();
            return Some(args);
        }

        self.next_token();
        args.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token == Token::COMMA {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(Precedence::Lowest)?);
        }

        if self.peek_token != Token::RPAREN {
            None
        } else {
            Some(args)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Expression::{Identifier, IntegerLiteral, PrefixExpression, BooleanLiteral};
    use crate::lexer::Lexer;

    use super::*;

    fn asset_let_statement(statement: &Statement, ident: &str, exp: &Expression) {
        match &statement {
            Statement::Let(LetStatement {
                name: identifier,
                value,
            }) => {
                assert_eq!(identifier, ident);
                assert_eq!(value, exp);
            }
            _ => assert!(false, "Expected LetStatement, got {:?}", statement),
        }
    }

    fn asset_return_statement(statement: &Statement, exp: &Expression) {
        match &statement {
            Statement::Return(ReturnStatement { value }) => {
                assert_eq!(value, exp);
            }
            _ => assert!(false, "Expected ReturnStatement, got {:?}", statement),
        }
    }

    fn asset_expression_statement(statement: &Statement, exp: &Expression) {
        match statement {
            Statement::Expression(ExpressionStatement { expression: data }) => {
                assert_eq!(data, exp);
            }
            _ => assert!(false, "Expected ExpressionStatement, got {:?}", statement),
        }
    }

    fn asset_prefix_expression(statement: &Statement, op: &str, exp: &Expression) {
        match statement {
            Statement::Expression(ExpressionStatement { expression: data }) => match data {
                PrefixExpression { operator, right } => {
                    assert_eq!(operator, op);
                    assert_eq!(right.as_ref(), exp);
                }
                _ => assert!(false, "Expected PrefixExpression, got {:?}", data),
            },
            _ => assert!(false, "Expected ExpressionStatement, got {:?}", statement),
        }
    }

    fn assert_infix_expression(
        statement: &Statement,
        left: &Expression,
        op: &str,
        right: &Expression,
    ) {
        match statement {
            Statement::Expression(data) => match &data.expression {
                Expression::InfixExpression {
                    operator,
                    left: l,
                    right: r,
                } => {
                    assert_eq!(operator, op);
                    assert_eq!(l.as_ref(), left);
                    assert_eq!(r.as_ref(), right);
                }
                _ => assert!(
                    false,
                    "Expected InfixExpression {:?}, got {:?}",
                    statement.to_string(),
                    data.expression.to_string()
                ),
            },
            _ => assert!(false, "Expected ExpressionStatement, got {:?}", statement),
        }
    }

    fn infix_expression(left: Expression, operator: String, right: Expression) -> Expression {
        return Expression::InfixExpression {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        };
    }

    /*
    #[test]
    fn test_let_statements_literal() {
        let input = r#"
        let name = "John";
        let age = 30;
        let isMale = true;
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(program.statements.len(), 3);

        asset_let_statement(&program.statements[0], "name", &StringLiteral("John".to_string()));
        asset_let_statement(&program.statements[1], "age", &IntegerLiteral(30));
        asset_let_statement(&program.statements[2], "isMale", &BooleanLiteral(true));
    }
    */

    #[test]
    fn test_return_statements() {
        let input = r#"
        return 5;
        return 10;
        return 993322;
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(program.statements.len(), 3);

        asset_return_statement(&program.statements[0], &IntegerLiteral(5));
        asset_return_statement(&program.statements[1], &IntegerLiteral(10));
        asset_return_statement(&program.statements[2], &IntegerLiteral(993322));
    }

    #[test]
    fn test_identifier_expression() {
        std::env::set_var("RUST_LOG", "trace");

        let input = r#"
        foobar;
        5;
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(program.statements.len(), 2);

        asset_expression_statement(&program.statements[0], &Identifier("foobar".to_string()));
        asset_expression_statement(&program.statements[1], &IntegerLiteral(5));
    }

    #[test]
    fn test_prefix_expressions() {
        let input = r#"
        !5;
        -15;
        !-a;
        !true;
        !false;
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(program.statements.len(), 5);

        asset_prefix_expression(&program.statements[0], "!", &IntegerLiteral(5));
        asset_prefix_expression(&program.statements[1], "-", &IntegerLiteral(15));
        asset_prefix_expression(
            &program.statements[2],
            "!",
            &PrefixExpression {
                operator: "-".to_string(),
                right: Box::new(Identifier("a".to_string())),
            },
        );
        asset_prefix_expression(&program.statements[3], "!", &BooleanLiteral(true));
        asset_prefix_expression(&program.statements[4], "!", &BooleanLiteral(false));
    }
    #[test]
    fn test_infix_expressions() {
        let input = r#"
        5 + 5;
        5 - 5;
        5 * 5;
        5 / 5;
        5 > 5;
        5 < 5;
        5 == 5;
        5 != 5;

        -a * b;
        a + b + c;
        a + b - c;
        a * b * c;
        a * b / c;
        a + b / c;
        a + b * c + d / e - f;
        3 + 4; -5 * 5;
        5 > 4 == 3 < 4;
        5 < 4 != 3 > 4;
        3 + 4 * 5 == 3 * 1 + 4 * 5;

        3 > 5 == false;
        3 < 5 == true;
        true != false;

        1 + (2 + 3) + 4;
        (5 + 5) * 2;
        2 / (5 + 5);
        -(5 + 5);
        !(true == true);
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        // program.to_string().lines().collect::<Vec<&str>>().iter().enumerate().for_each(|(i, line)| {
        //     warn!("{}: {}", i, line);
        // });

        assert_eq!(program.statements.len(), 28);

        assert_infix_expression(
            &program.statements[0],
            &IntegerLiteral(5),
            "+",
            &IntegerLiteral(5),
        );
        assert_infix_expression(
            &program.statements[1],
            &IntegerLiteral(5),
            "-",
            &IntegerLiteral(5),
        );
        assert_infix_expression(
            &program.statements[2],
            &IntegerLiteral(5),
            "*",
            &IntegerLiteral(5),
        );
        assert_infix_expression(
            &program.statements[3],
            &IntegerLiteral(5),
            "/",
            &IntegerLiteral(5),
        );
        assert_infix_expression(
            &program.statements[4],
            &IntegerLiteral(5),
            ">",
            &IntegerLiteral(5),
        );
        assert_infix_expression(
            &program.statements[5],
            &IntegerLiteral(5),
            "<",
            &IntegerLiteral(5),
        );
        assert_infix_expression(
            &program.statements[6],
            &IntegerLiteral(5),
            "==",
            &IntegerLiteral(5),
        );
        assert_infix_expression(
            &program.statements[7],
            &IntegerLiteral(5),
            "!=",
            &IntegerLiteral(5),
        );
        assert_infix_expression(
            &program.statements[8],
            &PrefixExpression {
                operator: "-".to_string(),
                right: Box::new(Identifier("a".to_string())),
            },
            "*",
            &Identifier("b".to_string()),
        );
        assert_infix_expression(
            &program.statements[9],
            &infix_expression(
                Identifier("a".to_string()),
                "+".to_string(),
                Identifier("b".to_string()),
            ),
            "+",
            &Identifier("c".to_string()),
        );
        assert_infix_expression(
            &program.statements[10],
            &infix_expression(
                Identifier("a".to_string()),
                "+".to_string(),
                Identifier("b".to_string()),
            ),
            "-",
            &Identifier("c".to_string()),
        );
        assert_infix_expression(
            &program.statements[11],
            &infix_expression(
                Identifier("a".to_string()),
                "*".to_string(),
                Identifier("b".to_string()),
            ),
            "*",
            &Identifier("c".to_string()),
        );
        assert_infix_expression(
            &program.statements[12],
            &infix_expression(
                Identifier("a".to_string()),
                "*".to_string(),
                Identifier("b".to_string()),
            ),
            "/",
            &Identifier("c".to_string()),
        );
        assert_eq!(&program.statements[13].to_string(), "(a + (b / c));");
        assert_eq!(
            &program.statements[14].to_string(),
            "(((a + (b * c)) + (d / e)) - f);"
        );
        assert_eq!(&program.statements[15].to_string(), "(3 + 4);");
        assert_eq!(&program.statements[16].to_string(), "((-5) * 5);");
        assert_eq!(&program.statements[17].to_string(), "((5 > 4) == (3 < 4));");
        assert_eq!(&program.statements[18].to_string(), "((5 < 4) != (3 > 4));");
        assert_eq!(
            &program.statements[19].to_string(),
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));"
        );

        assert_eq!(&program.statements[20].to_string(), "((3 > 5) == false);");
        assert_eq!(&program.statements[21].to_string(), "((3 < 5) == true);");
        assert_eq!(&program.statements[22].to_string(), "(true != false);");

        assert_eq!(&program.statements[23].to_string(), "((1 + (2 + 3)) + 4);");
        assert_eq!(&program.statements[24].to_string(), "((5 + 5) * 2);");
        assert_eq!(&program.statements[25].to_string(), "(2 / (5 + 5));");
        assert_eq!(&program.statements[26].to_string(), "(-(5 + 5));");
        assert_eq!(&program.statements[27].to_string(), "(!(true == true));");
    }
}
