use std::fmt::Display;

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

pub type BlockStatement = Vec<Statement>;

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Let(LetStatement { name, value }) => write!(f, "let {} = {};", name, value),
            Statement::Return(ReturnStatement { value }) => write!(f, "return {};", value),
            Statement::Expression(ExpressionStatement { expression }) => {
                write!(f, "{};", expression)
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(i64),
    BooleanLiteral(bool),
    FunctionLiteral {
        parameters: Vec<Identifier>,
        body: BlockStatement,
    },
    PrefixExpression {
        operator: String,
        right: Box<Expression>,
    },
    InfixExpression {
        left: Box<Expression>,
        operator: String,
        right: Box<Expression>,
    },
    IfExpression {
        condition: Box<Expression>,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    },
    CallExpression {
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::IntegerLiteral(int) => write!(f, "{}", int),
            Expression::BooleanLiteral(boolean) => write!(f, "{}", boolean),
            Expression::Identifier(identifier) => write!(f, "{}", identifier),
            Expression::PrefixExpression { operator, right } => {
                write!(f, "({}{})", operator, right)
            }
            Expression::InfixExpression {
                left,
                operator,
                right,
            } => write!(f, "({} {} {})", left, operator, right),
            Expression::IfExpression {
                condition,
                consequence,
                alternative,
            } => {
                let mut result = String::new();
                result.push_str("if ");
                result.push_str(&condition.to_string());
                result.push_str(" { ");
                consequence.iter().for_each(|statement| {
                    result.push_str(&statement.to_string());
                });
                result.push_str(" }");
                if let Some(alternative) = alternative {
                    result.push_str(" else ");
                    result.push_str("{ ");
                    alternative.iter().for_each(|statement| {
                        result.push_str(&statement.to_string());
                    });
                    result.push_str(" }");
                }
                return write!(f, "{}", result);
            }
            Expression::FunctionLiteral { parameters, body } => {
                let mut result = String::new();
                result.push_str("fn(");
                result.push_str(
                    parameters
                        .iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                        .as_str(),
                );
                result.push_str(") { ");
                body.iter().for_each(|statement| {
                    result.push_str(&statement.to_string());
                });
                result.push_str(" }");
                return write!(f, "{}", result);
            }
            Expression::CallExpression {
                function,
                arguments,
            } => {
                let mut result = String::new();
                result.push_str(&function.to_string());
                result.push_str("(");
                result.push_str(
                    arguments
                        .iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                        .as_str(),
                );
                result.push_str(")");
                return write!(f, "{}", result);
            }
        }
    }
}

pub type Identifier = String;

#[derive(Debug, PartialEq, Clone)]
pub struct LetStatement {
    pub name: Identifier,
    pub value: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStatement {
    pub value: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExpressionStatement {
    pub expression: Expression,
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut output = String::new();
        for stmt in &self.statements {
            output.push_str(&format!("{}", stmt));
        }
        return write!(f, "{}", output);
    }
}
