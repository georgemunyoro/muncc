use std::sync::Arc;

pub mod ast;
pub mod ast_node;
pub mod error;

use crate::{
    lexer::token::{self, punctuator::PunctuatorKind, Token, TokenKind},
    parser::ast::AstNodePrinter,
};

use self::{
    ast::AstNode,
    ast_node::{AssignmentExpression, Constant, Expression, Identifier, StringLiteral},
    error::MCCParserError,
};

pub struct Parser {
    index: usize,
    tokens: Arc<Vec<Token>>,
}

impl Parser {
    pub fn new(tokens: Arc<Vec<Token>>) -> Self {
        Self { index: 0, tokens }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.index)
    }

    fn peek_prev(&self) -> Option<&Token> {
        self.tokens.get(self.index - 1)
    }

    fn consume(&mut self) -> Option<&Token> {
        if self.peek().is_some() {
            self.index += 1;
            return self.peek_prev();
        }
        None
    }

    pub fn parse(&mut self) -> Result<AstNode, MCCParserError> {
        return self.expression();
    }

    fn primary_expression(&mut self) -> Result<AstNode, MCCParserError> {
        if let Some(token) = self.peek().cloned() {
            match token.kind {
                TokenKind::Identifier => {
                    self.consume();
                    return Ok(AstNode::Identifier(Identifier {
                        name: token.lexeme.clone(),
                    }));
                }

                TokenKind::StringLiteral => {
                    self.consume();
                    return Ok(AstNode::StringLiteral(StringLiteral {
                        value: token.lexeme.clone(),
                    }));
                }

                TokenKind::Constant(c) => {
                    self.consume();
                    return Ok(AstNode::Constant(Constant {
                        kind: c,
                        value: token.lexeme.clone(),
                    }));
                }

                TokenKind::Punctuator(PunctuatorKind::LParen) => {
                    let lparen = self.consume().unwrap().clone();
                    let expr = self.expression();
                    let rparen = self.consume();

                    if rparen
                        .is_some_and(|t| t.kind != TokenKind::Punctuator(PunctuatorKind::RParen))
                    {
                        return Err(MCCParserError::new(
                            format!(
                                "Expected closing parenthesis after expression at {}",
                                rparen.unwrap().location,
                            )
                            .as_str(),
                        ));
                    }

                    if rparen.is_none() {
                        return Err(MCCParserError::new(
                            format!(
                                "Opening parenthesis at {} has no matching closing parenthesis.",
                                lparen.location,
                            )
                            .as_str(),
                        ));
                    }

                    return expr;
                }

                _ => {}
            };
        }

        Err(MCCParserError::new(
            "Attempted to parse primary expression at invalid position.",
        ))
    }

    fn assignment_expression(&mut self) -> Result<AstNode, MCCParserError> {
        todo!()
    }

    fn expression(&mut self) -> Result<AstNode, MCCParserError> {
        let mut assignment_expr = self.assignment_expression().unwrap();
        if let Some(token) = self.peek() {
            if token.kind == TokenKind::Punctuator(PunctuatorKind::Comma) {
                let comma = self.consume().unwrap().clone();
                let expr = self.expression();


                // return Ok(AstNode::Expression(
                //     Expression::Multiple((), ())
                // ));

                // return Ok(AstNode::Expression(Expression::Multiple(

                // ))));
            }
        }

        match assignment_expr {
            AstNode::AssignmentExpression(assignment_expression) => {
                return Ok(AstNode::Expression(Expression::AssignmentExpression(
                    assignment_expression,
                )));
            }
            _ => {
                return Err(MCCParserError::new(
                    "Expected assignment expression at invalid position.",
                ))
            }
        }
    }
}

pub fn parse(tokens: Arc<Vec<Token>>) {
    let mut ps = Parser::new(Arc::clone(&tokens));
    let parse_res = ps.parse();
    if let Ok(node) = parse_res {
        AstNodePrinter::new().print(&node);
    } else {
        dbg!(parse_res.err());
    }
}
