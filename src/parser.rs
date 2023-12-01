use std::sync::Arc;

pub mod ast;
pub mod ast_node;
pub mod error;

use crate::lexer::token::{Token, TokenKind};

use self::{ast::AstNode, ast_node::AstIdentifier, error::MCCParserError};

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
        let nodes: Vec<Box<AstNode>> = Vec::new();
        while let Some(t) = self.peek() {
            match t.kind {
                TokenKind::Identifier => return self.identifier(),
                _ => todo!(),
            }
        }
        Err(MCCParserError::new("Unknown error ocurred!"))
    }

    fn identifier(&mut self) -> Result<AstNode, MCCParserError> {
        if let Some(token) = self.consume() {
            return Ok(AstNode::Identifier(AstIdentifier::new(token.clone())));
        }
        Err(MCCParserError::new(
            "Attempted to parse identifier at invalid position.",
        ))
    }
}

pub fn parse(tokens: Arc<Vec<Token>>) {
    let mut ps = Parser::new(Arc::clone(&tokens));
    ps.parse();

    println!("Hello, World!, {}", tokens.len());
}
