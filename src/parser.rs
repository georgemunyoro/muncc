use std::sync::Arc;

pub mod ast;
pub mod ast_node;
pub mod error;

use crate::{
    lexer::token::{
        self, keyword::KeywordKind, punctuator::PunctuatorKind, ConstantKind, Token, TokenKind,
    },
    parser::ast::AstNodePrinter,
};

use self::{
    ast::AstNode,
    ast_node::{
        AssignmentExpression, AssignmentOperator, CastExpression, ConditionalExpression, Constant,
        Expression, Identifier, PostfixExpression, PrimaryExpression, StringLiteral,
        UnaryExpression, UnaryOperator,
    },
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
        self.unary_expression()
    }

    pub fn type_name(&mut self) -> Result<AstNode, MCCParserError> {
        todo!("type_name")
    }

    pub fn cast_expression(&mut self) -> Result<AstNode, MCCParserError> {
        if let Some(token) = self.peek() {
            if token.kind == TokenKind::Punctuator(PunctuatorKind::LParen) {
                todo!();
            }

            if let Ok(unary_expr_node) = self.unary_expression() {
                match unary_expr_node {
                    AstNode::UnaryExpression(unary_expr) => {
                        return Ok(AstNode::CastExpression(CastExpression::Simple(Box::new(
                            unary_expr,
                        ))));
                    }
                    _ => unreachable!("Expected unary expression"),
                }
            } else {
                return Err(MCCParserError::new("Expected unary expression."));
            }
        } else {
            return Err(MCCParserError::new(
                "Expected cast expression, encountered EOF instead.",
            ));
        }
    }

    pub fn postfix_expression(&mut self) -> Result<AstNode, MCCParserError> {
        //    postfix-expression ::=
        //        primary-expression
        //        postfix-expression "[" expression "]"
        //        postfix-expression "(" argument-expression-list? ")"
        //        postfix-expression "." identifier
        //        postfix-expression "->" identifier
        //        postfix-expression "++"
        //        postfix-expression "--"
        //        "(" type-name ")" "{" initializer-list "}"
        //        "(" type-name ")" "{" initializer-list "," "}"
        let mut primary_expr = self.primary_expression().unwrap();

        match primary_expr {
            AstNode::PrimaryExpression(expr) => {
                primary_expr =
                    AstNode::PostfixExpression(PostfixExpression::PrimaryExpression(expr));
            }
            _ => {
                AstNodePrinter::new().print(&primary_expr);
                return Err(MCCParserError::new(
                    "Expected primary expression at invalid position.",
                ));
            }
        }

        loop {
            if let Some(t) = self.peek() {
                match t.kind {
                    TokenKind::Punctuator(PunctuatorKind::LBracket) => {
                        self.consume();
                        let expr = self.expression();
                        match self.expect(TokenKind::Punctuator(PunctuatorKind::RBracket), "']'") {
                            Err(e) => return Err(e),
                            _ => {}
                        }

                        match expr {
                            Ok(expr) => match expr {
                                AstNode::Expression(expr) => match primary_expr {
                                    AstNode::PostfixExpression(postfix_expr) => {
                                        primary_expr = AstNode::PostfixExpression(
                                            PostfixExpression::Array(Box::new(postfix_expr), expr),
                                        );
                                    }
                                    _ => {
                                        return Err(MCCParserError::new(
                                            "Expected postfix expression after '['.",
                                        ))
                                    }
                                },
                                _ => {
                                    return Err(MCCParserError::new(
                                        "Expected expression after '['.",
                                    ));
                                }
                            },
                            Err(e) => return Err(e),
                        }
                    }

                    TokenKind::Punctuator(PunctuatorKind::LParen) => {
                        todo!("postfix_expression LParen")
                    }

                    TokenKind::Punctuator(PunctuatorKind::Dot) => {
                        todo!("postfix_expression Dot")
                    }

                    TokenKind::Punctuator(PunctuatorKind::Deref) => {
                        todo!("postfix_expression Deref")
                    }

                    TokenKind::Punctuator(PunctuatorKind::IncrOp) => {
                        todo!("postfix_expression IncrOp")
                    }

                    TokenKind::Punctuator(PunctuatorKind::DecrOp) => {
                        todo!("postfix_expression DecrOp")
                    }

                    _ => {
                        break;
                    }
                }
            } else {
                break;
            }
        }
        return Ok(primary_expr);
    }

    pub fn unary_expression(&mut self) -> Result<AstNode, MCCParserError> {
        let token = self.peek().unwrap().clone();
        match token.kind {
            TokenKind::Punctuator(PunctuatorKind::IncrOp) => {
                self.consume();
                if let Ok(expr) = self.unary_expression() {
                    match expr {
                        AstNode::UnaryExpression(unary_expr) => {
                            return Ok(AstNode::UnaryExpression(UnaryExpression::Increment(
                                Box::new(unary_expr),
                            )));
                        }
                        _ => {
                            AstNodePrinter::new().print(&expr);
                            return Err(MCCParserError::new(
                                "Expected unary expression after increment operator.",
                            ));
                        }
                    }
                } else {
                    return Err(MCCParserError::new(
                        "Expected unary expression after increment operator.",
                    ));
                }
            }

            TokenKind::Punctuator(PunctuatorKind::DecrOp) => {
                self.consume();
                if let Ok(expr) = self.unary_expression() {
                    match expr {
                        AstNode::UnaryExpression(unary_expr) => {
                            return Ok(AstNode::UnaryExpression(UnaryExpression::Decrement(
                                Box::new(unary_expr),
                            )));
                        }
                        _ => {
                            return Err(MCCParserError::new(
                                "Expected unary expression after decrement operator.",
                            ));
                        }
                    }
                } else {
                    return Err(MCCParserError::new(
                        "Expected unary expression after decrement operator.",
                    ));
                }
            }

            TokenKind::Keyword(KeywordKind::Sizeof) => {
                self.consume();

                match self.peek() {
                    Some(token) => match token.kind {
                        TokenKind::Punctuator(PunctuatorKind::LParen) => {
                            self.consume();
                            let type_name = self.type_name().unwrap();

                            match self.expect(TokenKind::Punctuator(PunctuatorKind::RParen), "')'")
                            {
                                Err(e) => return Err(e),
                                _ => {}
                            }

                            match type_name {
                                AstNode::TypeName(type_name) => {
                                    return Ok(AstNode::UnaryExpression(
                                        UnaryExpression::SizeOfTypeName(type_name),
                                    ));
                                }
                                _ => {
                                    return Err(MCCParserError::new(
                                        "Expected type name after sizeof operator.",
                                    ));
                                }
                            }
                        }
                        _ => {
                            let unary_expr = self.unary_expression();
                            if let Ok(expr) = unary_expr {
                                match expr {
                                    AstNode::UnaryExpression(unary_expr) => {
                                        return Ok(AstNode::UnaryExpression(
                                            UnaryExpression::SizeOfUnaryExpression(Box::new(
                                                unary_expr,
                                            )),
                                        ));
                                    }
                                    _ => {
                                        return Err(MCCParserError::new(
                                            "Expected unary expression after sizeof operator.",
                                        ));
                                    }
                                }
                            } else {
                                return Err(MCCParserError::new(
                                    "Expected unary expression after sizeof operator.",
                                ));
                            }
                        }
                    },

                    None => {
                        return Err(MCCParserError::new(
                            format!(
                                "Expected expression after sizeof operator at {}.",
                                token.location
                            )
                            .as_str(),
                        ));
                    }
                }
            }

            TokenKind::Punctuator(
                PunctuatorKind::AddrBitAnd
                | PunctuatorKind::MultStar
                | PunctuatorKind::Plus
                | PunctuatorKind::Minus
                | PunctuatorKind::BitNot
                | PunctuatorKind::LogNot,
            ) => {
                let operator = match self.consume().unwrap().kind {
                    TokenKind::Punctuator(PunctuatorKind::AddrBitAnd) => UnaryOperator::And,
                    TokenKind::Punctuator(PunctuatorKind::MultStar) => UnaryOperator::Multiply,
                    TokenKind::Punctuator(PunctuatorKind::Plus) => UnaryOperator::Plus,
                    TokenKind::Punctuator(PunctuatorKind::Minus) => UnaryOperator::Minus,
                    TokenKind::Punctuator(PunctuatorKind::BitNot) => UnaryOperator::BitwiseNot,
                    TokenKind::Punctuator(PunctuatorKind::LogNot) => UnaryOperator::LogicalNot,
                    _ => unreachable!(),
                };

                match self.cast_expression().unwrap() {
                    AstNode::CastExpression(cast_expr) => {
                        return Ok(AstNode::UnaryExpression(UnaryExpression::UnaryOperator(
                            operator, cast_expr,
                        )));
                    }
                    _ => {
                        return Err(MCCParserError::new(
                            "Expected cast expression after unary operator.",
                        ));
                    }
                }
            }

            _ => {
                let parsed_res = self.postfix_expression();
                if let Ok(postfix_expr) = parsed_res {
                    match postfix_expr {
                        AstNode::PostfixExpression(postfix_expr) => {
                            return Ok(AstNode::UnaryExpression(
                                UnaryExpression::PostfixExpression(postfix_expr),
                            ))
                        }

                        _ => return Err(MCCParserError::new("Expected postfix expression.")),
                    }
                } else {
                    return parsed_res;
                }
            }
        }
    }

    fn expect(
        &mut self,
        expected_kind: TokenKind,
        expected_name: &str,
    ) -> Result<(), MCCParserError> {
        let token = self.consume();
        if token.is_some_and(|t| t.kind == expected_kind) {
            return Ok(());
        }
        Err(MCCParserError::new(
            format!("{} at {}", expected_name, token.unwrap().location,).as_str(),
        ))
    }

    fn primary_expression(&mut self) -> Result<AstNode, MCCParserError> {
        if let Some(token) = self.peek().cloned() {
            match token.kind {
                TokenKind::Identifier => {
                    self.consume();

                    return Ok(AstNode::PrimaryExpression(PrimaryExpression::Identifier(
                        Identifier {
                            name: token.lexeme.clone(),
                        },
                    )));
                }

                TokenKind::StringLiteral => {
                    self.consume();
                    return Ok(AstNode::PrimaryExpression(
                        PrimaryExpression::StringLiteral(StringLiteral {
                            value: token.lexeme.clone(),
                        }),
                    ));
                }

                TokenKind::Constant(c) => {
                    self.consume();
                    return Ok(AstNode::PrimaryExpression(PrimaryExpression::Constant(
                        Constant {
                            kind: c,
                            value: token.lexeme.clone(),
                        },
                    )));
                }

                TokenKind::Punctuator(PunctuatorKind::LParen) => {
                    self.consume();
                    let expr = self.expression();

                    match self.expect(TokenKind::Punctuator(PunctuatorKind::RParen), "')'") {
                        Err(e) => return Err(e),
                        _ => {}
                    }

                    if expr.is_err() {
                        return Err(MCCParserError::new(
                            "Expected expression after '(' at invalid position.",
                        ));
                    }

                    match expr.unwrap() {
                        AstNode::Expression(e) => {
                            return Ok(AstNode::PrimaryExpression(PrimaryExpression::Expression(
                                Box::new(e),
                            )));
                        }
                        _ => {
                            return Err(MCCParserError::new(
                                "Expected expression after '(' at invalid position.",
                            ));
                        }
                    }
                }

                _ => {}
            };
        }

        Err(MCCParserError::new(
            "Attempted to parse primary expression at invalid position.",
        ))
    }

    fn conditional_expression(&mut self) -> Result<AstNode, MCCParserError> {
        return self.unary_expression();
    }

    fn assignment_expression(&mut self) -> Result<AstNode, MCCParserError> {
        let start_index = self.index;

        if let Ok(conditional_expr) = self.conditional_expression() {
            match conditional_expr {
                AstNode::ConditionalExpression(conditional_expr) => {
                    return Ok(AstNode::AssignmentExpression(
                        AssignmentExpression::ConditionalExpression(conditional_expr),
                    ));
                }
                _ => {
                    return Err(MCCParserError::new(
                        "Expected conditional expression at invalid position.",
                    ))
                }
            }
        } else {
            self.index = start_index;

            if let Ok(unary_expr) = self.unary_expression() {
                match unary_expr {
                    AstNode::UnaryExpression(unary_expr) => {
                        if let Some(token) = self.peek() {
                            match token.kind {
                                TokenKind::Punctuator(
                                    PunctuatorKind::Assign
                                    | PunctuatorKind::DivAssign
                                    | PunctuatorKind::MulAssign
                                    | PunctuatorKind::RemAssign
                                    | PunctuatorKind::PlusAssign
                                    | PunctuatorKind::MinusAssign
                                    | PunctuatorKind::BitOrAssign
                                    | PunctuatorKind::BitAndAssign
                                    | PunctuatorKind::BitXorAssign
                                    | PunctuatorKind::LShiftAssign
                                    | PunctuatorKind::RShiftAssign,
                                ) => {
                                    let operator = AssignmentOperator::Assign;
                                    self.consume();
                                    let assignment_expr = self.assignment_expression();

                                    match assignment_expr {
                                        Ok(assignment_expr) => match assignment_expr {
                                            AstNode::AssignmentExpression(assignment_expr) => {
                                                return Ok(AstNode::AssignmentExpression(
                                                    AssignmentExpression::UnaryAssignment {
                                                        unary_expression: Box::new(unary_expr),
                                                        operator,
                                                        rhs: Box::new(assignment_expr),
                                                    },
                                                ))
                                            },
                                            _ => {
                                                return Err(MCCParserError::new(
                                                    "Expected assignment expression at invalid position.",
                                                ))
                                            }
                                        },
                                        Err(e) => return Err(e),
                                    }
                                }
                                _ => {
                                    return Err(MCCParserError::new(
                                        "Expected assignment operator at invalid position.",
                                    ))
                                }
                            }
                        }
                        return Err(MCCParserError::new(
                            "Expected assignment operator at invalid position.",
                        ));
                    }
                    _ => {
                        return Err(MCCParserError::new(
                            "Expected unary expression at invalid position.",
                        ))
                    }
                }
            } else {
                return Err(MCCParserError::new(
                    "Expected unary or conditional expression at invalid position.",
                ));
            }
        }
    }

    fn expression(&mut self) -> Result<AstNode, MCCParserError> {
        let mut assignment_expr = self.assignment_expression().unwrap();

        AstNodePrinter::new().print(&assignment_expr);

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
