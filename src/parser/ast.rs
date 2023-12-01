use std::ops::Deref;

use super::ast_node::{
    AssignmentExpression, ConditionalExpression, Constant, Expression, ExternalDeclaration,
    Identifier, LogicalAndExpression, LogicalOrExpression, StringLiteral, TranslationUnit,
    UnaryExpression,
};

pub enum AstNode {
    Identifier(Identifier),
    StringLiteral(StringLiteral),
    TranslationUnit(TranslationUnit),
    ExternalDeclaration(ExternalDeclaration),
    Constant(Constant),
    Expression(Expression),
    AssignmentExpression(AssignmentExpression),
    UnaryExpression(UnaryExpression),
    ConditionalExpression(ConditionalExpression),
    LogicalOrExpression(LogicalOrExpression),
    LogicalAndExpression(LogicalAndExpression),
}

impl AstNode {
    fn visit<V: AstNodeVisitor>(&self, visitor: &mut V) {
        match self {
            Self::Identifier(identifier) => visitor.visit_identifier(identifier),
            Self::StringLiteral(string_literal) => visitor.visit_string_literal(string_literal),
            Self::TranslationUnit(translation_unit) => {
                visitor.visit_translation_unit(translation_unit)
            }
            Self::ExternalDeclaration(external_declaration) => {
                visitor.visit_external_declaration(external_declaration)
            }
            Self::Constant(constant) => visitor.visit_constant(constant),
            Self::Expression(expression) => visitor.visit_expression(expression),
            Self::AssignmentExpression(assignment_expression) => {
                visitor.visit_assignment_expression(assignment_expression)
            }
            Self::UnaryExpression(unary_expression) => {
                visitor.visit_unary_expression(unary_expression)
            }
            Self::ConditionalExpression(conditional_expression) => {
                visitor.visit_conditional_expression(conditional_expression)
            }
            Self::LogicalOrExpression(logical_or_expression) => {
                visitor.visit_logical_or_expression(logical_or_expression)
            }
            Self::LogicalAndExpression(logical_and_expression) => {
                visitor.visit_logical_and_expression(logical_and_expression)
            }
        }
    }
}

pub trait AstNodeVisitor: Sized {
    fn visit_identifier(&mut self, identifier: &Identifier);
    fn visit_string_literal(&mut self, string_literal: &StringLiteral);
    fn visit_translation_unit(&mut self, translation_unit: &TranslationUnit);
    fn visit_external_declaration(&mut self, external_declaration: &ExternalDeclaration);
    fn visit_constant(&mut self, constant: &Constant);
    fn visit_expression(&mut self, expression: &Expression);
    fn visit_assignment_expression(&mut self, assignment_expression: &AssignmentExpression);
    fn visit_unary_expression(&mut self, unary_expression: &UnaryExpression);
    fn visit_conditional_expression(&mut self, conditional_expression: &ConditionalExpression);
    fn visit_logical_or_expression(&mut self, logical_or_expression: &LogicalOrExpression);
    fn visit_logical_and_expression(&mut self, logical_and_expression: &LogicalAndExpression);
}

pub struct AstNodePrinter {
    indent: usize,
}

impl AstNodePrinter {
    pub fn new() -> Self {
        Self { indent: 0 }
    }

    pub fn print(&mut self, node: &AstNode) {
        node.visit(self);
    }

    fn print_indent(&self) {
        for _ in 0..self.indent {
            print!("  ");
        }
    }
}

impl AstNodeVisitor for AstNodePrinter {
    fn visit_identifier(&mut self, identifier: &Identifier) {
        self.print_indent();
        println!("Identifier: {}", identifier.name);
    }

    fn visit_string_literal(&mut self, string_literal: &StringLiteral) {
        self.print_indent();
        println!("StringLiteral: {}", string_literal.value);
    }

    fn visit_external_declaration(&mut self, external_declaration: &ExternalDeclaration) {}

    fn visit_translation_unit(&mut self, translation_unit: &TranslationUnit) {
        self.print_indent();
        println!("TranslationUnit:");
        self.indent += 1;

        match translation_unit {
            TranslationUnit::ExternalDeclaration(external_declaration) => {
                self.visit_external_declaration(external_declaration);
            }
            TranslationUnit::TranslationUnit(translation_unit_inner, external_declaration) => {
                self.visit_translation_unit(translation_unit_inner);
                self.visit_external_declaration(external_declaration);
            }
        }

        self.indent -= 1;
    }

    fn visit_constant(&mut self, constant: &Constant) {
        self.print_indent();
        println!(
            "Constant: {:?} {}",
            constant.kind.clone(),
            constant.value.clone()
        );
    }

    fn visit_assignment_expression(&mut self, assignment_expression: &AssignmentExpression) {
        self.print_indent();
        println!("AssignmentExpression:");
        self.indent += 1;

        match assignment_expression {
            AssignmentExpression::UnaryAssignment {
                unary_expression,
                operator,
                rhs,
            } => {
                self.print_indent();
                println!("UnaryAssignment:");
                self.indent += 1;

                self.print_indent();
                println!("UnaryExpression:");
                self.indent += 1;

                self.visit_unary_expression(unary_expression);

                self.indent -= 1;

                self.print_indent();
                println!("Operator: {:?}", operator);

                self.print_indent();
                println!("Rhs:");
                self.indent += 1;

                self.visit_assignment_expression(rhs.deref());

                self.indent -= 1;
            }

            AssignmentExpression::ConditionalExpression(conditional_expression) => {
                self.visit_conditional_expression(conditional_expression);
            }
        }

        self.indent -= 1;
    }

    fn visit_expression(&mut self, expression: &Expression) {
        self.print_indent();
        println!("Expression:");
        self.indent += 1;

        match expression {
            Expression::AssignmentExpression(assignment_expression) => {
                self.visit_assignment_expression(assignment_expression);
            }
            Expression::Multiple(expression, assignment_expression) => {
                self.visit_expression(expression);
                self.visit_assignment_expression(assignment_expression);
            }
        }

        self.indent -= 1;
    }

    fn visit_unary_expression(&mut self, unary_expression: &UnaryExpression) {
        self.print_indent();
        println!("UnaryExpression:");
        self.indent += 1;

        match unary_expression {
            _ => todo!(),
        }

        self.indent -= 1;
    }

    fn visit_conditional_expression(&mut self, conditional_expression: &ConditionalExpression) {
        self.print_indent();
        println!("ConditionalExpression:");
        self.indent += 1;

        match conditional_expression {
            ConditionalExpression::TernaryExpression {
                condition,
                true_expr,
                false_expr,
            } => {
                self.print_indent();
                println!("Condition:");
                self.indent += 1;

                self.visit_logical_or_expression(condition);

                self.indent -= 1;

                self.print_indent();
                println!("TrueExpr:");
                self.indent += 1;

                self.visit_expression(true_expr);

                self.indent -= 1;

                self.print_indent();
                println!("FalseExpr:");
                self.indent += 1;

                self.visit_conditional_expression(false_expr);

                self.indent -= 1;
            }
            ConditionalExpression::LogicalOrExpression(logical_or_expression) => {
                self.visit_logical_or_expression(logical_or_expression);
            }
        }

        self.indent -= 1;
    }

    fn visit_logical_or_expression(&mut self, logical_or_expression: &LogicalOrExpression) {
        self.print_indent();
        println!("LogicalOrExpression:");
        self.indent += 1;

        match logical_or_expression {
            LogicalOrExpression::Or(logical_or_inner, logical_and_inner) => {
                self.visit_logical_or_expression(&logical_or_inner);
                self.visit_logical_and_expression(&logical_and_inner);
            }
            LogicalOrExpression::Simple(logical_and_inner) => {
                self.visit_logical_and_expression(&logical_and_inner);
            }
        }

        self.indent -= 1;
    }

    fn visit_logical_and_expression(&mut self, logical_and_expression: &LogicalAndExpression) {
        self.print_indent();
        println!("LogicalAndExpression:");
        self.indent += 1;

        match logical_and_expression {
            _ => todo!(),
        }

        self.indent -= 1;
    }
}
