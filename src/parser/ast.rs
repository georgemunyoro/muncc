use super::ast_node::AstIdentifier;

pub enum AstNode {
    Identifier(AstIdentifier),
}

impl AstNode {
    fn visit<V: AstNodeVisitor>(&self, visitor: &mut V) {
        match self {
            Self::Identifier(identifier) => visitor.visit_identifier(identifier),
        }
    }
}

pub trait AstNodeVisitor: Sized {
    fn visit_identifier(&mut self, identifier: &AstIdentifier);
}

