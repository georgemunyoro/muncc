use crate::lexer::token::Token;


pub trait Visitable {
    fn visit<V: Visitor>(&self, visitor: &mut V);
}

pub trait Visitor : Sized {
    fn visit_identifier(&mut self, identifier: &Identifier) {
        identifier.visit(self);
    }
}

pub struct Identifier {
    token: Token
}

impl Visitable for Identifier {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        
    }
}

struct MCCParserError {}

struct Parser {
    index: usize,
    tokens: &Vec<Token>
}

impl Parser {
    fn parse(tokens) ->  {

    }
}

pub fn parse(tokens: Vec<Token>){

    let ps = Parser {
        index: 0,
    };

    println!("Hello, World!, {}", tokens.len());
}

