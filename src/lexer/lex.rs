use std::sync::Arc;

use super::{
    error::{TokenizationError, TokenizationErrorKind},
    token::{
        punctuator::{IsPunctuator, PunctuatorKind},
        ConstantKind, Location, Token, TokenKind,
    },
};

pub struct Lexer {
    index: usize,
    source: Arc<String>,
    source_chars: Vec<char>,
    location: Location,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Self {
            source: Arc::new(String::from(source)),
            source_chars: source.chars().collect(),
            index: 0,
            location: Location { line: 1, column: 1 },
        }
    }

    fn peek(&self) -> Option<&char> {
        self.source_chars.get(self.index)
    }

    fn peek_prev(&self) -> Option<&char> {
        self.source_chars.get(self.index - 1)
    }

    fn consume(&mut self) -> Option<&char> {
        if let Some(&cc) = self.peek() {
            self.location.column += 1;
            if cc == '\n' {
                self.location.line += 1;
                self.location.column = 1;
            }
            self.index += 1;
            return self.peek_prev();
        }
        None
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, TokenizationError> {
        let mut tokens: Vec<Token> = Vec::new();

        while let Some(&cc) = self.peek() {
            if cc.is_whitespace() {
                self.consume();
                continue;
            }

            // For identifiers we check if alphabetic instead of numeric
            // to ensure the first character is never a number.
            if cc.is_alphabetic() {
                match self.identifier() {
                    Err(e) => return Err(e),
                    Ok(token) => tokens.push(token),
                }
                continue;
            }

            if cc.is_digit(10) {
                match self.number() {
                    Err(e) => return Err(e),
                    Ok(token) => tokens.push(token),
                }
                continue;
            }

            if cc.is_punctuator() {
                match self.punctuator() {
                    Err(e) => return Err(e),
                    Ok(token) => tokens.push(token),
                }
                continue;
            }

            return Err(TokenizationError {
                kind: TokenizationErrorKind::UnexpectedToken(String::from(cc)),
            });
        }
        return Ok(tokens);
    }

    fn identifier(&mut self) -> Result<Token, TokenizationError> {
        let id_location = self.location.clone();

        let mut lexeme = String::new();
        while let Some(&cc) = self.peek() {
            if !cc.is_alphanumeric() {
                break;
            }
            lexeme.push(*self.consume().unwrap());
        }



        return Ok(Token::new(TokenKind::Identifier, lexeme, id_location));
    }

    fn number(&mut self) -> Result<Token, TokenizationError> {
        let num_location = self.location.clone();

        let mut lexeme = String::new();
        while let Some(&cc) = self.peek() {
            if !cc.is_digit(10) {
                break;
            }
            lexeme.push(*self.consume().unwrap());
        }

        return Ok(Token::new(
            TokenKind::Constant(ConstantKind::Number),
            lexeme,
            num_location,
        ));
    }

    fn punctuator(&mut self) -> Result<Token, TokenizationError> {
        let p_location = self.location.clone();
        let mut lexeme = String::new();
        while let Some(&cc) = self.peek() {
            if !format!("{lexeme}{cc}").is_punctuator() {
                break;
            }
            lexeme.push(*self.consume().unwrap());
        }

        if !lexeme.is_empty() {
            return Ok(Token::new(
                TokenKind::Punctuator(PunctuatorKind::from(lexeme.clone())),
                lexeme,
                p_location,
            ));
        }

        return Err(TokenizationError {
            kind: TokenizationErrorKind::ExpectedToken("<non-empty>".to_owned()),
        });
    }
}
