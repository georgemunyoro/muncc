use std::sync::Arc;

use super::{
    error::{TokenizationError, TokenizationErrorKind},
    token::{
        keyword::{IsKeyword, KeywordKind},
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

            let token = if cc.is_alphabetic() {
                // For identifiers we check if alphabetic instead of numeric
                // to ensure the first character is never a number.
                self.identifier()
            } else if cc.is_digit(10) {
                self.number()
            } else if cc == '"' {
                self.string()
            } else if cc == '\'' {
                self.char()
            } else if cc.is_punctuator() {
                self.punctuator()
            } else {
                Err(TokenizationError {
                    kind: TokenizationErrorKind::UnexpectedToken(String::from(cc)),
                    source: Arc::clone(&self.source),
                    location: self.location.clone(),
                })
            };

            match token {
                Err(e) => return Err(e),
                Ok(tk) => tokens.push(tk),
            }
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

        let token_kind = if lexeme.clone().is_keyword() {
            TokenKind::Keyword(KeywordKind::from(lexeme.clone()))
        } else {
            TokenKind::Identifier
        };

        return Ok(Token::new(token_kind, lexeme, id_location));
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
            source: Arc::clone(&self.source),
            location: self.location.clone(),
        });
    }

    fn char(&mut self) -> Result<Token, TokenizationError> {
        let c_location = self.location.clone();
        debug_assert_eq!(*self.consume().unwrap(), '\'');

        let mut lexeme = String::new();
        loop {
            if let Some(&cc) = self.consume() {
                if cc == '\'' {
                    break;
                }
                lexeme.push(cc);
            } else {
                return Err(TokenizationError {
                    kind: TokenizationErrorKind::UnterminatedChar,
                    source: Arc::clone(&self.source),
                    location: c_location.clone(),
                })
            }
        }

        Ok(Token::new(TokenKind::Constant(ConstantKind::Char), lexeme, c_location))

    }

    fn string(&mut self) -> Result<Token, TokenizationError> {
        let s_location = self.location.clone();
        debug_assert_eq!(*self.consume().unwrap(), '\"');

        let mut lexeme = String::new();
        loop {
            if let Some(&cc) = self.consume() {
                if cc == '"' {
                    break;
                }
                lexeme.push(cc);
            } else {
                return Err(TokenizationError {
                    kind: TokenizationErrorKind::UnterminatedString,
                    source: Arc::clone(&self.source),
                    location: s_location.clone(),
                })
            }
        }

        Ok(Token::new(TokenKind::StringLiteral, lexeme, s_location))
    }
}
