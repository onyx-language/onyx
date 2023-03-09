use crate::{
    tokens::{Token, TokenKind},
    span::Span,
    error::OnyxError
};
#[derive(Debug, Clone)]
pub struct OnyxLexer {
    file_name: String,
    content: Result<String, OnyxError>,
    index: usize,
    line: usize,
    start: usize,
    end: usize,
}
impl OnyxLexer {
    pub fn new(file_name: String) -> Result<OnyxLexer, OnyxError> {
        Ok(OnyxLexer {
            file_name: file_name.clone(),
            content: match std::fs::read_to_string(file_name.clone()) {
                Ok(contents) => Ok(contents),
                Err(_) => return Err(OnyxError::IOError(std::rc::Rc::new(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    format!("file '{}' not found", file_name.clone())
                )))),
            },
            index: 0,
            line: 1,
            start: 0,
            end: 0,
        })
    }
    pub fn lex(&mut self) -> Result<Vec<Token>, Vec<OnyxError>> {
        let mut tokens: Vec<Token> = vec![];
        let mut errors: Vec<OnyxError> = vec![];
        let binding = self.clone();
        let content = binding.content.as_ref().unwrap();
        while self.index < content.len() {
            match self.content.as_ref().unwrap().chars().nth(self.index).unwrap() {
                ' ' => {
                    self.index += 1;
                    self.start += 1;
                    self.end += 1;
                }
                '\t' => {
                    self.index += 1;
                    self.start += 4;
                    self.end += 4;
                }
                '\n' => {
                    self.index += 1;
                    self.line += 1;
                    self.end += 1;
                    self.start = self.end;
                }
                '\r' => {
                    self.index += 1;
                    self.start += 1;
                    self.end += 1;
                }
                '0'..='9' => {
                    let mut number = String::new();
                    while self.content.as_ref().unwrap().chars().nth(self.index).unwrap().is_numeric() ||
                            self.content.as_ref().unwrap().chars().nth(self.index).unwrap() == '.' {
                        number.push(self.content.as_ref().unwrap().chars().nth(self.index).unwrap());
                        self.index += 1;
                        self.end += 1;
                    }
                    self.start += 1;

                    if number.contains('.') {
                        tokens.push(Token::new(
                            TokenKind::Float(number.parse::<f64>().unwrap()),
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                    } else {
                        tokens.push(Token::new(
                            TokenKind::Number(number.parse::<i64>().unwrap()),
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                    }
                    self.start = self.end;
                }
                'a'..='z' | 'A'..='Z' | '_' => {
                    let mut identifier = String::new();
                    while self.content.as_ref().unwrap().chars().nth(self.index).unwrap().is_alphanumeric() || self.content.as_ref().unwrap().chars().nth(self.index).unwrap() == '_' {
                        identifier.push(self.content.as_ref().unwrap().chars().nth(self.index).unwrap());
                        self.index += 1;
                        self.end += 1;
                    }
                    tokens.push(Token::new(
                        match identifier.as_str() {
                            "true" => TokenKind::Bool(true),
                            "false" => TokenKind::Bool(false),
                            "boxed" => TokenKind::Boxed,
                            "class" => TokenKind::Class,
                            "const" => TokenKind::Const,
                            "defer" => TokenKind::Defer,
                            "enum" => TokenKind::Enum,
                            "function" => TokenKind::Function,
                            "match" => TokenKind::Match,
                            "named" => TokenKind::Named,
                            "new" => TokenKind::New,
                            "null" => TokenKind::Null,
                            "override" => TokenKind::Override,
                            "private" => TokenKind::Private,
                            "protected" => TokenKind::Protected,
                            "public" => TokenKind::Public,
                            "raw" => TokenKind::Raw,
                            "return" => TokenKind::Return,
                            "sizeof" => TokenKind::Sizeof,
                            "var" => TokenKind::Var,
                            "virtual" => TokenKind::Virtual,
                            "weak" => TokenKind::Weak,
                            _ => TokenKind::Identifier(identifier),
                        },
                        Span::new(self.file_name.clone(), self.start, self.end),
                    ));
                    self.start = self.end;
                }
                '"' => {
                    let mut string = String::new();
                    self.index += 1;
                    self.end += 1;
                    while self.content.as_ref().unwrap().chars().nth(self.index).unwrap() != '"' {
                        string.push(self.content.as_ref().unwrap().chars().nth(self.index).unwrap());
                        self.index += 1;
                        self.end += 1;
                    }
                    self.index += 1;
                    self.end += 1;
                    tokens.push(Token::new(
                        TokenKind::String(string),
                        Span::new(self.file_name.clone(), self.start, self.end),
                    ));
                    self.start = self.end;
                }
                '\'' => {
                    let mut character = String::new();
                    self.index += 1;
                    self.end += 1;
                    while self.content.as_ref().unwrap().chars().nth(self.index).unwrap() != '\'' {
                        character.push(self.content.as_ref().unwrap().chars().nth(self.index).unwrap());
                        self.index += 1;
                        self.end += 1;
                    }
                    self.index += 1;
                    self.end += 1;
                    tokens.push(Token::new(
                        TokenKind::Char(character.chars().nth(0).unwrap()),
                        Span::new(self.file_name.clone(), self.start, self.end),
                    ));
                    self.start = self.end;
                }
                '(' => {
                    self.index += 1;
                    self.end += 1;
                    tokens.push(Token::new(
                        TokenKind::LeftParen,
                        Span::new(self.file_name.clone(), self.start, self.end),
                    ));
                    self.start += 1;
                }
                ')' => {
                    self.index += 1;
                    self.end += 1;
                    tokens.push(Token::new(
                        TokenKind::RightParen,
                        Span::new(self.file_name.clone(), self.start, self.end),
                    ));
                    self.start += 1;
                }
                '{' => {
                    self.index += 1;
                    self.end += 1;
                    tokens.push(Token::new(
                        TokenKind::LeftBrace,
                        Span::new(self.file_name.clone(), self.start, self.end),
                    ));
                    self.start += 1;
                }
                '}' => {
                    self.index += 1;
                    self.end += 1;
                    tokens.push(Token::new(
                        TokenKind::RightBrace,
                        Span::new(self.file_name.clone(), self.start, self.end),
                    ));
                    self.start += 1;
                }
                '[' => {
                    self.index += 1;
                    self.end += 1;
                    tokens.push(Token::new(
                        TokenKind::LeftBracket,
                        Span::new(self.file_name.clone(), self.start, self.end),
                    ));
                    self.start += 1;
                }
                ']' => {
                    self.index += 1;
                    self.end += 1;
                    tokens.push(Token::new(
                        TokenKind::RightBracket,
                        Span::new(self.file_name.clone(), self.start, self.end),
                    ));
                    self.start += 1;
                }
                '<' => {
                    self.index += 1;
                    self.end += 1;
                    if self.content.as_ref().unwrap().chars().nth(self.index).unwrap() == '=' {
                        self.index += 1;
                        self.end += 1;
                        tokens.push(Token::new(
                            TokenKind::LessThanOrEqual,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 2;
                    } else if self.content.as_ref().unwrap().chars().nth(self.index).unwrap() == '<' {
                        self.index += 1;
                        self.end += 1;
                        if self.content.as_ref().unwrap().chars().nth(self.index).unwrap() == '=' {
                            self.index += 1;
                            self.end += 1;
                            tokens.push(Token::new(
                                TokenKind::BitwiseLeftShiftEqual,
                                Span::new(self.file_name.clone(), self.start, self.end),
                            ));
                            self.start += 3;
                        } else {
                            tokens.push(Token::new(
                                TokenKind::BitwiseLeftShift,
                                Span::new(self.file_name.clone(), self.start, self.end),
                            ));
                            self.start += 2;
                        }
                    } else {
                        tokens.push(Token::new(
                            TokenKind::LessThan,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 1;
                    }
                }
                '>' => {
                    self.index += 1;
                    self.end += 1;
                    if self.content.as_ref().unwrap().chars().nth(self.index).unwrap() == '=' {
                        self.index += 1;
                        self.end += 1;
                        tokens.push(Token::new(
                            TokenKind::GreaterThanOrEqual,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 2;
                    } else if self.content.as_ref().unwrap().chars().nth(self.index).unwrap() == '>' {
                        self.index += 1;
                        self.end += 1;
                        if self.content.as_ref().unwrap().chars().nth(self.index).unwrap() == '=' {
                            self.index += 1;
                            self.end += 1;
                            tokens.push(Token::new(
                                TokenKind::BitwiseRightShiftEqual,
                                Span::new(self.file_name.clone(), self.start, self.end),
                            ));
                            self.start += 3;
                        } else {
                            tokens.push(Token::new(
                                TokenKind::BitwiseRightShift,
                                Span::new(self.file_name.clone(), self.start, self.end),
                            ));
                            self.start += 2;
                        }
                    } else {
                        tokens.push(Token::new(
                            TokenKind::GreaterThan,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 1;
                    }
                }
                ':' => {
                    self.index += 1;
                    self.end += 1;
                    if self.content.as_ref().unwrap().chars().nth(self.index).unwrap() == ':' {
                        self.index += 1;
                        self.end += 1;
                        tokens.push(Token::new(
                            TokenKind::StaticAccess,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 2;
                    } else {
                        tokens.push(Token::new(
                            TokenKind::Colon,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 1;
                    }
                }
                ';' => {
                    self.index += 1;
                    self.end += 1;
                    tokens.push(Token::new(
                        TokenKind::Semicolon,
                        Span::new(self.file_name.clone(), self.start, self.end),
                    ));
                    self.start += 1;
                }
                '.' => {
                    self.index += 1;
                    self.end += 1;
                    if self.content.as_ref().unwrap().chars().nth(self.index).unwrap().is_numeric() {
                        let mut number = String::new();
                        number.push('0');
                        number.push('.');
                        while self.content.as_ref().unwrap().chars().nth(self.index).unwrap().is_numeric() {
                            number.push(self.content.as_ref().unwrap().chars().nth(self.index).unwrap());
                            self.index += 1;
                            self.end += 1;
                        }
                        tokens.push(Token::new(
                            TokenKind::Float(number.parse::<f64>().unwrap()),
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start = self.end;
                        continue;
                    }
                    if self.content.as_ref().unwrap().chars().nth(self.index).unwrap() == '.' {
                        self.index += 1;
                        self.end += 1;
                        if self.content.as_ref().unwrap().chars().nth(self.index).unwrap() == '=' {
                            self.index += 1;
                            self.end += 1;
                            tokens.push(Token::new(
                                TokenKind::RangeInclusive,
                                Span::new(self.file_name.clone(), self.start, self.end),
                            ));
                            self.start += 3;
                        } else if self.content.as_ref().unwrap().chars().nth(self.index).unwrap() == '<' {
                            self.index += 1;
                            self.end += 1;
                            if self.content.as_ref().unwrap().chars().nth(self.index).unwrap() == '=' {
                                self.index += 1;
                                self.end += 1;
                                tokens.push(Token::new(
                                    TokenKind::RangeToInclusive,
                                    Span::new(self.file_name.clone(), self.start, self.end),
                                ));
                                self.start += 4;
                            } else {
                                tokens.push(Token::new(
                                    TokenKind::RangeTo,
                                    Span::new(self.file_name.clone(), self.start, self.end),
                                ));
                                self.start += 3;
                            }
                        } else {
                            tokens.push(Token::new(
                                TokenKind::Range,
                                Span::new(self.file_name.clone(), self.start, self.end),
                            ));
                            self.start += 2;
                        }
                    }
                    tokens.push(Token::new(
                        TokenKind::Dot,
                        Span::new(self.file_name.clone(), self.start, self.end),
                    ));
                    self.start += 1;
                }
                ',' => {
                    self.index += 1;
                    self.end += 1;
                    tokens.push(Token::new(
                        TokenKind::Comma,
                        Span::new(self.file_name.clone(), self.start, self.end),
                    ));
                    self.start += 1;
                }
                '?' => {
                    self.index += 1;
                    self.end += 1;
                    tokens.push(Token::new(
                        TokenKind::QuestionMark,
                        Span::new(self.file_name.clone(), self.start, self.end),
                    ));
                    self.start += 1;
                }
                '+' => {
                    self.index += 1;
                    self.end += 1;
                    if self.content.as_ref().unwrap().chars().nth(self.index).unwrap() == '=' {
                        self.index += 1;
                        self.end += 1;
                        tokens.push(Token::new(
                            TokenKind::PlusEqual,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 2;
                    } else if self.content.as_ref().unwrap().chars().nth(self.index).unwrap() == '+' {
                        self.index += 1;
                        self.end += 1;
                        tokens.push(Token::new(
                            TokenKind::Increment,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 2;
                    } else {
                        tokens.push(Token::new(
                            TokenKind::Plus,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 1;
                    }
                }
                '-' => {
                    self.index += 1;
                    self.end += 1;
                    if self.content.as_ref().unwrap().chars().nth(self.index).unwrap() == '>' {
                        self.index += 1;
                        self.end += 1;
                        tokens.push(Token::new(
                            TokenKind::Arrow,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 1;
                    } else if self.content.as_ref().unwrap().chars().nth(self.index).unwrap() == '=' {
                        self.index += 1;
                        self.end += 1;
                        tokens.push(Token::new(
                            TokenKind::MinusEqual,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 2;
                    } else if self.content.as_ref().unwrap().chars().nth(self.index).unwrap() == '-' {
                        self.index += 1;
                        self.end += 1;
                        tokens.push(Token::new(
                            TokenKind::Decrement,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 2;
                    } else {
                        tokens.push(Token::new(
                            TokenKind::Minus,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 1;
                    }
                }
                '*' => {
                    self.index += 1;
                    self.end += 1;
                    if self.content.as_ref().unwrap().chars().nth(self.index).unwrap() == '=' {
                        self.index += 1;
                        self.end += 1;
                        tokens.push(Token::new(
                            TokenKind::StarEqual,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 2;
                    } else {
                        tokens.push(Token::new(
                            TokenKind::Star,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 1;
                    }
                }
                '/' => {
                    self.index += 1;
                    self.end += 1;
                    if self.content.as_ref().unwrap().chars().nth(self.index).unwrap() == '/' {
                        self.index += 1;
                        self.end += 1;
                        while self.content.as_ref().unwrap().chars().nth(self.index).unwrap() != '\n' {
                            self.index += 1;
                            self.end += 1;
                        }
                        self.index += 1;
                        self.end += 1;
                        self.start = self.end;
                    } else if self.content.as_ref().unwrap().chars().nth(self.index).unwrap() == '=' {
                        self.index += 1;
                        self.end += 1;
                        tokens.push(Token::new(
                            TokenKind::SlashEqual,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 2;
                    } else {
                        tokens.push(Token::new(
                            TokenKind::Slash,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 1;
                    }
                }
                '%' => {
                    self.index += 1;
                    self.end += 1;
                    if self.content.as_ref().unwrap().chars().nth(self.index).unwrap() == '=' {
                        self.index += 1;
                        self.end += 1;
                        tokens.push(Token::new(
                            TokenKind::PercentEqual,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 2;
                    } else {
                        tokens.push(Token::new(
                            TokenKind::Percent,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 1;
                    }
                }
                '^' => {
                    self.index += 1;
                    self.end += 1;
                    if self.content.as_ref().unwrap().chars().nth(self.index).unwrap() == '=' {
                        self.index += 1;
                        self.end += 1;
                        tokens.push(Token::new(
                            TokenKind::CaretEqual,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 2;
                    } else {
                        tokens.push(Token::new(
                            TokenKind::Caret,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 1;
                    }
                }
                '&' => {
                    self.index += 1;
                    self.end += 1;
                    if self.content.as_ref().unwrap().chars().nth(self.index).unwrap() == '&' {
                        self.index += 1;
                        self.end += 1;
                        tokens.push(Token::new(
                            TokenKind::And,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 2;
                    } else if self.content.as_ref().unwrap().chars().nth(self.index).unwrap() == '=' {
                        self.index += 1;
                        self.end += 1;
                        tokens.push(Token::new(
                            TokenKind::BitwiseAndEqual,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 2;
                    } else {
                        tokens.push(Token::new(
                            TokenKind::BitwiseAnd,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 1;
                    }
                }
                '|' => {
                    self.index += 1;
                    self.end += 1;
                    if self.content.as_ref().unwrap().chars().nth(self.index).unwrap() == '|' {
                        self.index += 1;
                        self.end += 1;
                        tokens.push(Token::new(
                            TokenKind::Or,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 2;
                    } else if self.content.as_ref().unwrap().chars().nth(self.index).unwrap() == '=' {
                        self.index += 1;
                        self.end += 1;
                        tokens.push(Token::new(
                            TokenKind::BitwiseOrEqual,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 2;
                    } else {
                        tokens.push(Token::new(
                            TokenKind::BitwiseOr,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 1;
                    }
                }
                '=' => {
                    self.index += 1;
                    self.end += 1;
                    if self.content.as_ref().unwrap().chars().nth(self.index).unwrap() == '>' {
                        self.index += 1;
                        self.end += 1;
                        tokens.push(Token::new(
                            TokenKind::FatArrow,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 1;
                    } else if self.content.as_ref().unwrap().chars().nth(self.index).unwrap() == '=' {
                        self.index += 1;
                        self.end += 1;
                        tokens.push(Token::new(
                            TokenKind::Equals,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 1;
                    } else {
                        tokens.push(Token::new(
                            TokenKind::Equal,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 1;
                    }
                }
                '!' => {
                    self.index += 1;
                    self.end += 1;
                    if self.content.as_ref().unwrap().chars().nth(self.index).unwrap() == '=' {
                        self.index += 1;
                        self.end += 1;
                        tokens.push(Token::new(
                            TokenKind::NotEqual,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 1;
                    } else {
                        tokens.push(Token::new(
                            TokenKind::ExclamationMark,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 1;
                    }
                }
                '~' => {
                    self.index += 1;
                    self.end += 1;
                    if self.content.as_ref().unwrap().chars().nth(self.index).unwrap() == '=' {
                        self.index += 1;
                        self.end += 1;
                        tokens.push(Token::new(
                            TokenKind::BitwiseNotEqual,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 1;
                    } else {
                        tokens.push(Token::new(
                            TokenKind::BitwiseNot,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 1;
                    }
                }
                _ => {
                    errors.push(OnyxError::SyntaxError(
                        format!("unrecognized character '{}'", self.content.as_ref().unwrap().chars().nth(self.index).unwrap()),
                        Span::new(self.file_name.clone(), self.start, self.end)
                    ));
                    self.index += 1;
                    self.start += 1;
                    self.end += 1;
                }
            }
        }
        if errors.len() > 0 {
            return Err(errors);
        }
        Ok(tokens)
    }
}