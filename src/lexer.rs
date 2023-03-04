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
            match self.curr_char() {
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
                '\r' => {
                    self.index += 1;
                }
                '\n' => {
                    self.index += 1;
                    self.line += 1;
                    self.start = self.end;
                }
                '0'..='9' => {
                    let mut number = String::new();
                    while self.curr_char().is_numeric() {
                        number.push(self.curr_char());
                        self.index += 1;
                        self.end += 1;
                    }
                    tokens.push(Token::new(
                        TokenKind::Number(number.parse::<i64>().unwrap()),
                        Span::new(self.file_name.clone(), self.start, self.end),
                    ));
                    self.start = self.end;
                }
                'a'..='z' | 'A'..='Z' | '_' => {
                    let mut identifier = String::new();
                    while self.curr_char().is_alphanumeric() || self.curr_char() == '_' {
                        identifier.push(self.curr_char());
                        self.index += 1;
                        self.end += 1;
                    }
                    tokens.push(Token::new(
                        match identifier.as_str() {
                            "const" => TokenKind::Const,
                            "enum" => TokenKind::Enum,
                            "raw" => TokenKind::Raw,
                            "weak" => TokenKind::Weak,
                            _ => TokenKind::Identifier(identifier),
                        },
                        Span::new(self.file_name.clone(), self.start, self.end),
                    ));
                    self.start = self.end;
                }
                '\0' => break,
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
                    tokens.push(Token::new(
                        TokenKind::LessThan,
                        Span::new(self.file_name.clone(), self.start, self.end),
                    ));
                    self.start += 1;
                }
                '>' => {
                    self.index += 1;
                    self.end += 1;
                    tokens.push(Token::new(
                        TokenKind::GreaterThan,
                        Span::new(self.file_name.clone(), self.start, self.end),
                    ));
                    self.start += 1;
                }
                ':' => {
                    self.index += 1;
                    self.end += 1;
                    tokens.push(Token::new(
                        TokenKind::Colon,
                        Span::new(self.file_name.clone(), self.start, self.end),
                    ));
                    self.start += 1;
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
                ',' => {
                    self.index += 1;
                    self.end += 1;
                    tokens.push(Token::new(
                        TokenKind::Comma,
                        Span::new(self.file_name.clone(), self.start, self.end),
                    ));
                    self.start += 1;
                }
                '-' => {
                    self.index += 1;
                    self.end += 1;
                    if self.curr_char() == '>' {
                        self.index += 1;
                        self.end += 1;
                        tokens.push(Token::new(
                            TokenKind::Arrow,
                            Span::new(self.file_name.clone(), self.start, self.end),
                        ));
                        self.start += 1;
                    } else {
                        // tokens.push(Token::new(
                        //     TokenKind::Minus,
                        //     Span::new(self.file_name.clone(), self.start, self.end),
                        // ));
                        self.start += 1;
                    }
                }
                _ => {
                    errors.push(OnyxError::SyntaxError(
                        format!("unrecognized character '{}'", self.curr_char()),
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
    fn curr_char(&mut self) -> char {
        match self.content.as_ref().unwrap().chars().nth(self.index) {
            Some(c) => c,
            None => '\0',
        }
    }
}