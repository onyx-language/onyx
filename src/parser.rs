use crate::{
    tokens::{Token, TokenKind},
    span::Span,
    error::OnyxError
};

#[derive(Debug, Clone)]
pub enum ParsedType {
    Name(String, Span),
    GenericType(String, Vec<ParsedType>, Span),
    Array(Box<ParsedType>, Span),
    SizedArray(Box<ParsedType>, usize, Span),
    Optional(Box<ParsedType>, Span),
    RawPtr(Box<ParsedType>, Span),
    WeakPtr(Box<ParsedType>, Span),
    Empty
}

#[derive(Debug, Clone)]
pub enum EnumVariant {
    Untyped(String, Span),          // A
    // WithValue(String, ParsedExpression, Span), // A = 5
    StructLike(String, Vec<(String, ParsedType)>, Span), // A(x: i32, y: i32)
    // Typed(String, ParsedType, Span), // A: i32
}

#[derive(Debug, Clone)]
pub struct ParsedEnum {
    pub name: String,
    pub generic_parameters: Vec<(String, Span)>,
    pub variants: Vec<EnumVariant>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ParsedFirstClassStatement {
    Enum(ParsedEnum),
}

#[derive(Debug, Clone)]
pub enum ParsedStatement {
    Garbage(Span),
}

#[derive(Debug, Clone)]
pub struct ParsedBlock {
    pub stmts: Vec<ParsedStatement>,
}

impl ParsedBlock {
    pub fn new(stmts: Vec<ParsedStatement>) -> ParsedBlock {
        ParsedBlock { stmts }
    }
}

#[derive(Debug, Clone)]
pub enum MatchBody {
    Expression(ParsedExpression),
    Block(ParsedBlock),
}

#[derive(Debug, Clone)]
pub enum MatchCase {
    EnumVariant {
        variant_name: Vec<(String, Span)>,
        variant_arguments: Vec<(Option<String>, String)>,
        arguments_span: Span,
        body: MatchBody,
    },
}

#[derive(Debug, Clone)]
pub enum ParsedExpression {
    Match(Box<ParsedExpression>, Vec<MatchCase>, Span),
    Garbage(Span),
}

#[derive(Debug, Clone)]
pub struct Parser {
    pub tokens: Vec<Token>,
    pub index: usize,
    pub errors: Vec<OnyxError>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens,
            index: 0,
            errors: vec![],
        }
    }
    pub fn parse(&mut self) -> Result<Vec<ParsedFirstClassStatement>, Vec<OnyxError>> {
        let mut statements: Vec<ParsedFirstClassStatement> = vec![];
        while self.index < self.tokens.len() {
            statements.push(match self.parse_first_class_statement() {
                Ok(statement) => statement,
                Err(error) => {
                    self.errors.push(error);
                    continue;
                }
            });
        }
        if self.errors.len() > 0 {
            Err(self.errors.clone())
        } else {
            Ok(statements)
        }
    }
    fn parse_first_class_statement(&mut self) -> Result<ParsedFirstClassStatement, OnyxError> {
        match self.tokens[self.index].kind() {
            TokenKind::Enum => {
                self.parse_enum()
            }
            _ => {
                let err = OnyxError::SyntaxError(format!("invalid first class statement: {:?}", self.tokens[self.index].kind()), self.tokens[self.index].span());
                self.index += 1;
                Err(err)
            }
        }
    }
    fn parse_enum(&mut self) -> Result<ParsedFirstClassStatement, OnyxError> {
        let current_token: Token = self.tokens[self.index].clone();
        self.expect(TokenKind::Enum)?;
        let name: String = self.parse_identifier()?;
        let generic_parameters: Vec<(String, Span)> = self.parse_generic_parameters()?;
        self.expect(TokenKind::LeftBrace)?;
        let mut variants: Vec<EnumVariant> = vec![];
        while self.tokens[self.index].kind() != TokenKind::RightBrace {
            variants.push(self.parse_enum_variant()?);
            if self.tokens[self.index].kind() == TokenKind::Comma {
                self.expect(TokenKind::Comma)?;
            }
        }
        self.expect(TokenKind::RightBrace)?;
        Ok(ParsedFirstClassStatement::Enum(ParsedEnum {
            name,
            generic_parameters,
            variants,
            span: current_token.span(),
        }))
    }
    fn parse_generic_parameters(&mut self) -> Result<Vec<(String, Span)>, OnyxError> {
        let mut generic_parameters: Vec<(String, Span)> = vec![];
        if self.tokens[self.index].kind() == TokenKind::LessThan {
            self.index += 1;
            while self.tokens[self.index].kind() != TokenKind::GreaterThan {
                generic_parameters.push((self.parse_identifier()?, self.tokens[self.index].span()));
                if self.tokens[self.index].kind() == TokenKind::Comma {
                    self.index += 1;
                }
            }
            self.expect(TokenKind::GreaterThan)?;
        }
        Ok(generic_parameters)
    }
    fn parse_enum_variant(&mut self) -> Result<EnumVariant, OnyxError> {
        let name: String = self.parse_identifier()?;
        if self.tokens[self.index].kind() == TokenKind::LeftParen {
            self.expect(TokenKind::LeftParen)?;
            let mut fields: Vec<(String, ParsedType)> = vec![];
            while self.tokens[self.index].kind() != TokenKind::RightParen {
                let field_name: String = self.parse_identifier()?;
                self.expect(TokenKind::Colon)?;
                let field_type: ParsedType = self.parse_type()?;
                fields.push((field_name, field_type));
                if self.tokens[self.index].kind() == TokenKind::Comma {
                    self.expect(TokenKind::Comma)?;
                }
            }
            self.expect(TokenKind::RightParen)?;
            Ok(EnumVariant::StructLike(name, fields, self.tokens[self.index].span()))
        } else {
            Ok(EnumVariant::Untyped(name, self.tokens[self.index].span()))
        }
    }
    fn parse_expression(&mut self) -> Result<ParsedExpression, OnyxError> {
        Ok(ParsedExpression::Garbage(self.tokens[self.index].span().clone()))
    }
    fn parse_type(&mut self) -> Result<ParsedType, OnyxError> {
        let mut parsed_type: ParsedType = ParsedType::Empty;
        let current_token: Token = self.tokens[self.index].clone();
        match current_token.kind() {
            TokenKind::Identifier(identifier) => {
                self.index += 1;
                if self.tokens[self.index].kind() == TokenKind::LessThan {
                    self.expect(TokenKind::LessThan)?;
                    let mut generic_types: Vec<ParsedType> = vec![];
                    while self.tokens[self.index].kind() != TokenKind::GreaterThan {
                        let ty: ParsedType = self.parse_type()?;
                        generic_types.push(ty);
                        if self.tokens[self.index].kind() == TokenKind::Comma {
                            self.expect(TokenKind::Comma)?;
                        }
                    }
                    self.expect(TokenKind::GreaterThan)?;
                    parsed_type = ParsedType::GenericType(identifier, generic_types, current_token.span());
                } else {
                    parsed_type = ParsedType::Name(identifier, current_token.span());
                }
            }
            TokenKind::LeftBracket => {
                self.expect(TokenKind::LeftBracket)?;
                let t: ParsedType = self.parse_type()?;
                self.expect(TokenKind::RightBracket)?;
                parsed_type = ParsedType::Array(Box::new(t), current_token.span());
            }
            TokenKind::Raw => {
                self.expect(TokenKind::Raw)?;
                let t: ParsedType = self.parse_type()?;
                parsed_type = ParsedType::RawPtr(Box::new(t), current_token.span());
            }
            TokenKind::Weak => {
                self.expect(TokenKind::Weak)?;
                let t: ParsedType = self.parse_type()?;
                parsed_type = ParsedType::WeakPtr(Box::new(t), current_token.span());
            }
            _ => {
                return Err(OnyxError::SyntaxError(format!("expected type, but got {:?}", current_token.kind()), current_token.span()));
            }
        }
        Ok(parsed_type)
    }
    fn parse_identifier(&mut self) -> Result<String, OnyxError> {
        if let TokenKind::Identifier(identifier) = self.tokens[self.index].kind() {
            self.index += 1;
            Ok(identifier)
        } else {
            Err(OnyxError::SyntaxError(format!("expected identifier, but got {:?}", self.tokens[self.index].kind()), self.tokens[self.index].span()))
        }
    }
    fn expect(&mut self, kind: TokenKind) -> Result<Token, OnyxError> {
        if self.tokens[self.index].kind() == kind {
            let token = self.tokens[self.index].clone();
            self.index += 1;
            Ok(token)
        } else {
            Err(OnyxError::SyntaxError(format!("expected {:?}, but got {:?}", kind, self.tokens[self.index].kind()), self.tokens[self.index].span()))
        }
    }
}