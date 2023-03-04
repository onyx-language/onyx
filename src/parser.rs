use crate::{
    tokens::{Token, TokenKind},
    span::Span,
    error::OnyxError
};
#[derive(Debug, Clone)] pub enum ParsedType {
    Name(String, Span),
    GenericType(String, Vec<ParsedType>, Span),
    Array(Box<ParsedType>, Span),
    SizedArray(Box<ParsedType>, usize, Span),
    Optional(Box<ParsedType>, Span),
    RawPtr(Box<ParsedType>, Span),
    WeakPtr(Box<ParsedType>, Span),
    Empty
}
#[derive(Debug, Clone)] pub enum EnumVariant {
    Untyped(String, Span),          // A
    // WithValue(String, ParsedExpression, Span), // A = 5
    StructLike(String, Vec<(String, ParsedType)>, Span), // A(x: i32, y: i32)
    // Typed(String, ParsedType, Span), // A: i32
}
#[derive(Debug, Clone)] pub struct ParsedEnum {
    pub name: String,
    pub generic_parameters: Vec<(String, Span)>,
    pub variants: Vec<EnumVariant>,
    pub span: Span,
}
#[derive(Debug, Clone)] pub struct ParsedVariableDeclaration {
    pub name: String,
    pub var_type: ParsedType,
    pub initializer: Option<ParsedExpression>,
    pub mutable: bool,
    pub span: Span,
}
#[derive(Debug, Clone)] pub struct ParsedFunctionDeclaration {
    pub name: String,
    pub name_span: Span,
    pub generic_parameters: Vec<(String, Span)>,
    pub parameters: Vec<ParsedParameter>,
    pub return_type: ParsedType,
    pub body: ParsedBody,
}
#[derive(Debug, Clone)] pub struct ParsedParameter {
    pub name: String,
    pub name_span: Span,
    pub parameter_type: ParsedType,
    pub initializer: Option<ParsedExpression>,
    pub is_named: bool,
}
#[derive(Debug, Clone)] pub enum ParsedFirstClassStatement {
    Enum(ParsedEnum),
    Function(ParsedFunctionDeclaration),
}
#[derive(Debug, Clone)] pub enum ParsedStatement {
    VariableDeclaration(ParsedVariableDeclaration),
    Expression(ParsedExpression),
    Garbage(Span),
}
#[derive(Debug, Clone)] pub struct ParsedBlock {
    pub stmts: Vec<ParsedStatement>,
    pub span: Span,
}
impl ParsedBlock {
    pub fn new(stmts: Vec<ParsedStatement>, span: Span) -> ParsedBlock {
        ParsedBlock { stmts, span }
    }
}
#[derive(Debug, Clone)] pub enum ParsedBody {
    Expression(ParsedExpression),
    Block(ParsedBlock),
    Empty,
}
#[derive(Debug, Clone)] pub enum MatchCase {
    StructLike {
        variant_name: String,
        name_span: Span,
        variant_arguments: Vec<(String, Option<ParsedType>)>,
        arguments_span: Span,
        body: ParsedBody,
    },
    Untyped {
        variant_name: String,
        name_span: Span,
        body: ParsedBody,
    },
}
#[derive(Debug, Clone)] pub enum ParsedExpression {
    Identifier(String, Span),
    Number(i64, Span),
    Match(Box<ParsedExpression>, Vec<MatchCase>, Span),
    Garbage(Span),
}
#[derive(Debug, Clone)] pub struct Parser {
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
            TokenKind::Enum => self.parse_enum(),
            TokenKind::Function => self.parse_function(),
            _ => {
                let err = OnyxError::SyntaxError(format!("invalid first class statement: {:?}", self.tokens[self.index].kind()), self.span());
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
    fn parse_function(&mut self) -> Result<ParsedFirstClassStatement, OnyxError> {
        self.expect(TokenKind::Function)?;
        let name_span: Span = self.span();
        let name: String = self.parse_identifier()?;
        let generic_parameters: Vec<(String, Span)> = self.parse_generic_parameters()?;
        self.expect(TokenKind::LeftParen)?;
        let mut parameters: Vec<ParsedParameter> = vec![];
        while self.tokens[self.index].kind() != TokenKind::RightParen {
            parameters.push(self.parse_parameter()?);
            if self.tokens[self.index].kind() == TokenKind::Comma {
                self.expect(TokenKind::Comma)?;
            }
        }
        self.expect(TokenKind::RightParen)?;
        self.expect(TokenKind::Arrow)?;
        let return_type: ParsedType = self.parse_type()?;
        let mut body: ParsedBody = ParsedBody::Empty;
        if self.tokens[self.index].kind() == TokenKind::FatArrow {
            self.expect(TokenKind::FatArrow)?;
            let expression: ParsedExpression = self.parse_expression()?;
            self.expect(TokenKind::Semicolon)?;
            body = ParsedBody::Expression(expression);
        } else if self.tokens[self.index].kind() == TokenKind::LeftBrace {
            let span: Span = self.tokens[self.index].span();
            self.expect(TokenKind::LeftBrace)?;
            let mut statements: Vec<ParsedStatement> = vec![];
            while self.tokens[self.index].kind() != TokenKind::RightBrace {
                statements.push(self.parse_statement()?);
                if self.tokens[self.index].kind() == TokenKind::Comma {
                    self.expect(TokenKind::Comma)?;
                }
            }
            self.expect(TokenKind::RightBrace)?;
            body = ParsedBody::Block(ParsedBlock::new(statements, span));
        } else {
            self.expect(TokenKind::Semicolon)?;
        }
        Ok(ParsedFirstClassStatement::Function(ParsedFunctionDeclaration {
            name,
            name_span,
            generic_parameters,
            parameters,
            return_type,
            body
        }))
    }
    fn parse_parameter(&mut self) -> Result<ParsedParameter, OnyxError> {
        let mut is_named: bool = false;
        if self.tokens[self.index].kind() == TokenKind::Named {
            self.expect(TokenKind::Named)?;
            is_named = true;
        }
        let name_span: Span = self.tokens[self.index].span();
        let name: String = self.parse_identifier()?;
        self.expect(TokenKind::Colon)?;
        let parameter_type: ParsedType = self.parse_type()?;
        let mut initializer: Option<ParsedExpression> = None;
        if self.tokens[self.index].kind() == TokenKind::Equals {
            self.expect(TokenKind::Equals)?;
            initializer = Some(self.parse_expression()?);
        }
        Ok(ParsedParameter {
            name,
            name_span,
            parameter_type,
            initializer,
            is_named
        })
    }
    fn parse_generic_parameters(&mut self) -> Result<Vec<(String, Span)>, OnyxError> {
        let mut generic_parameters: Vec<(String, Span)> = vec![];
        if self.tokens[self.index].kind() == TokenKind::LessThan {
            self.index += 1;
            while self.tokens[self.index].kind() != TokenKind::GreaterThan {
                generic_parameters.push((self.parse_identifier()?, self.span()));
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
            Ok(EnumVariant::StructLike(name, fields, self.span()))
        } else {
            Ok(EnumVariant::Untyped(name, self.span()))
        }
    }
    fn parse_statement(&mut self) -> Result<ParsedStatement, OnyxError> {
        match self.tokens[self.index].kind() {
            TokenKind::Const => self.parse_variable_declaration(false),
            TokenKind::Var => self.parse_variable_declaration(true),
            _ => Ok(ParsedStatement::Expression(self.parse_expression()?))
        }
    }
    fn parse_variable_declaration(&mut self, mutable: bool) -> Result<ParsedStatement, OnyxError> {
        let current_token: Token = self.tokens[self.index].clone();
        if mutable {
            self.expect(TokenKind::Var)?;
        } else {
            self.expect(TokenKind::Const)?;
        }
        let name: String = self.parse_identifier()?;
        self.expect(TokenKind::Colon)?;
        let type_: ParsedType = self.parse_type()?;
        self.expect(TokenKind::Equals)?;
        let expression: ParsedExpression = self.parse_expression()?;
        self.expect(TokenKind::Semicolon)?;
        Ok(ParsedStatement::VariableDeclaration(ParsedVariableDeclaration {
            name,
            var_type: type_,
            initializer: Some(expression),
            mutable,
            span: current_token.span(),
        }))
    }
    fn parse_expression(&mut self) -> Result<ParsedExpression, OnyxError> {
        self.parse_primary_expression()
    }
    fn parse_primary_expression(&mut self) -> Result<ParsedExpression, OnyxError> {
        let current_token: Token = self.tokens[self.index].clone();
        match current_token.kind() {
            TokenKind::Identifier(identifier) => {
                self.index += 1;
                Ok(ParsedExpression::Identifier(identifier, current_token.span()))
            }
            TokenKind::Number(number) => {
                self.index += 1;
                Ok(ParsedExpression::Number(number, current_token.span()))
            }
            TokenKind::Match => {
                self.expect(TokenKind::Match)?;
                let expression: ParsedExpression = self.parse_expression()?;
                self.expect(TokenKind::LeftBrace)?;
                let mut cases: Vec<MatchCase> = vec![];
                while self.tokens[self.index].kind() != TokenKind::RightBrace {
                    cases.push(self.parse_match_case()?);
                    if self.tokens[self.index].kind() == TokenKind::Comma {
                        self.expect(TokenKind::Comma)?;
                    }
                }
                self.expect(TokenKind::RightBrace)?;
                Ok(ParsedExpression::Match(Box::new(expression), cases, current_token.span()))
            }
            _ => {
                let err = OnyxError::SyntaxError(format!("invalid primary expression: {:?}", self.tokens[self.index].kind()), self.span());
                self.index += 1;
                Err(err)
            }
        }
    }
    fn parse_match_case(&mut self) -> Result<MatchCase, OnyxError> {
        if let TokenKind::Identifier(identifier) = self.tokens[self.index].kind() {
            let name_span: Span = self.span();
            self.index += 1;
            if self.tokens[self.index].kind() == TokenKind::LeftParen {
                self.expect(TokenKind::LeftParen)?;
                let mut variant_arguments: Vec<(String, Option<ParsedType>)> = vec![];
                let arguments_span: Span = self.span();
                while self.tokens[self.index].kind() != TokenKind::RightParen {
                    let field_name: String = self.parse_identifier()?;
                    self.expect(TokenKind::Colon)?;
                    let field_type: ParsedType = self.parse_type()?;
                    variant_arguments.push((field_name, Some(field_type)));
                    if self.tokens[self.index].kind() == TokenKind::Comma {
                        self.expect(TokenKind::Comma)?;
                    }
                }
                self.expect(TokenKind::RightParen)?;
                self.expect(TokenKind::FatArrow)?;
                let body: ParsedBody = self.parse_match_body()?;
                Ok(MatchCase::StructLike { variant_name: identifier, name_span, variant_arguments, arguments_span, body })
            } else {
                self.expect(TokenKind::FatArrow)?;
                let body: ParsedBody = self.parse_match_body()?;
                Ok(MatchCase::Untyped { variant_name: identifier, name_span, body })
            }
        } else {
            let err = OnyxError::SyntaxError(format!("invalid match case: {:?}", self.tokens[self.index].kind()), self.span());
            self.index += 1;
            Err(err)
        }
    }
    fn parse_match_body(&mut self) -> Result<ParsedBody, OnyxError> {
        let current_token: Token = self.tokens[self.index].clone();
        match current_token.kind() {
            TokenKind::LeftBrace => {
                self.expect(TokenKind::LeftBrace)?;
                let mut statements: Vec<ParsedStatement> = vec![];
                while self.tokens[self.index].kind() != TokenKind::RightBrace {
                    statements.push(self.parse_statement()?);
                    if self.tokens[self.index].kind() == TokenKind::Comma {
                        self.expect(TokenKind::Comma)?;
                    }
                }
                self.expect(TokenKind::RightBrace)?;
                Ok(ParsedBody::Block(ParsedBlock::new(statements, current_token.span())))
            }
            _ => {
                let expression: ParsedExpression = self.parse_expression()?;
                Ok(ParsedBody::Expression(expression))
            }
        }
    }
    fn parse_type(&mut self) -> Result<ParsedType, OnyxError> {
        let parsed_type: ParsedType;
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
            Err(OnyxError::SyntaxError(format!("expected identifier, but got {:?}", self.tokens[self.index].kind()), self.span()))
        }
    }
    fn expect(&mut self, kind: TokenKind) -> Result<Token, OnyxError> {
        if self.tokens[self.index].kind() == kind {
            let token = self.tokens[self.index].clone();
            self.index += 1;
            Ok(token)
        } else {
            Err(OnyxError::SyntaxError(format!("expected {:?}, but got {:?}", kind, self.tokens[self.index].kind()), self.span()))
        }
    }
    fn span(&mut self) -> Span {
        self.tokens[self.index].span()
    }
}