use crate::{
    tokens::{Token, TokenKind},
    span::Span,
    error::OnyxError
};
#[derive(Debug, Clone, PartialEq)] pub enum Visibility {
    Public,
    Private,
    Protected,
}
#[derive(Debug, Clone)] pub struct ParsedAST {
    pub statements: Vec<ParsedFirstClassStatement>,
}
#[derive(Debug, Clone, PartialEq)] pub enum ParsedType {
    Name(String, Span),
    GenericType(String, Vec<ParsedType>, Span),
    Array(Box<ParsedType>, Span),
    SizedArray(Box<ParsedType>, usize, Span),
    Optional(Box<ParsedType>, Span),
    RawPtr(Box<ParsedType>, Span),
    WeakPtr(Box<ParsedType>, Span),
    Empty(Span)
}
impl ParsedType {
    pub fn span(&self) -> Span {
        match self {
            ParsedType::Name(_, span) => span.clone(),
            ParsedType::GenericType(_, _, span) => span.clone(),
            ParsedType::Array(_, span) => span.clone(),
            ParsedType::SizedArray(_, _, span) => span.clone(),
            ParsedType::Optional(_, span) => span.clone(),
            ParsedType::RawPtr(_, span) => span.clone(),
            ParsedType::WeakPtr(_, span) => span.clone(),
            ParsedType::Empty(span) => span.clone(),
        }
    }
}
#[derive(Debug, Clone)] pub enum EnumVariant {
    NoValues(String, Span),                                 // A
    // TODO: I need to figure out how to parse this but also parse the ValuesNamed.
    ValuesNoNamed(String, Vec<ParsedType>, Span),           // A(i32, i32)
    ValuesNamed(String, Vec<(String, ParsedType)>, Span),   // A(x: i32, y: i32)
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
#[derive(Debug, Clone)] pub struct ParsedFunction {
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
#[derive(Debug, Clone)] pub struct ParsedClass {
    pub name: String,
    pub name_span: Span,
    pub generic_parameters: Vec<(String, Span)>,
    pub parent_class: Option<String>,
    pub parent_generic_parameters: Option<Vec<(String, Span)>>,
    pub fields: Vec<ParsedField>,
    pub methods: Vec<ParsedMethod>,
}
#[derive(Debug, Clone)] pub struct ParsedField {
    pub base_declaration: ParsedVariableDeclaration,
    pub visibility: Visibility,
}
#[derive(Debug, Clone)] pub struct ParsedMethod {
    pub base_declaration: ParsedFunction,
    pub visibility: Visibility,
    pub is_virtual: bool,
    pub is_override: bool,
}
impl ParsedMethod {
    pub fn is_static(&self) -> bool {
        self.base_declaration.parameters.clone().get(0).unwrap().name == "this"
    }
}
#[derive(Debug, Clone)] pub enum ParsedFirstClassStatement {
    Enum(ParsedEnum),
    Function(ParsedFunction),
    Class(ParsedClass)
}
#[derive(Debug, Clone)] pub enum ParsedStatement {
    Defer(ParsedBody, Span),
    Expression(ParsedExpression),
    Return(Option<ParsedExpression>, Span),
    VariableDeclaration(ParsedVariableDeclaration),
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
#[derive(Debug, Clone)] pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Exponent,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    And,
    Or,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseLeftShift,
    BitwiseRightShift,
    Range,
    RangeInclusive,
    RangeTo,
    RangeToInclusive,
}
#[derive(Debug, Clone)] pub enum UnaryOperator {
    Negate,
    Not,
    BitwiseNot,
    Increment,
    Decrement,
}
#[derive(Debug, Clone)] pub enum ParsedExpression {
    Array(Vec<ParsedExpression>, Span),
    Assignment(Box<ParsedExpression>, Box<ParsedExpression>, Span),
    BinaryOperation(Box<ParsedExpression>, BinaryOperator, Box<ParsedExpression>, Span),
    Boolean(bool, Span),
    Call(Box<ParsedExpression>, Vec<(Option<String>, ParsedExpression)>, Span),
    Character(char, Span),
    FloatingPoint(f64, Span),
    Identifier(String, Span),
    Match(Box<ParsedExpression>, Vec<MatchCase>, Span),
    MemberAccess(Box<ParsedExpression>, Box<ParsedExpression>, Span),
    Null(Span),
    Number(i64, Span),
    String(String, Span),
    UnaryOperation(UnaryOperator, Box<ParsedExpression>, Span),
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
    pub fn parse(&mut self) -> Result<ParsedAST, Vec<OnyxError>> {
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
            Ok(ParsedAST { statements })
        }
    }
    fn parse_first_class_statement(&mut self) -> Result<ParsedFirstClassStatement, OnyxError> {
        match self.tokens[self.index].kind() {
            TokenKind::Enum => self.parse_enum(),
            TokenKind::Function => self.parse_function(),
            TokenKind::Class => self.parse_class(),
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
            if self.tokens[self.index].kind() == TokenKind::Identifier("this".to_string()) {
                let name_span: Span = self.span();
                self.expect(TokenKind::Identifier("this".to_string()))?;
                let type_span: Span = self.span();
                parameters.push(ParsedParameter {
                    name: "this".to_string(),
                    name_span,
                    parameter_type: ParsedType::Empty(type_span),
                    initializer: None,
                    is_named: false,
                });
                if self.tokens[self.index].kind() == TokenKind::Comma {
                    self.expect(TokenKind::Comma)?;
                }
                continue;
            }
            parameters.push(self.parse_parameter()?);
            if self.tokens[self.index].kind() == TokenKind::Comma {
                self.expect(TokenKind::Comma)?;
            }
        }
        self.expect(TokenKind::RightParen)?;
        let mut return_type: ParsedType = ParsedType::Empty(self.span());
        if self.tokens[self.index].kind() == TokenKind::Arrow {
            self.expect(TokenKind::Arrow)?;
            return_type = self.parse_type()?;
        }
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
        Ok(ParsedFirstClassStatement::Function(ParsedFunction {
            name,
            name_span,
            generic_parameters,
            parameters,
            return_type,
            body
        }))
    }
    fn parse_class(&mut self) -> Result<ParsedFirstClassStatement, OnyxError> {
        self.expect(TokenKind::Class)?;
        let name_span: Span = self.span();
        let name: String = self.parse_identifier()?;
        let generic_parameters: Vec<(String, Span)> = self.parse_generic_parameters()?;
        let mut parent_class: Option<String> = None;
        let mut parent_generic_parameters: Option<Vec<(String, Span)>> = None;
        if self.tokens[self.index].kind() == TokenKind::Colon {
            self.expect(TokenKind::Colon)?;
            parent_class = Some(self.parse_identifier()?);
            parent_generic_parameters = Some(self.parse_generic_parameters()?);
        }
        self.expect(TokenKind::LeftBrace)?;
        let mut fields: Vec<ParsedField> = vec![];
        let mut methods: Vec<ParsedMethod> = vec![];
        while self.tokens[self.index].kind().clone() != TokenKind::RightBrace {
            let mut visibility: Visibility = Visibility::Public;
            if self.tokens[self.index].kind() == TokenKind::Private ||
                self.tokens[self.index].kind() == TokenKind::Public ||
                self.tokens[self.index].kind() == TokenKind::Protected {
                visibility = self.parse_visibility()?;
            }
            let mut is_override: bool = false;
            let mut is_virtual: bool = false;
            if self.tokens[self.index].kind() == TokenKind::Override {
                self.expect(TokenKind::Override)?;
                is_override = true;
            } else if self.tokens[self.index].kind() == TokenKind::Virtual {
                self.expect(TokenKind::Virtual)?;
                is_virtual = false;
            }
            match self.tokens[self.index].kind() {
                // TODO: parse virtual and override
                TokenKind::Function => {
                    let base_declaration: ParsedFunction = match self.parse_function()? {
                        ParsedFirstClassStatement::Function(function) => function,
                        _ => {
                            let err = OnyxError::SyntaxError(format!("expected function, but got: {:?}", self.tokens[self.index].kind()), self.span());
                            self.index += 1;
                            return Err(err);
                        }
                    };
                    methods.push(ParsedMethod {
                        base_declaration,
                        visibility,
                        is_virtual,
                        is_override
                    })
                }
                TokenKind::Var | TokenKind::Const => {
                    let mutable: bool = self.tokens[self.index].kind() == TokenKind::Var;
                    let base_declaration: ParsedVariableDeclaration = match self.parse_variable_declaration(mutable)? {
                        ParsedStatement::VariableDeclaration(variable) => variable,
                        _ => {
                            let err = OnyxError::SyntaxError(format!("expected variable, but got: {:?}", self.tokens[self.index].kind()), self.span());
                            self.index += 1;
                            return Err(err);
                        }
                    };
                    fields.push(ParsedField {
                        base_declaration,
                        visibility
                    })
                }
                _ => {
                    let err = OnyxError::SyntaxError(format!("expected function, virtual, override or var, but got: {:?}", self.tokens[self.index].kind()), self.span());
                    self.index += 1;
                    return Err(err);
                }
            }
            if self.tokens[self.index].kind() == TokenKind::Comma {
                self.expect(TokenKind::Comma)?;
            }
        }
        self.expect(TokenKind::RightBrace)?;
        Ok(ParsedFirstClassStatement::Class(ParsedClass {
            name,
            name_span,
            generic_parameters,
            parent_class,
            parent_generic_parameters,
            fields,
            methods
        }))
    }
    fn parse_visibility(&mut self) -> Result<Visibility, OnyxError> {
        match self.tokens[self.index].kind() {
            TokenKind::Public => {
                self.expect(TokenKind::Public)?;
                Ok(Visibility::Public)
            },
            TokenKind::Private => {
                self.expect(TokenKind::Private)?;
                Ok(Visibility::Private)
            },
            TokenKind::Protected => {
                self.expect(TokenKind::Protected)?;
                Ok(Visibility::Protected)
            },
            _ => {
                let err = OnyxError::SyntaxError(format!("invalid visibility: {:?}", self.tokens[self.index].kind()), self.span());
                self.index += 1;
                Err(err)
            }
        }
    }
    fn parse_parameter(&mut self) -> Result<ParsedParameter, OnyxError> {
        let mut is_named: bool = false;
        if self.tokens[self.index].kind() == TokenKind::Named {
            self.expect(TokenKind::Named)?;
            is_named = true;
        }
        let name_span: Span = self.tokens[self.index].span();
        let name: String = self.parse_identifier()?;
        let mut parameter_type: ParsedType = ParsedType::Empty(name_span.clone());
        if self.tokens[self.index].kind() == TokenKind::Colon {
            self.expect(TokenKind::Colon)?;
            parameter_type = self.parse_type()?;
        }
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
            Ok(EnumVariant::ValuesNamed(name, fields, self.span()))
        } else {
            Ok(EnumVariant::NoValues(name, self.span()))
        }
    }
    fn parse_statement(&mut self) -> Result<ParsedStatement, OnyxError> {
        match self.tokens[self.index].kind() {
            TokenKind::Const => self.parse_variable_declaration(false),
            TokenKind::Var => self.parse_variable_declaration(true),
            TokenKind::Return => self.parse_return(),
            TokenKind::Defer => self.parse_defer(),
            _ => {
                let expression: ParsedExpression = self.parse_expression()?;
                self.expect(TokenKind::Semicolon)?;
                Ok(ParsedStatement::Expression(expression))
            }
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
        let mut var_type: ParsedType = ParsedType::Empty(self.span());
        if self.tokens[self.index].kind() == TokenKind::Colon {
            self.expect(TokenKind::Colon)?;
            var_type = self.parse_type()?;
        }
        let mut expression: Option<ParsedExpression> = None;
        if self.tokens[self.index].kind() == TokenKind::Equals {
            self.expect(TokenKind::Equals)?;
            expression = Some(self.parse_expression()?);
        }
        self.expect(TokenKind::Semicolon)?;
        Ok(ParsedStatement::VariableDeclaration(ParsedVariableDeclaration {
            name,
            var_type,
            initializer: expression,
            mutable,
            span: current_token.span(),
        }))
    }
    fn parse_return(&mut self) -> Result<ParsedStatement, OnyxError> {
        let span: Span = self.span();
        self.expect(TokenKind::Return)?;
        let mut expression: Option<ParsedExpression> = None;
        if self.tokens[self.index].kind() != TokenKind::Semicolon {
            expression = Some(self.parse_expression()?);
        }
        self.expect(TokenKind::Semicolon)?;
        Ok(ParsedStatement::Return(expression, span))
    }
    fn parse_defer(&mut self) -> Result<ParsedStatement, OnyxError> {
        let span: Span = self.span();
        self.expect(TokenKind::Defer)?;
        let body: ParsedBody;
        if self.tokens[self.index].kind() == TokenKind::LeftBrace {
            let span: Span = self.span();
            self.expect(TokenKind::LeftBrace)?;
            let mut statements: Vec<ParsedStatement> = vec![];
            while self.tokens[self.index].kind() != TokenKind::RightBrace {
                statements.push(self.parse_statement()?);
                if self.tokens[self.index].kind() == TokenKind::Comma {
                    self.expect(TokenKind::Comma)?;
                }
            }
            self.expect(TokenKind::RightBrace)?;
            body = ParsedBody::Block(ParsedBlock::new(statements, span))
        } else {
            let expression: ParsedExpression = self.parse_expression()?;
            self.expect(TokenKind::Semicolon)?;
            body = ParsedBody::Expression(expression);
        }
        Ok(ParsedStatement::Defer(body, span))
    }
    fn parse_expression(&mut self) -> Result<ParsedExpression, OnyxError> {
        self.parse_assignment_expression()
    }
    fn parse_assignment_expression(&mut self) -> Result<ParsedExpression, OnyxError> {
        let span: Span = self.span();
        let mut expression: ParsedExpression = self.parse_call_expression()?;
        loop {
            match self.tokens[self.index].kind() {
                TokenKind::Equal => {
                    self.index += 1;
                    expression = ParsedExpression::Assignment(Box::new(expression), Box::new(self.parse_expression()?), span.clone());
                }
                TokenKind::PlusEqual => {
                    self.index += 1;
                    expression = ParsedExpression::Assignment(Box::new(expression.clone()), Box::new(ParsedExpression::BinaryOperation(Box::new(expression), BinaryOperator::Add, Box::new(self.parse_expression()?), span.clone())), span.clone());
                }
                TokenKind::MinusEqual => {
                    self.index += 1;
                    expression = ParsedExpression::Assignment(Box::new(expression.clone()), Box::new(ParsedExpression::BinaryOperation(Box::new(expression), BinaryOperator::Subtract, Box::new(self.parse_expression()?), span.clone())), span.clone());
                }
                TokenKind::StarEqual => {
                    self.index += 1;
                    expression = ParsedExpression::Assignment(Box::new(expression.clone()), Box::new(ParsedExpression::BinaryOperation(Box::new(expression), BinaryOperator::Multiply, Box::new(self.parse_expression()?), span.clone())), span.clone());
                }
                TokenKind::SlashEqual => {
                    self.index += 1;
                    expression = ParsedExpression::Assignment(Box::new(expression.clone()), Box::new(ParsedExpression::BinaryOperation(Box::new(expression), BinaryOperator::Divide, Box::new(self.parse_expression()?), span.clone())), span.clone());
                }
                TokenKind::PercentEqual => {
                    self.index += 1;
                    expression = ParsedExpression::Assignment(Box::new(expression.clone()), Box::new(ParsedExpression::BinaryOperation(Box::new(expression), BinaryOperator::Modulo, Box::new(self.parse_expression()?), span.clone())), span.clone());
                }
                _ => break,
            }
        }
        Ok(expression)
    }
    fn parse_call_expression(&mut self) -> Result<ParsedExpression, OnyxError> {
        let mut expression: ParsedExpression = self.parse_equality_expression()?;
        loop {
            match self.tokens[self.index].kind() {
                TokenKind::LeftParen => {
                    self.index += 1;
                    let mut arguments: Vec<(Option<String>, ParsedExpression)> = vec![];
                    while self.tokens[self.index].kind() != TokenKind::RightParen {
                        if self.tokens[self.index + 1].kind() == TokenKind::Colon {
                            let name: String = self.parse_identifier()?;
                            self.expect(TokenKind::Colon)?;
                            let argument: ParsedExpression = self.parse_expression()?;
                            arguments.push((Some(name), argument));
                        } else {
                            let argument: ParsedExpression = self.parse_expression()?;
                            arguments.push((None, argument));
                        }
                        if self.tokens[self.index].kind() == TokenKind::Comma {
                            self.expect(TokenKind::Comma)?;
                        }
                    }
                    self.expect(TokenKind::RightParen)?;
                    expression = ParsedExpression::Call(Box::new(expression), arguments, self.span());
                }
                _ => break,
            }
        }
        Ok(expression)
    }
    fn parse_equality_expression(&mut self) -> Result<ParsedExpression, OnyxError> {
        let mut expression: ParsedExpression = self.parse_comparison_expression()?;
        loop {
            match self.tokens[self.index].kind() {
                TokenKind::Equals => {
                    self.index += 1;
                    expression = ParsedExpression::BinaryOperation(Box::new(expression), BinaryOperator::Equal, Box::new(self.parse_expression()?), self.span());
                }
                TokenKind::NotEqual => {
                    self.index += 1;
                    expression = ParsedExpression::BinaryOperation(Box::new(expression), BinaryOperator::NotEqual, Box::new(self.parse_expression()?), self.span());
                }
                _ => break,
            }
        }
        Ok(expression)
    }
    fn parse_comparison_expression(&mut self) -> Result<ParsedExpression, OnyxError> {
        let mut expression: ParsedExpression = self.parse_additive_expression()?;
        loop {
            match self.tokens[self.index].kind() {
                TokenKind::LessThan => {
                    self.index += 1;
                    expression = ParsedExpression::BinaryOperation(Box::new(expression), BinaryOperator::LessThan, Box::new(self.parse_expression()?), self.span());
                }
                TokenKind::LessThanOrEqual => {
                    self.index += 1;
                    expression = ParsedExpression::BinaryOperation(Box::new(expression), BinaryOperator::LessThanOrEqual, Box::new(self.parse_expression()?), self.span());
                }
                TokenKind::GreaterThan => {
                    self.index += 1;
                    expression = ParsedExpression::BinaryOperation(Box::new(expression), BinaryOperator::GreaterThan, Box::new(self.parse_expression()?), self.span());
                }
                TokenKind::GreaterThanOrEqual => {
                    self.index += 1;
                    expression = ParsedExpression::BinaryOperation(Box::new(expression), BinaryOperator::GreaterThanOrEqual, Box::new(self.parse_expression()?), self.span());
                }
                _ => break,
            }
        }
        Ok(expression)
    }
    fn parse_additive_expression(&mut self) -> Result<ParsedExpression, OnyxError> {
        let mut expression: ParsedExpression = self.parse_multiplicative_expression()?;
        loop {
            match self.tokens[self.index].kind() {
                TokenKind::Plus => {
                    self.index += 1;
                    expression = ParsedExpression::BinaryOperation(Box::new(expression), BinaryOperator::Add, Box::new(self.parse_expression()?), self.span());
                }
                TokenKind::Minus => {
                    self.index += 1;
                    expression = ParsedExpression::BinaryOperation(Box::new(expression), BinaryOperator::Subtract, Box::new(self.parse_expression()?), self.span());
                }
                _ => break,
            }
        }
        Ok(expression)
    }
    fn parse_multiplicative_expression(&mut self) -> Result<ParsedExpression, OnyxError> {
        let mut expression: ParsedExpression = self.parse_range_expression()?;
        loop {
            match self.tokens[self.index].kind() {
                TokenKind::Star => {
                    self.index += 1;
                    expression = ParsedExpression::BinaryOperation(Box::new(expression), BinaryOperator::Multiply, Box::new(self.parse_expression()?), self.span());
                }
                TokenKind::Slash => {
                    self.index += 1;
                    expression = ParsedExpression::BinaryOperation(Box::new(expression), BinaryOperator::Divide, Box::new(self.parse_expression()?), self.span());
                }
                TokenKind::Percent => {
                    self.index += 1;
                    expression = ParsedExpression::BinaryOperation(Box::new(expression), BinaryOperator::Modulo, Box::new(self.parse_expression()?), self.span());
                }
                _ => break,
            }
        }
        Ok(expression)
    }
    fn parse_range_expression(&mut self) -> Result<ParsedExpression, OnyxError> {
        let mut expression: ParsedExpression = self.parse_unary_expression()?;
        loop {
            match self.tokens[self.index].kind() {
                TokenKind::Range => {
                    self.index += 1;
                    expression = ParsedExpression::BinaryOperation(Box::new(expression), BinaryOperator::Range, Box::new(self.parse_expression()?), self.span());
                }
                TokenKind::RangeInclusive => {
                    self.index += 1;
                    expression = ParsedExpression::BinaryOperation(Box::new(expression), BinaryOperator::RangeInclusive, Box::new(self.parse_expression()?), self.span());
                }
                TokenKind::RangeTo => {
                    self.index += 1;
                    expression = ParsedExpression::BinaryOperation(Box::new(expression), BinaryOperator::RangeTo, Box::new(self.parse_expression()?), self.span());
                }
                TokenKind::RangeToInclusive => {
                    self.index += 1;
                    expression = ParsedExpression::BinaryOperation(Box::new(expression), BinaryOperator::RangeToInclusive, Box::new(self.parse_expression()?), self.span());
                }
                _ => break,
            }
        }
        Ok(expression)
    }
    fn parse_unary_expression(&mut self) -> Result<ParsedExpression, OnyxError> {
        let span: Span = self.span();
        match self.tokens[self.index].kind() {
            TokenKind::Minus => {
                self.index += 1;
                Ok(ParsedExpression::UnaryOperation(UnaryOperator::Negate, Box::new(self.parse_unary_expression()?), span))
            }
            TokenKind::ExclamationMark => {
                self.index += 1;
                Ok(ParsedExpression::UnaryOperation(UnaryOperator::Not, Box::new(self.parse_unary_expression()?), span))
            }
            TokenKind::BitwiseNot => {
                self.index += 1;
                Ok(ParsedExpression::UnaryOperation(UnaryOperator::BitwiseNot, Box::new(self.parse_unary_expression()?), span))
            }
            TokenKind::Increment => {
                self.index += 1;
                Ok(ParsedExpression::UnaryOperation(UnaryOperator::Increment, Box::new(self.parse_unary_expression()?), span))
            }
            TokenKind::Decrement => {
                self.index += 1;
                Ok(ParsedExpression::UnaryOperation(UnaryOperator::Decrement, Box::new(self.parse_unary_expression()?), span))
            }
            _ => self.parse_postfix_expression(),
        }
    }
    fn parse_postfix_expression(&mut self) -> Result<ParsedExpression, OnyxError> {
        let span: Span = self.span();
        let mut expression: ParsedExpression = self.parse_primary_expression()?;
        loop {
            match self.tokens[self.index].kind() {
                TokenKind::Dot => {
                    self.index += 1;
                    expression = ParsedExpression::MemberAccess(Box::new(expression), Box::new(self.parse_expression()?), span.clone());
                }
                TokenKind::Increment => {
                    self.index += 1;
                    expression = ParsedExpression::UnaryOperation(UnaryOperator::Increment, Box::new(expression), span.clone());
                }
                TokenKind::Decrement => {
                    self.index += 1;
                    expression = ParsedExpression::UnaryOperation(UnaryOperator::Decrement, Box::new(expression), span.clone());
                }
                _ => break,
            }
        }
        Ok(expression)
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
            TokenKind::Float(float) => {
                self.index += 1;
                Ok(ParsedExpression::FloatingPoint(float, current_token.span()))
            }
            TokenKind::String(string) => {
                self.index += 1;
                Ok(ParsedExpression::String(string, current_token.span()))
            }
            TokenKind::Bool(value) => {
                self.index += 1;
                Ok(ParsedExpression::Boolean(value, current_token.span()))
            }
            TokenKind::Char(character) => {
                self.index += 1;
                Ok(ParsedExpression::Character(character, current_token.span()))
            }
            TokenKind::Null => {
                self.index += 1;
                Ok(ParsedExpression::Null(current_token.span()))
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
            TokenKind::LeftBracket => {
                self.expect(TokenKind::LeftBracket)?;
                let mut elements: Vec<ParsedExpression> = vec![];
                while self.tokens[self.index].kind() != TokenKind::RightBracket {
                    elements.push(self.parse_expression()?);
                    if self.tokens[self.index].kind() == TokenKind::Comma {
                        self.expect(TokenKind::Comma)?;
                    }
                }
                self.expect(TokenKind::RightBracket)?;
                Ok(ParsedExpression::Array(elements, current_token.span()))
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
        let mut parsed_type: ParsedType;
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
        if self.tokens[self.index].kind() == TokenKind::QuestionMark {
            let span: Span = self.span();
            self.expect(TokenKind::QuestionMark)?;
            parsed_type = ParsedType::Optional(Box::new(parsed_type), span);
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