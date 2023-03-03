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
    Typed(String, ParsedType, Span),    // name: T
    Untyped(ParsedType, Span),          // T
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
pub enum ParsedStatement { }

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
}