#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug)]
pub enum EnumVariant {
    Untyped(ParsedType, Span),          // T
    Typed(String, ParsedType, Span),    // name: T
}

#[derive(Debug)]
pub struct ParsedEnum {
    pub name: String,
    pub generic_parameters: Vec<(String, Span)>,
    pub variants: Vec<EnumVariant>,
    pub span: Span,
}