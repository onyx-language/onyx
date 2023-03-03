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