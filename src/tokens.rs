use crate::span::Span;
#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    Number(i64),
    Identifier(String),

    Const,          // const
    Enum,           // enum
    Match,          // match
    Raw,            // raw
    Var,            // var
    Weak,           // weak

    LeftParen,      // (
    RightParen,     // )
    LeftBrace,      // {
    RightBrace,     // }
    LeftBracket,    // [
    RightBracket,   // ]

    Equals,         // =
    LessThan,       // <
    GreaterThan,    // >

    Colon,          // :
    Semicolon,      // ;
    Comma,          // ,

    Arrow,          // ->
    FatArrow,       // =>
}
#[derive(Clone, Debug)]
pub struct Token { kind: TokenKind, span: Span }
impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Token {
        Token { kind, span }
    }
    pub fn kind(&self) -> TokenKind { self.kind.clone() }
    pub fn span(&self) -> Span { self.span.clone() }
}