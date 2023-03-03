use crate::span::Span;
#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind { }
#[derive(Clone, Debug)]
pub struct Token { kind: TokenKind, span: Span }
impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Token {
        Token { kind, span }
    }
}