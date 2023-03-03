use crate::span::Span;
#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind { }
#[derive(Clone, Debug, PartialEq)]
pub struct Token { kind: TokenKind, span: Span }