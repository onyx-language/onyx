use crate::span::Span;
#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    Number(i64),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
    Identifier(String),

    Class,          // class
    Const,          // const
    Defer,          // defer
    Enum,           // enum
    Function,       // function
    Match,          // match
    Named,          // named
    Override,       // override
    Public,         // public
    Private,        // private
    Protected,      // protected
    Raw,            // raw
    Return,         // return
    Var,            // var
    Virtual,        // virtual
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
    Dot,            // .
    Comma,          // ,
    QuestionMark,   // ?

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