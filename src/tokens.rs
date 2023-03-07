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
    Null,           // null
    Override,       // override
    Public,         // public
    Private,        // private
    Protected,      // protected
    Raw,            // raw
    Return,         // return
    Sizeof,         // sizeof
    Var,            // var
    Virtual,        // virtual
    Weak,           // weak

    LeftParen,      // (
    RightParen,     // )
    LeftBrace,      // {
    RightBrace,     // }
    LeftBracket,    // [
    RightBracket,   // ]

    Equal,              // =
    Plus,               // +
    Minus,              // -
    Star,               // *
    Slash,              // /
    Percent,            // %
    Caret,              // ^
    Equals,             // ==
    NotEqual,           // !=
    LessThan,           // <
    LessThanOrEqual,    // <=
    GreaterThan,        // >
    GreaterThanOrEqual, // >=
    And,                // &&
    Or,                 // ||
    BitwiseAnd,         // &
    BitwiseOr,          // |
    BitwiseNot,         // ~
    BitwiseLeftShift,   // <<
    BitwiseRightShift,  // >>
    Range,              // ..
    RangeInclusive,     // ..=
    RangeTo,            // ..<
    RangeToInclusive,   // ..<=
    PlusEqual,          // +=
    MinusEqual,         // -=
    StarEqual,          // *=
    SlashEqual,         // /=
    PercentEqual,       // %=
    CaretEqual,         // ^=
    BitwiseAndEqual,    // &=
    BitwiseOrEqual,     // |=
    BitwiseNotEqual,    // ~=
    BitwiseLeftShiftEqual,  // <<=
    BitwiseRightShiftEqual, // >>=
    Increment,          // ++
    Decrement,          // --

    Colon,          // :
    Semicolon,      // ;
    Dot,            // .
    Comma,          // ,
    QuestionMark,   // ?
    ExclamationMark,// !

    Arrow,          // ->
    FatArrow,       // =>
    StaticAccess,   // ::
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