use onyx::{
    lexer::OnyxLexer,
    error::OnyxError,
    tokens::Token,
    parser::{
        Parser,
        ParsedFirstClassStatement
    },
};

fn main() {
    let mut args: std::env::Args = std::env::args();
    args.next();
    let file_name: String = args.next().unwrap();

    let mut lexer: OnyxLexer = OnyxLexer::new(file_name).unwrap();
    let tokens: Result<Vec<Token>, Vec<OnyxError>> = lexer.lex();
    match tokens {
        Ok(tokens) => {
            let mut parser: Parser = Parser::new(tokens);
            let statements: Result<Vec<ParsedFirstClassStatement>, Vec<OnyxError>> = parser.parse();
            match statements {
                Ok(statements) => {
                    for statement in statements {
                        println!("{:?}", statement);
                    }
                }
                Err(errors) => {
                    for error in errors {
                        println!("{}", error.to_string());
                    }
                }
            }
        }
        Err(errors) => {
            for error in errors {
                println!("{}", error.to_string());
            }
        }
    }
}
