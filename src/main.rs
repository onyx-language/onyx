use onyx::{
    lexer::OnyxLexer,
    error::OnyxError,
    tokens::Token,
};

fn main() {
    let mut args: std::env::Args = std::env::args();
    args.next();
    let file_name: String = args.next().unwrap();

    let mut lexer: OnyxLexer = OnyxLexer::new(file_name).unwrap();
    let tokens: Result<Vec<Token>, Vec<OnyxError>> = lexer.lex();
    match tokens {
        Ok(tokens) => {
            for token in tokens {
                println!("{:?}", token);
            }
        }
        Err(errors) => {
            for error in errors {
                println!("{}", error.to_string());
            }
        }
    }
}
