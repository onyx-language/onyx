use onyx::{
    lexer::OnyxLexer,
    error::OnyxError,
    tokens::Token,
    parser::{ Parser, ParsedAST },
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
            let ast: Result<ParsedAST, Vec<OnyxError>> = parser.parse();
            match ast {
                Ok(ast) => {
                    for statement in &ast.statements {
                        println!("{:?}", statement);
                    }
                    // NOTE: I don't remember how well this typechecker works, so it's commented out for now.
                    // let mut typechecker: Typechecker = Typechecker::new(ast);
                    // let checked_ast: Result<CheckedAST, Vec<OnyxError>> = typechecker.typecheck();
                    // match checked_ast {
                    //     Ok(checked_ast) => {
                    //         println!("{:?}", checked_ast);
                    //     }
                    //     Err(errors) => {
                    //         for error in errors {
                    //             println!("{}", error.to_string());
                    //         }
                    //     }
                    // }
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
