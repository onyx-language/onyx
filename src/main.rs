use onyx::{
    lexer::OnyxLexer,
    error::OnyxError,
    tokens::Token,
    parser::{ Parser, ParsedAST },
    typechecker::{ Typechecker, CheckedAST },
    codegen::{ CppCodegen, RustCodegen },
};

fn main() {
    let mut args: std::env::Args = std::env::args();
    args.next();
    let file_name: String = args.next().unwrap();

    let mut lexer: OnyxLexer = OnyxLexer::new(file_name.clone()).unwrap();
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
                    let mut typechecker: Typechecker = Typechecker::new(&ast);
                    let checked_ast: Result<CheckedAST, Vec<OnyxError>> = typechecker.typecheck();
                    match checked_ast {
                        Ok(checked_ast) => {
                            let mut cpp_codegen: CppCodegen = CppCodegen::new(file_name.clone());
                            let cpp_code: Result<String, OnyxError> = cpp_codegen.codegen(&checked_ast);
                            let header_code: String = cpp_codegen.header;
                            match cpp_code {
                                Ok(cpp_code) => {
                                    println!("{}", cpp_code);
                                    println!("{}", header_code);
                                    let cpp_file_name: String = file_name.clone() + ".cpp";
                                    let header_file_name: String = file_name.clone() + ".hpp";
                                    std::fs::write(cpp_file_name, cpp_code).unwrap();
                                    std::fs::write(header_file_name, header_code).unwrap();
                                }
                                Err(err) => {
                                    println!("{}", err.to_string());
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
