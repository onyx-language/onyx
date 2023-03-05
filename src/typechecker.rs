use crate::{
    parser::ParsedAST,
    error::OnyxError
};
#[derive(Debug, Clone)] pub struct CheckedAST { }
#[derive(Debug, Clone)] pub struct Typechecker {
    pub ast: ParsedAST
}
impl Typechecker {
    pub fn new(ast: ParsedAST) -> Typechecker {
        Typechecker { ast }
    }
    pub fn typecheck(&self) -> Result<CheckedAST, Vec<OnyxError>> {
        Ok(CheckedAST {})
    }
}