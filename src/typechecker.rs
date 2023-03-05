use crate::{
    parser::{ParsedAST, ParsedFirstClassStatement, ParsedType, ParsedBody, ParsedExpression, ParsedStatement},
    error::OnyxError, span::Span
};
#[derive(Debug, Clone, PartialEq)] pub enum CheckedType {
    Void(Span),
    I8(Span), I16(Span), I32(Span), I64(Span),
    U8(Span), U16(Span), U32(Span), U64(Span),
    F32(Span), F64(Span),
    Bool(Span),
    Char(Span), 
    Array(Box<CheckedType>, Span),
    SizedArray(Box<CheckedType>, usize, Span),
    Optional(Box<CheckedType>, Span),
    RawPtr(Box<CheckedType>, Span),
    WeakPtr(Box<CheckedType>, Span),
}
impl CheckedType {
    pub fn name(&self) -> String {
        match self {
            CheckedType::Void(_) => "void".to_string(),
            CheckedType::I8(_) => "i8".to_string(),
            CheckedType::I16(_) => "i16".to_string(),
            CheckedType::I32(_) => "i32".to_string(),
            CheckedType::I64(_) => "i64".to_string(),
            CheckedType::U8(_) => "u8".to_string(),
            CheckedType::U16(_) => "u16".to_string(),
            CheckedType::U32(_) => "u32".to_string(),
            CheckedType::U64(_) => "u64".to_string(),
            CheckedType::F32(_) => "f32".to_string(),
            CheckedType::F64(_) => "f64".to_string(),
            CheckedType::Bool(_) => "bool".to_string(),
            CheckedType::Char(_) => "char".to_string(),
            CheckedType::Array(inner_ty, _) => format!("[{}]", inner_ty.name()),
            CheckedType::SizedArray(inner_ty, size, _) => format!("[{}; {}]", inner_ty.name(), size),
            CheckedType::Optional(inner_ty, _) => format!("{}?", inner_ty.name()),
            CheckedType::RawPtr(inner_ty, _) => format!("raw {}", inner_ty.name()),
            CheckedType::WeakPtr(inner_ty, _) => format!("weak {}", inner_ty.name()),
        }
    }
}
#[derive(Debug, Clone)] pub enum CheckedStatement {
    Function(CheckedFunction)
}
#[derive(Debug, Clone)] pub struct CheckedFunction {
    pub name: String,
    // pub parameters: Vec<CheckedParameter>,
    pub return_type: CheckedType,
    pub body: Vec<CheckedStatement>,
}
#[derive(Debug, Clone)] pub struct CheckedAST {
    pub statements: Vec<CheckedStatement>
}
#[derive(Debug, Clone)] pub struct Typechecker {
    pub ast: ParsedAST
}
impl Typechecker {
    pub fn new(ast: ParsedAST) -> Typechecker {
        Typechecker { ast }
    }
    pub fn typecheck(&self) -> Result<CheckedAST, Vec<OnyxError>> {
        let mut statements: Vec<CheckedStatement> = vec![];
        let mut errors: Vec<OnyxError> = vec![];
        for statement in &self.ast.statements {
            match statement {
                ParsedFirstClassStatement::Function(function) => {
                    let mut return_type: CheckedType;
                    if function.name.clone() == "main" {
                        return_type = CheckedType::I32(function.return_type.clone().span());
                    } else if let ParsedType::Empty(span) = function.return_type.clone() {
                        let body: ParsedBody = function.body.clone();
                        let mut type_: Option<CheckedType> = None;
                        match body {
                            ParsedBody::Expression(expression) => match expression {
                                ParsedExpression::Number(number, span) => type_ = Some(self.get_checked_type_from_i64(number, span)),
                                _ => {}
                            }
                            ParsedBody::Block(block) => {
                                for statement in block.stmts {
                                    match statement.clone() {
                                        ParsedStatement::Return(expression, span) => {
                                            if expression.is_none() {
                                                type_ = Some(CheckedType::Void(span));
                                                break;
                                            } else {
                                                match expression.unwrap() {
                                                    ParsedExpression::Number(number, span) => type_ = Some(self.get_checked_type_from_i64(number, span)),
                                                    _ => {}
                                               }
                                               break;
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                            }
                            ParsedBody::Empty => {}
                        }
                        if let Some(type_) = type_ {
                            return_type = type_;
                        } else {
                            return_type = CheckedType::Void(function.return_type.clone().span())
                        }
                    } else {
                        return_type = self.typecheck_type(&function.return_type);
                        let body: ParsedBody = function.body.clone();
                        match body {
                            ParsedBody::Expression(expression) => match expression {
                                ParsedExpression::Number(number, span) => {
                                    let type_: CheckedType = self.get_checked_type_from_i64(number, span.clone());
                                    if type_ != return_type {
                                        errors.push(OnyxError::TypeError(format!("Type mismatch: expected {}, got {}", return_type.name(), type_.name()), span.clone()));
                                    }
                                },
                                _ => {}
                            }
                            ParsedBody::Block(block) => {
                                for statement in block.stmts {
                                    match statement.clone() {
                                        ParsedStatement::Return(expression, span) => {
                                            if expression.is_none() {
                                                if let CheckedType::Void(_) = return_type {
                                                    continue;
                                                } else {
                                                    errors.push(OnyxError::TypeError(format!("Type mismatch: expected {}, got {}", return_type.name(), CheckedType::Void(span.clone()).name()), span.clone()));
                                                }
                                            } else {
                                                match expression.unwrap() {
                                                    ParsedExpression::Number(number, span) => {
                                                        let type_: CheckedType = self.get_checked_type_from_i64(number, span.clone());
                                                        if type_ != return_type {
                                                            errors.push(OnyxError::TypeError(format!("Type mismatch: expected {}, got {}", return_type.name(), type_.name()), span.clone()));
                                                        }
                                                    },
                                                    _ => {}
                                                }
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                            }
                            ParsedBody::Empty => {}
                        }
                    }
                    statements.push(CheckedStatement::Function(CheckedFunction {
                        name: function.name.clone(),
                        // parameters: vec![],
                        return_type,
                        body: vec![]
                    }));
                }
                _ => {}
            }
        }
        if errors.len() > 0 {
            return Err(errors);
        }
        Ok(CheckedAST { statements })
    }
    fn typecheck_type(&self, parsed_type: &ParsedType) -> CheckedType {
        match parsed_type {
            ParsedType::Name(name, span) => {
                match name.as_str() {
                    "void" => CheckedType::Void(span.clone()),
                    "i8" => CheckedType::I8(span.clone()),
                    "i16" => CheckedType::I16(span.clone()),
                    "i32" => CheckedType::I32(span.clone()),
                    "i64" => CheckedType::I64(span.clone()),
                    "u8" => CheckedType::U8(span.clone()),
                    "u16" => CheckedType::U16(span.clone()),
                    "u32" => CheckedType::U32(span.clone()),
                    "u64" => CheckedType::U64(span.clone()),
                    "f32" => CheckedType::F32(span.clone()),
                    "f64" => CheckedType::F64(span.clone()),
                    "bool" => CheckedType::Bool(span.clone()),
                    "char" => CheckedType::Char(span.clone()),
                    _ => todo!("Typechecker: typecheck_type: ParsedType::Name")
                }
            }
            ParsedType::GenericType(_name, _generic_type, _span) => todo!("Typechecker: typecheck_type: ParsedType::GenericType"),
            ParsedType::Array(array_type, span) => CheckedType::Array(Box::new(self.typecheck_type(array_type)), span.clone()),
            ParsedType::SizedArray(array_type, size, span) => CheckedType::SizedArray(Box::new(self.typecheck_type(array_type)), *size, span.clone()),
            ParsedType::Optional(optional_type, span) => CheckedType::Optional(Box::new(self.typecheck_type(optional_type)), span.clone()),
            ParsedType::RawPtr(raw_ptr_type, span) => CheckedType::RawPtr(Box::new(self.typecheck_type(raw_ptr_type)), span.clone()),
            ParsedType::WeakPtr(weak_ptr_type, span) => CheckedType::WeakPtr(Box::new(self.typecheck_type(weak_ptr_type)), span.clone()),
            ParsedType::Empty(span) => CheckedType::Void(span.clone())
        }
    }
    fn get_checked_type_from_i64(&self, number: i64, span: Span) -> CheckedType {
        if number >= i8::MIN as i64 && number <= i8::MAX as i64 {
            CheckedType::I8(span.clone())
        } else if number >= i16::MIN as i64 && number <= i16::MAX as i64 {
            CheckedType::I16(span.clone())
        } else if number >= i32::MIN as i64 && number <= i32::MAX as i64 {
            CheckedType::I32(span.clone())
        } else if number >= i64::MIN as i64 && number <= i64::MAX as i64 {
            CheckedType::I64(span.clone())
        } else {
            todo!("Typechecker: get_checked_type_from_i64: number out of range")
        }
    }
} 