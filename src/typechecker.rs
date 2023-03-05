use crate::{
    parser::{ParsedAST, ParsedFirstClassStatement, ParsedType, ParsedBody, ParsedExpression, ParsedStatement, Visibility, ParsedFunction, ParsedClass},
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
    Class(String, Span),
    Enum(String, Span),
    TypeAlias(String, Span),
}
impl CheckedType {
    pub fn span(&self) -> Span {
        match self {
            CheckedType::Void(s) => s.clone(),
            CheckedType::I8(s) => s.clone(),
            CheckedType::I16(s) => s.clone(),
            CheckedType::I32(s) => s.clone(),
            CheckedType::I64(s) => s.clone(),
            CheckedType::U8(s) => s.clone(),
            CheckedType::U16(s) => s.clone(),
            CheckedType::U32(s) => s.clone(),
            CheckedType::U64(s) => s.clone(),
            CheckedType::F32(s) => s.clone(),
            CheckedType::F64(s) => s.clone(),
            CheckedType::Bool(s) => s.clone(),
            CheckedType::Char(s) => s.clone(),
            CheckedType::Array(_, s) => s.clone(),
            CheckedType::SizedArray(_, _, s) => s.clone(),
            CheckedType::Optional(_, s) => s.clone(),
            CheckedType::RawPtr(_, s) => s.clone(),
            CheckedType::WeakPtr(_, s) => s.clone(),
            CheckedType::Class(_, s) => s.clone(),
            CheckedType::Enum(_, s) => s.clone(),
            CheckedType::TypeAlias(_, s) => s.clone(),
        }
    }
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
            CheckedType::Class(name, _) => name.clone(),
            CheckedType::Enum(name, _) => name.clone(),
            CheckedType::TypeAlias(name, _) => name.clone(),
        }
    }
}
#[derive(Debug, Clone)] pub enum CheckedStatement {
    Class(CheckedClass),
    Function(CheckedFunction)
}
#[derive(Debug, Clone)] pub enum CheckedExpression {
    IntegerLiteral(i64, Span),
}
#[derive(Debug, Clone)] pub struct CheckedFunction {
    pub name: String,
    pub parameters: Vec<CheckedParameter>,
    pub return_type: CheckedType,
    pub body: Vec<CheckedStatement>,
}
#[derive(Debug, Clone)] pub struct CheckedParameter {
    pub name: String,
    pub ty: CheckedType,
    pub initializer: Option<CheckedExpression>,
    pub is_named: bool,
}
#[derive(Debug, Clone)] pub struct CheckedClass {
    pub name: String,
    pub generic_parameters: Vec<(String, Span)>,
    pub parent_class: Option<String>,
    pub parent_generic_parameters: Option<Vec<(String, Span)>>,
    // pub fields: Vec<CheckedField>,
    pub methods: Vec<CheckedMethod>,
}
#[derive(Debug, Clone)] pub struct CheckedField {
    // pub base_declaration: ParsedVariableDeclaration,
    pub visibility: Visibility,
}
#[derive(Debug, Clone)] pub struct CheckedMethod {
    pub base_declaration: CheckedFunction,
    pub visibility: Visibility,
    pub is_virtual: bool,
    pub is_override: bool,
    pub is_static: bool,
}
#[derive(Debug, Clone)] pub struct CheckedAST {
    pub statements: Vec<CheckedStatement>
}
#[derive(Debug, Clone)] pub struct Typechecker {
    pub ast: ParsedAST,
    pub functions: Vec<CheckedFunction>,
    pub classes: Vec<CheckedClass>,
    // pub enums: Vec<CheckedEnum>,
    // pub type_aliases: Vec<CheckedTypeAlias>,
    pub current_function: Option<ParsedFunction>,
    pub current_class: Option<ParsedClass>,
    // pub current_enum: Option<CheckedEnum>,
    // pub current_type_alias: Option<CheckedTypeAlias>,
}
impl Typechecker {
    pub fn new(ast: ParsedAST) -> Typechecker {
        Typechecker {
            ast,
            functions: vec![],
            classes: vec![],
            // enums: vec![],
            // type_aliases: vec![],
            current_function: None,
            current_class: None,
            // current_enum: None,
            // current_type_alias: None,
        }
    }
    pub fn typecheck(&mut self) -> Result<CheckedAST, Vec<OnyxError>> {
        let mut statements: Vec<CheckedStatement> = vec![];
        let mut errors: Vec<OnyxError> = vec![];
        for statement in &self.ast.statements {
            match statement {
                ParsedFirstClassStatement::Function(function) => {
                    self.current_function = Some(function.clone());
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
                                        _ => todo!("typecheck function body")
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
                                    if let Err(err) = self.test_types(type_.clone(), return_type.clone()) {
                                        errors.push(err);
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
                                                    errors.push(OnyxError::TypeError(format!("type {} is not void", return_type.name()), span.clone()));
                                                }
                                            } else {
                                                match expression.unwrap() {
                                                    ParsedExpression::Number(number, span) => {
                                                        let type_: CheckedType = self.get_checked_type_from_i64(number, span.clone());
                                                        if let Err(err) = self.test_types(type_.clone(), return_type.clone()) {
                                                            errors.push(err);
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
                    self.current_function = None;
                    statements.push(CheckedStatement::Function(CheckedFunction {
                        name: function.name.clone(),
                        parameters: vec![],
                        return_type,
                        body: vec![]
                    }));
                }
                ParsedFirstClassStatement::Class(class) => {
                    self.current_class = Some(class.clone());
                    let this_type: CheckedType = CheckedType::Class(class.name.clone(), class.name_span.clone());

                    for _ in class.fields.clone() {
                        todo!("Typecheck fields");
                    }

                    let mut methods: Vec<CheckedMethod> = vec![];
                    for method in class.methods.clone() {
                        let mut return_type: CheckedType;
                        let mut parameters: Vec<CheckedParameter> = vec![];
                        let mut is_static: bool = true;
                        for parameter in method.base_declaration.parameters.clone() {
                            if parameter.name.clone() == "this" {
                                is_static = false;
                                parameters.push(CheckedParameter {
                                    name: parameter.name.clone(),
                                    ty: this_type.clone(),
                                    initializer: None,
                                    is_named: false
                                });
                            } else {
                                let initializer: Option<CheckedExpression> = match parameter.initializer.clone() {
                                    Some(expression) => Some(self.typecheck_expression(&expression)),
                                    None => None
                                };
                                let ty = match parameter.parameter_type {
                                    ParsedType::Empty(span) => {
                                        if initializer.clone().is_none() {
                                            match method.base_declaration.body {
                                                ParsedBody::Block(_) => todo!("typecheck parameter type"),
                                                _ => {
                                                    errors.push(OnyxError::TypeErrorWithHint(
                                                        format!("cannot infer type of parameter {}", parameter.name), span.clone(),
                                                        "try specifying the type for the parameter".to_string(), span.clone()
                                                    ));
                                                    CheckedType::Void(span.clone())
                                                }
                                            }
                                        } else {
                                            let initializer: CheckedExpression = initializer.clone().unwrap();
                                            match initializer {
                                                CheckedExpression::IntegerLiteral(number, span) => self.get_checked_type_from_i64(number, span),
                                                _ => todo!("typecheck parameter type")
                                            }
                                        }
                                    }
                                    _ => self.typecheck_type(&parameter.parameter_type)
                                };
                                parameters.push(CheckedParameter {
                                    name: parameter.name.clone(),
                                    ty,
                                    initializer,
                                    is_named: parameter.is_named
                                });
                            }
                        }

                        methods.push(CheckedMethod {
                            base_declaration: CheckedFunction {
                                name: method.base_declaration.name.clone(),
                                parameters,
                                return_type: self.typecheck_type(&method.base_declaration.return_type),
                                body: vec![]
                            },
                            visibility: method.visibility,
                            is_override: method.is_override,
                            is_virtual: method.is_virtual,
                            is_static
                        });
                    }


                    self.current_class = None;
                    let class = CheckedClass {
                        name: class.name.clone(),
                        generic_parameters: vec![],
                        methods,
                        parent_class: None,
                        parent_generic_parameters: None,
                    };
                    statements.push(CheckedStatement::Class(class.clone()));
                    self.classes.push(class);
                }
                _ => {}
            }
        }
        if errors.len() > 0 {
            return Err(errors);
        }
        Ok(CheckedAST { statements })
    }
    fn typecheck_expression(&self, expression: &ParsedExpression) -> CheckedExpression {
        match expression {
            ParsedExpression::Number(number, span) => CheckedExpression::IntegerLiteral(*number, span.clone()),
            _ => todo!("Typecheck expression: {:?}", expression)
        }
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
    fn test_types(&self, t: CheckedType, return_type: CheckedType) -> Result<(), OnyxError> {

        match return_type {
            CheckedType::I8(_) => match t {
                CheckedType::I8(_) => Ok(()),
                _ => Err(OnyxError::TypeError(format!("type {} is not compatible with type {}", t.name(), return_type.name()), t.span()))
            },
            CheckedType::I16(_) => match t {
                CheckedType::I8(_) | CheckedType::I16(_) => Ok(()),
                _ => Err(OnyxError::TypeError(format!("type {} is not compatible with type {}", t.name(), return_type.name()), t.span()))
            },
            CheckedType::I32(_) => match t {
                CheckedType::I8(_) | CheckedType::I16(_) | CheckedType::I32(_) => Ok(()),
                _ => Err(OnyxError::TypeError(format!("type {} is not compatible with type {}", t.name(), return_type.name()), t.span()))
            },
            CheckedType::I64(_) => match t {
                CheckedType::I8(_) | CheckedType::I16(_) | CheckedType::I32(_) | CheckedType::I64(_) => Ok(()),
                _ => Err(OnyxError::TypeError(format!("type {} is not compatible with type {}", t.name(), return_type.name()), t.span()))
            },
            CheckedType::U8(_) => match t {
                CheckedType::U8(_) => Ok(()),
                _ => Err(OnyxError::TypeError(format!("type {} is not compatible with type {}", t.name(), return_type.name()), t.span()))
            },
            CheckedType::U16(_) => match t {
                CheckedType::U8(_) | CheckedType::U16(_) => Ok(()),
                _ => Err(OnyxError::TypeError(format!("type {} is not compatible with type {}", t.name(), return_type.name()), t.span()))
            },
            CheckedType::U32(_) => match t {
                CheckedType::U8(_) | CheckedType::U16(_) | CheckedType::U32(_) => Ok(()),
                _ => Err(OnyxError::TypeError(format!("type {} is not compatible with type {}", t.name(), return_type.name()), t.span()))
            },
            CheckedType::U64(_) => match t {
                CheckedType::U8(_) | CheckedType::U16(_) | CheckedType::U32(_) | CheckedType::U64(_) => Ok(()),
                _ => Err(OnyxError::TypeError(format!("type {} is not compatible with type {}", t.name(), return_type.name()), t.span()))
            },
            CheckedType::F32(_) => match t {
                CheckedType::F32(_) => Ok(()),
                _ => Err(OnyxError::TypeError(format!("type {} is not compatible with type {}", t.name(), return_type.name()), t.span()))
            },
            CheckedType::F64(_) => match t {
                CheckedType::F32(_) | CheckedType::F64(_) => Ok(()),
                _ => Err(OnyxError::TypeError(format!("type {} is not compatible with type {}", t.name(), return_type.name()), t.span()))
            },
            _ => {
                if t == return_type {
                    Ok(())
                } else {
                    Err(OnyxError::TypeError(format!("type {} is not compatible with type {}", t.name(), return_type.name()), t.span()))
                }
            }
        }
    }
} 