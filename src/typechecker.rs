use std::{collections::HashMap, fmt::format};
use crate::{
    span::Span,
    parser::{ParsedAST, ParsedFirstClassStatement, ParsedType, ParsedExpression, ParsedBody, ParsedStatement, EnumVariant, Visibility},
    error::OnyxError,
};
pub type ScopeId = usize;
#[derive(Debug, Clone, PartialEq)] pub enum SafetyMode { Safe, Unsafe }
#[derive(Debug, Clone, PartialEq)] pub enum Type {
    Char,
    Bool,
    I8, I16, I32, I64, // maybe: I128,
    U8, U16, U32, U64, // maybe: U128,
    F32, F64, // maybe: F128,
    Usize,
    Void,
    Array(Box<Type>),
    SizedArray(Box<Type>, usize),
    Optional(Box<Type>),
    RawPtr(Box<Type>),
    WeakPtr(Box<Type>),
    Class(String),
    Enum(String),
    Generic(String, Vec<Type>),
    Unknown(String),
}
impl Type {
    pub fn to_string(&self) -> String {
        match self {
            Type::Char => "char".to_string(),
            Type::Bool => "bool".to_string(),
            Type::I8 => "i8".to_string(),
            Type::I16 => "i16".to_string(),
            Type::I32 => "i32".to_string(),
            Type::I64 => "i64".to_string(),
            Type::U8 => "u8".to_string(),
            Type::U16 => "u16".to_string(),
            Type::U32 => "u32".to_string(),
            Type::U64 => "u64".to_string(),
            Type::F32 => "f32".to_string(),
            Type::F64 => "f64".to_string(),
            Type::Usize => "usize".to_string(),
            Type::Void => "void".to_string(),
            Type::Array(t) => format!("{}[]", t.to_string()),
            Type::SizedArray(t, size) => format!("{}[{}]", t.to_string(), size),
            Type::Optional(t) => format!("{}?", t.to_string()),
            Type::RawPtr(t) => format!("{}*", t.to_string()),
            Type::WeakPtr(t) => format!("{}&", t.to_string()),
            Type::Class(name) => name.clone(),
            Type::Enum(name) => name.clone(),
            Type::Generic(name, generics) => {
                let mut generics_string: String = "".to_string();
                for generic in generics {
                    generics_string.push_str(&format!("{}, ", generic.to_string()));
                }
                generics_string.pop();
                generics_string.pop();
                format!("{}<{}>", name, generics_string)
            }
            Type::Unknown(name) => name.clone(),
        }
    }
}
#[derive(Debug, Clone)] pub struct CheckedAST {
    pub statements: Vec<CheckedStatement>,
}
#[derive(Debug, Clone)] pub enum CheckedStatement {
    Function(CheckedFunction),
    Enum(CheckedEnum),
    Class(CheckedClass),
    VariableDeclaration(CheckedVariable),
    Expression(CheckedExpression),
}
#[derive(Debug, Clone)] pub struct CheckedClass {
    pub name: String,
    pub name_span: Span,
    pub generic_parameters: Vec<(Type, Span)>,
    pub parent_class: Option<String>,
    pub parent_generic_parameters: Option<Vec<(Type, Span)>>,
    pub fields: Vec<CheckedField>,
    pub methods: Vec<CheckedMethod>,
}
#[derive(Debug, Clone)] pub struct CheckedField {
    pub name: String,
    pub name_span: Span,
    pub field_type: Type,
    pub initializer: Option<CheckedExpression>,
    pub mutable: bool,
    pub visibility: Visibility,
}
#[derive(Debug, Clone)] pub struct CheckedMethod {
    pub name: String,
    pub name_span: Span,
    pub generic_parameters: Vec<(Type, Span)>,
    pub parameters: Vec<CheckedParameter>,
    pub return_type: Type,
    pub body: CheckedBody,
    pub visibility: Visibility,
    pub is_virtual: bool,
    pub is_override: bool,
}
#[derive(Debug, Clone)] pub enum CheckedExpression {
    Integer8(i8, Span),
    Integer16(i16, Span),
    Integer32(i32, Span),
    Integer64(i64, Span),
    UnsignedInteger8(u8, Span),
    UnsignedInteger16(u16, Span),
    UnsignedInteger32(u32, Span),
    UnsignedInteger64(u64, Span),
    Float32(f32, Span),
    Float64(f64, Span),
    Char(char, Span),
    Bool(bool, Span),
    String(String, Span),
    Sizeof(Type, Span),
    Identifier(String, Span),
    MemberAccess(Box<CheckedExpression>, Box<CheckedExpression>, Span),
    Assignment(Box<CheckedExpression>, Box<CheckedExpression>, Span),
    Call(Box<CheckedExpression>, Vec<CheckedExpression>, Span),
}
impl CheckedExpression {
    pub fn span(&self) -> Span {
        match self {
            CheckedExpression::Integer8(_, span) => span.clone(),
            CheckedExpression::Integer16(_, span) => span.clone(),
            CheckedExpression::Integer32(_, span) => span.clone(),
            CheckedExpression::Integer64(_, span) => span.clone(),
            CheckedExpression::UnsignedInteger8(_, span) => span.clone(),
            CheckedExpression::UnsignedInteger16(_, span) => span.clone(),
            CheckedExpression::UnsignedInteger32(_, span) => span.clone(),
            CheckedExpression::UnsignedInteger64(_, span) => span.clone(),
            CheckedExpression::Float32(_, span) => span.clone(),
            CheckedExpression::Float64(_, span) => span.clone(),
            CheckedExpression::Char(_, span) => span.clone(),
            CheckedExpression::Bool(_, span) => span.clone(),
            CheckedExpression::String(_, span) => span.clone(),
            CheckedExpression::Sizeof(_, span) => span.clone(),
            CheckedExpression::Identifier(_, span) => span.clone(),
            CheckedExpression::MemberAccess(_, _, span) => span.clone(),
            CheckedExpression::Assignment(_, _, span) => span.clone(),
            CheckedExpression::Call(_, _, span) => span.clone(),
        }
    }
}
#[derive(Debug, Clone)] pub struct CheckedVariable {
    pub name: String,
    pub var_type: Type,
    pub initializer: Option<CheckedExpression>,
    pub mutable: bool,
    pub span: Span,
}
#[derive(Debug, Clone)] pub struct CheckedFunction {
    pub name: String,
    pub name_span: Span,
    pub generic_parameters: Vec<(Type, Span)>,
    pub parameters: Vec<CheckedParameter>,
    pub return_type: Type,
    pub body: CheckedBody,
}
#[derive(Debug, Clone)] pub enum CheckedBody {
    Block(CheckedBlock),
    Expression(CheckedExpression),
}
#[derive(Debug, Clone)] pub struct CheckedEnum {
    pub name: String,
    pub name_span: Span,
    pub generic_parameters: Vec<(String, Span)>,
    pub is_boxed: bool,
    pub variants: Vec<CheckedEnumVariant>,
}
#[derive(Debug, Clone)] pub enum CheckedEnumVariant {
    Unit(String, Span),
    Tuple(String, Span, Vec<(Type, Span)>),
    Struct(String, Span, Vec<(String, Type, Span)>),
}
#[derive(Debug, Clone)] pub struct CheckedBlock {
    pub statements: Vec<CheckedStatement>,
}
#[derive(Debug, Clone)] pub struct CheckedParameter {
    pub name: String,
    pub name_span: Span,
    pub parameter_type: Type,
    pub initializer: Option<CheckedExpression>,
    pub is_named: bool,
}
#[derive(Debug, Clone)] pub struct Scope {
    pub parent: Option<ScopeId>,
    pub variables: Vec<CheckedVariable>,
    pub functions: Vec<CheckedFunction>,
}
impl Scope {
    pub fn new(parent: Option<ScopeId>) -> Self {
        Self {
            parent,
            variables: vec![],
            functions: vec![],
        }
    }
}
#[derive(Debug, Clone)] pub struct Typechecker {
    pub ast: ParsedAST,
    pub scopes: HashMap<ScopeId, Scope>,
    pub current_scope: Scope,
    pub current_class: Option<String>,
    pub current_safety_context: SafetyMode,
}
impl Typechecker {
    pub fn new(ast: &ParsedAST) -> Self {
        let mut scopes: HashMap<ScopeId, Scope> = HashMap::new();
        scopes.insert(0, Scope::new(None));
        let current_scope: Scope = scopes.get(&(0 as usize)).unwrap().clone();
        Self {
            ast: ast.clone(),
            scopes,
            current_scope,
            current_class: None,
            current_safety_context: SafetyMode::Safe,
        }
    }
    pub fn typecheck(&mut self) -> Result<CheckedAST, Vec<OnyxError>> {
        let mut statements: Vec<CheckedStatement> = vec![];
        let mut errors: Vec<OnyxError> = vec![];
        for statement in self.ast.clone().statements {
            let checked_statement: Result<CheckedStatement, OnyxError> = self.typecheck_first_class_statement(statement);
            match checked_statement {
                Ok(checked_statement) => {
                    statements.push(checked_statement);
                }
                Err(error) => {
                    errors.push(error);
                }
            }
        }
        if errors.len() > 0 {
            return Err(errors);
        }
        Ok(CheckedAST { statements })
    }
    fn typecheck_first_class_statement(&mut self, statement: ParsedFirstClassStatement) -> Result<CheckedStatement, OnyxError> {
        match statement {
            ParsedFirstClassStatement::Function(function) => {
                self.new_scope();
                let mut parameters: Vec<CheckedParameter> = vec![];
                for parameter in function.parameters {
                    let mut checked_parameter: CheckedParameter = CheckedParameter {
                        name: parameter.name,
                        name_span: parameter.name_span,
                        parameter_type: self.typecheck_type(parameter.parameter_type.clone())?,
                        initializer: match parameter.initializer {
                            Some(initializer) => Some(self.typecheck_expression(initializer)?),
                            None => None,
                        },
                        is_named: parameter.is_named,
                    };
                    parameters.push(checked_parameter);
                }
                let checked_function: CheckedFunction = CheckedFunction {
                    name: function.name,
                    name_span: function.name_span,
                    generic_parameters: self.typecheck_generic_parameters(function.generic_parameters)?,
                    parameters,
                    return_type: self.typecheck_type(function.return_type)?,
                    body: self.typecheck_function_body(function.body)?,
                };
                self.end_scope();
                self.current_scope.functions.push(checked_function.clone());
                Ok(CheckedStatement::Function(checked_function))
            }
            ParsedFirstClassStatement::Enum(enum_) => {
                let mut variants: Vec<CheckedEnumVariant> = vec![];
                for variant in enum_.variants {
                    match variant {
                        EnumVariant::NoValues(name, span) => {
                            variants.push(CheckedEnumVariant::Unit(name, span));
                        }
                        EnumVariant::ValuesNoNamed(name, types, span) => {
                            let mut checked_types: Vec<(Type, Span)> = vec![];
                            for type_ in types {
                                checked_types.push((self.typecheck_type(type_)?, span.clone()));
                            }
                            variants.push(CheckedEnumVariant::Tuple(name, span.clone(), checked_types));
                        }
                        EnumVariant::ValuesNamed(name, values, span) => {
                            let mut checked_values: Vec<(String, Type, Span)> = vec![];
                            for value in values {
                                checked_values.push((value.0, self.typecheck_type(value.1)?, span.clone()));
                            }
                            variants.push(CheckedEnumVariant::Struct(name, span, checked_values));
                        }
                    }
                }
                let checked_enum: CheckedEnum = CheckedEnum {
                    name: enum_.name,
                    name_span: enum_.span,
                    generic_parameters: enum_.generic_parameters,
                    is_boxed: enum_.is_boxed,
                    variants,
                };
                Ok(CheckedStatement::Enum(checked_enum))
            }
            ParsedFirstClassStatement::Class(class) => {
                let mut checked_methods: Vec<CheckedMethod> = vec![];
                self.current_class = Some(class.name.clone());
                for method in class.methods {
                    self.new_scope();
                    let mut parameters: Vec<CheckedParameter> = vec![];
                    for parameter in method.base_declaration.parameters {
                        if parameter.name.clone() == "this".to_string() {
                            let checked_parameter: CheckedParameter = CheckedParameter {
                                name: parameter.name.clone(),
                                name_span: parameter.name_span.clone(),
                                parameter_type: Type::Class(class.name.clone()),
                                initializer: None,
                                is_named: parameter.is_named,
                            };
                            parameters.push(checked_parameter);
                        } else {
                            let checked_parameter: CheckedParameter = CheckedParameter {
                                name: parameter.name.clone(),
                                name_span: parameter.name_span.clone(),
                                parameter_type: self.typecheck_type(parameter.parameter_type)?,
                                initializer: match parameter.initializer {
                                    Some(initializer) => Some(self.typecheck_expression(initializer)?),
                                    None => None,
                                },
                                is_named: parameter.is_named,
                            };
                            parameters.push(checked_parameter);
                        }
                    }
                    let checked_method: CheckedMethod = CheckedMethod {
                        name: method.base_declaration.name,
                        name_span: method.base_declaration.name_span,
                        generic_parameters: self.typecheck_generic_parameters(method.base_declaration.generic_parameters)?,
                        parameters,
                        return_type: self.typecheck_type(method.base_declaration.return_type)?,
                        body: self.typecheck_function_body(method.base_declaration.body)?,
                        is_virtual: method.is_virtual,
                        is_override: method.is_override,
                        visibility: method.visibility,
                    };
                    self.end_scope();
                    self.current_class = None;
                    checked_methods.push(checked_method);
                }
                let mut checked_fields: Vec<CheckedField> = vec![];
                for field in class.fields {
                    checked_fields.push(CheckedField {
                        name: field.base_declaration.name,
                        name_span: field.base_declaration.span,
                        field_type: self.typecheck_type(field.base_declaration.var_type)?,
                        initializer: match field.base_declaration.initializer {
                            Some(initializer) => Some(self.typecheck_expression(initializer)?),
                            None => None,
                        },
                        mutable: field.base_declaration.mutable,
                        visibility: field.visibility,
                    });
                }
                let checked_class: CheckedClass = CheckedClass {
                    name: class.name,
                    name_span: class.name_span,
                    parent_class: class.parent_class,
                    parent_generic_parameters: match class.parent_generic_parameters {
                        Some(parent_generic_parameters) => Some(self.typecheck_generic_parameters(parent_generic_parameters)?),
                        None => None,
                    },
                    generic_parameters: self.typecheck_generic_parameters(class.generic_parameters)?,
                    methods: checked_methods,
                    fields: checked_fields,
                };
                Ok(CheckedStatement::Class(checked_class))
            }
            _ => {
                Err(OnyxError::TypeError("unimplemented".to_string(), statement.span()))
            }
        }
    }
    fn typecheck_generic_parameters(&mut self, parameters: Vec<(String, Span)>) -> Result<Vec<(Type, Span)>, OnyxError> {
        let mut checked_parameters: Vec<(Type, Span)> = vec![];
        for parameter in parameters {
            checked_parameters.push((Type::Generic(parameter.0, vec![]), parameter.1));
        }
        Ok(checked_parameters)
    }
    fn typecheck_statement(&mut self, statement: ParsedStatement) -> Result<CheckedStatement, OnyxError> {
        match statement {
            ParsedStatement::VariableDeclaration(variable) => {
                let variable_type: Type = self.typecheck_type(variable.var_type)?;
                let initializer: Option<CheckedExpression> = match variable.initializer {
                    Some(initializer) => Some(self.typecheck_expression(initializer)?),
                    None => None,
                };
                let expected_type: Type = self.get_expected_type_from_checked_expression(initializer.clone().unwrap())?;
                if variable_type != expected_type {
                    return Err(OnyxError::TypeError(format!("expected type {} but got {}", variable_type.to_string(), expected_type.to_string()), variable.span));
                }

                let checked_variable: CheckedVariable = CheckedVariable {
                    name: variable.name,
                    var_type: variable_type,
                    initializer,
                    mutable: variable.mutable,
                    span: variable.span,
                };
                self.current_scope.variables.push(checked_variable.clone());
                Ok(CheckedStatement::VariableDeclaration(checked_variable))
            }
            ParsedStatement::Expression(expression) => {
                let checked_expression: CheckedExpression = self.typecheck_expression(expression)?;
                Ok(CheckedStatement::Expression(checked_expression))
            }
            _ => Err(OnyxError::TypeError(format!("unimplemented: {:?}", statement), statement.span())),
        }
    }
    fn typecheck_expression(&mut self, expression: ParsedExpression) -> Result<CheckedExpression, OnyxError> {
        match expression {
            ParsedExpression::Number(number, span) => {
                if number >= i8::MIN as i64 && number <= i8::MAX as i64 {
                    Ok(CheckedExpression::Integer8(number as i8, span))
                } else if number >= i16::MIN as i64 && number <= i16::MAX as i64 {
                    Ok(CheckedExpression::Integer16(number as i16, span))
                } else if number >= i32::MIN as i64 && number <= i32::MAX as i64 {
                    Ok(CheckedExpression::Integer32(number as i32, span))
                } else if number >= i64::MIN as i64 && number <= i64::MAX as i64 {
                    Ok(CheckedExpression::Integer64(number as i64, span))
                } else {
                    Err(OnyxError::TypeError("number out of range".to_string(), span))
                }
            }
            ParsedExpression::Sizeof(t, span) => {
                let checked_type: Type = self.typecheck_type(t)?;
                if self.get_expected_type_from_checked_expression(CheckedExpression::Sizeof(checked_type.clone(), span.clone()))? != Type::Usize {
                    return Err(OnyxError::TypeError("sizeof must return usize".to_string(), span.clone()));
                }
                Ok(CheckedExpression::Sizeof(checked_type, span))
            }
            ParsedExpression::MemberAccess(expression, member, span) => {
                let checked_expression: CheckedExpression = self.typecheck_expression(*expression)?;
                let checked_member: CheckedExpression = self.typecheck_expression(*member)?;
                Ok(CheckedExpression::MemberAccess(Box::new(checked_expression), Box::new(checked_member), span))
            }
            ParsedExpression::Identifier(name, span) => {
                // TODO: check if variable exists
                Ok(CheckedExpression::Identifier(name, span))
            }
            ParsedExpression::Assignment(left, right, span) => {
                let checked_left: CheckedExpression = self.typecheck_expression(*left)?;
                let checked_right: CheckedExpression = self.typecheck_expression(*right)?;
                // TODO: check if left is assignable to right
                Ok(CheckedExpression::Assignment(Box::new(checked_left), Box::new(checked_right), span))
            }
            ParsedExpression::Call(callee, arguments, span) => {
                let checked_callee: CheckedExpression = self.typecheck_expression(*callee)?;
                let mut checked_arguments: Vec<CheckedExpression> = vec![];
                for argument in arguments {
                    checked_arguments.push(self.typecheck_expression(argument.1)?);
                }
                Ok(CheckedExpression::Call(Box::new(checked_callee), checked_arguments, span))
            }
            _ => Err(OnyxError::TypeError(format!("unimplemented: {:?}", expression), expression.span()))
        }
    }
    fn typecheck_function_body(&mut self, body: ParsedBody) -> Result<CheckedBody, OnyxError> {
        match body {
            ParsedBody::Block(block) => {
                let mut statements: Vec<CheckedStatement> = vec![];
                for statement in block.stmts {
                    let checked_statement: Result<CheckedStatement, OnyxError> = self.typecheck_statement(statement);
                    match checked_statement {
                        Ok(checked_statement) => {
                            statements.push(checked_statement);
                        }
                        Err(error) => {
                            return Err(error);
                        }
                    }
                }
                Ok(CheckedBody::Block(CheckedBlock { statements }))
            }
            ParsedBody::Expression(expression) => {
                let checked_expression: CheckedExpression = self.typecheck_expression(expression).unwrap();
                Ok(CheckedBody::Expression(checked_expression))
            }
            ParsedBody::Empty => {
                Ok(CheckedBody::Block(CheckedBlock { statements: vec![] }))
            }
        }
    }
    fn typecheck_type(&mut self, type_: ParsedType) -> Result<Type, OnyxError> {
        match type_ {
            ParsedType::Name(ref string, _) => match string.as_str() {
                "char" => Ok(Type::Char),
                "bool" => Ok(Type::Bool),
                "i8" => Ok(Type::I8),
                "i16" => Ok(Type::I16),
                "i32" => Ok(Type::I32),
                "i64" => Ok(Type::I64),
                "u8" => Ok(Type::U8),
                "u16" => Ok(Type::U16),
                "u32" => Ok(Type::U32),
                "u64" => Ok(Type::U64),
                "f32" => Ok(Type::F32),
                "f64" => Ok(Type::F64),
                "usize" => Ok(Type::Usize),
                "void" => Ok(Type::Void),
                _ => Ok(Type::Unknown(string.clone())),
            },
            ParsedType::Array(t, _) => Ok(Type::Array(Box::new(self.typecheck_type(*t)?))),
            ParsedType::SizedArray(t, size, _) => Ok(Type::SizedArray(Box::new(self.typecheck_type(*t)?), size)),
            ParsedType::Optional(t, _) => Ok(Type::Optional(Box::new(self.typecheck_type(*t)?))),
            ParsedType::RawPtr(t, _) => Ok(Type::RawPtr(Box::new(self.typecheck_type(*t)?))),
            ParsedType::WeakPtr(t, _) => Ok(Type::WeakPtr(Box::new(self.typecheck_type(*t)?))),
            ParsedType::GenericType(name, types, _) => {
                let mut checked_types: Vec<Type> = vec![];
                for type_ in types {
                    checked_types.push(self.typecheck_type(type_)?);
                }
                Ok(Type::Generic(name, checked_types))
            }
            ParsedType::Empty(_) => Err(OnyxError::TypeErrorWithHint("empty type found".to_string(), type_.span(), "did you forget to specify a type?".to_string(), type_.span())),
        }
    }
    fn get_expected_type_from_checked_expression(&mut self, expression: CheckedExpression) -> Result<Type, OnyxError> {
        match expression.clone() {
            CheckedExpression::Integer8(integer, _) => Ok(Type::I8),
            CheckedExpression::Integer16(integer, _) => Ok(Type::I16),
            CheckedExpression::Integer32(integer, _) => Ok(Type::I32),
            CheckedExpression::Integer64(integer, _) => Ok(Type::I64),
            CheckedExpression::UnsignedInteger8(integer, _) => Ok(Type::U8),
            CheckedExpression::UnsignedInteger16(integer, _) => Ok(Type::U16),
            CheckedExpression::UnsignedInteger32(integer, _) => Ok(Type::U32),
            CheckedExpression::UnsignedInteger64(integer, _) => Ok(Type::U64),
            CheckedExpression::Float32(float, _) => Ok(Type::F32),
            CheckedExpression::Float64(float, _) => Ok(Type::F64),
            CheckedExpression::Char(character, _) => Ok(Type::Char),
            CheckedExpression::Bool(boolean, _) => Ok(Type::Bool),
            CheckedExpression::String(string, _) => Ok(Type::Array(Box::new(Type::Char))),
            CheckedExpression::Sizeof(t, _) => Ok(Type::Usize),
            _ => Err(OnyxError::TypeError("unknown expression type".to_string(), expression.span()))
        }
    }
    fn new_scope(&mut self) {
        let old_scope_id: ScopeId = self.scopes.len();
        let scope_id: ScopeId = self.scopes.len();
        self.scopes.insert(scope_id, Scope::new(Some(old_scope_id)));
        self.current_scope = self.scopes.get(&scope_id).unwrap().clone();
        self.current_scope.variables = self.scopes.get(&old_scope_id).unwrap().variables.clone();
    }
    fn end_scope(&mut self) {
        let parent_id: ScopeId = self.current_scope.parent.clone().unwrap();
        let current_id: ScopeId = self.scopes.len();
        self.current_scope = self.scopes.get(&parent_id).unwrap().clone();
        self.scopes.remove(&current_id);
    }
}