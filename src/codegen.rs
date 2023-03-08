use std::collections::HashMap;

use crate::{
    error::OnyxError,
    typechecker::{
        CheckedStatement,
        CheckedExpression,
        CheckedBlock,
        CheckedParameter,
        Type, CheckedAST, CheckedBody, CheckedEnumVariant, CheckedField, CheckedMethod,
    }, parser::Visibility
};

pub trait Codegen {
    fn codegen_ast(&mut self, ast: CheckedAST) -> Result<String, OnyxError>;
    fn codegen_statement(&mut self, statement: CheckedStatement) -> Result<(), OnyxError>;
    fn codegen_field(&mut self, field: CheckedField) -> Result<(), OnyxError>;
    fn codegen_method(&mut self, method: CheckedMethod) -> Result<(), OnyxError>;
    fn codegen_expression(&mut self, expression: CheckedExpression) -> Result<(), OnyxError>;
    fn codegen_type(&mut self, t: Type) -> Result<(), OnyxError>;
    fn codegen_block(&mut self, block: CheckedBlock) -> Result<(), OnyxError>;
    fn codegen_parameter(&mut self, parameter: CheckedParameter) -> Result<(), OnyxError>;
    fn codegen_body(&mut self, block: CheckedBody) -> Result<(), OnyxError>;

    fn write(&mut self, s: &str) -> ();
    fn writeln(&mut self, s: &str) -> ();
}

#[derive(Debug, Clone)] pub struct CppCodegen {
    pub filename: String,
    pub output: String,
    pub header: String,
    pub indent: usize,
    includes: Vec<String>,

    functions: Vec<String>,
    classes: Vec<String>,
    enums: Vec<String>,
}

#[derive(Debug, Clone)] pub struct RustCodegen {
    pub output: String,
    pub indent: usize,
}

impl CppCodegen {
    pub fn new(filename: String) -> Self {
        Self {
            filename,
            output: String::new(),
            header: String::new(),
            indent: 0,
            includes: Vec::new(),
            functions: Vec::new(),
            classes: Vec::new(),
            enums: Vec::new(),
        }
    }

    pub fn codegen(&mut self, ast: &CheckedAST) -> Result<String, OnyxError> {
        self.output.push_str("#include <variant>\n");
        self.output.push_str(format!("#include \"{}\"\n", self.filename.replace(".onyx", ".hpp")).as_str());
        Ok(self.codegen_ast(ast.clone())?)
    }
}

impl Codegen for CppCodegen {
    fn codegen_ast(&mut self, ast: CheckedAST) -> Result<String, OnyxError> {
        for statement in &ast.statements {
            self.codegen_statement(statement.clone())?;
        }
        Ok(self.output.clone())
    }
    fn codegen_statement(&mut self, statement: CheckedStatement) -> Result<(), OnyxError> {
        match statement.clone() {
            CheckedStatement::Function(function) => {
                self.codegen_type(function.return_type)?;
                self.write(" ");
                self.write(&function.name);
                self.write("(");
                for (i, parameter) in function.parameters.iter().enumerate() {
                    self.codegen_parameter(parameter.clone())?;
                    if i != function.parameters.len() - 1 {
                        self.write(", ");
                    }
                }
                self.write(")");
                self.codegen_body(function.body)?;
                self.functions.push(function.name.clone());
            }
            CheckedStatement::Enum(enum_) => {
                self.includes.push("variant".to_string());
                if enum_.is_boxed {
                    ()
                } else {
                    for (i, variant) in enum_.variants.iter().enumerate() {
                        match variant {
                            CheckedEnumVariant::Tuple(name, _, types) => {
                                ()
                            }
                            CheckedEnumVariant::Unit(name, _) => {
                                self.write("struct ");
                                self.write(&name);
                                self.writeln(" {};");
                            }
                            CheckedEnumVariant::Struct(name, _, fields) => {
                                for field in fields {
                                    if let Type::Unknown(name) = field.1.clone() {
                                        if enum_.generic_parameters.iter().any(|x| x.0 == name) {
                                            self.write("template<");
                                            for (i, parameter) in enum_.generic_parameters.iter().enumerate() {
                                                if name != parameter.0 {
                                                    continue;
                                                }
                                                self.write("typename ");
                                                self.write(&parameter.0);
                                                self.write(", ");
                                            }
                                            self.output.pop();
                                            self.output.pop();
                                            self.write("> ");
                                        }
                                    }
                                }
                                self.write("struct ");
                                self.write(&name);
                                self.write(" {");
                                for (i, field) in fields.iter().enumerate() {
                                    self.codegen_type(field.1.clone())?;
                                    self.write(" ");
                                    self.write(&field.0);
                                    if i != fields.len() - 1 {
                                        self.write(", ");
                                    }
                                }
                                self.writeln("};");
                            }
                        }   
                    }

                    if enum_.generic_parameters.len() > 0 {
                        self.write("template<");
                        for (i, parameter) in enum_.generic_parameters.iter().enumerate() {
                            self.write("typename ");
                            self.write(&parameter.0);
                            if i != enum_.generic_parameters.len() - 1 {
                                self.write(", ");
                            }
                        }
                        self.writeln(">");
                    }
                    self.write("using ");
                    self.write(&enum_.name);
                    self.write(" = std::variant<");
                    for (i, variant) in enum_.variants.iter().enumerate() {
                        match variant {
                            CheckedEnumVariant::Tuple(name, _, types) => {
                                self.write("std::tuple<");
                                for (i, t) in types.iter().enumerate() {
                                    self.codegen_type(t.0.clone())?;
                                    if i != types.len() - 1 {
                                        self.write(", ");
                                    }
                                }
                                self.write(">");
                            }
                            CheckedEnumVariant::Unit(name, _) => {
                                self.write(&name);
                            }
                            CheckedEnumVariant::Struct(name, _, fields) => {
                                self.write(&name);
                                self.write("<");
                                for (i, field) in fields.iter().enumerate() {
                                    if let Type::Unknown(name) = field.1.clone() {
                                        if enum_.generic_parameters.iter().any(|x| x.0 == name) {
                                            self.write(&name);
                                        } else {
                                            self.codegen_type(field.1.clone())?;
                                        }
                                    } else {
                                        self.codegen_type(field.1.clone())?;
                                    }
                                    if i != fields.len() - 1 {
                                        self.write(", ");
                                    }
                                }
                                self.write(">");
                            }
                        }
                        if i != enum_.variants.len() - 1 {
                            self.write(", ");
                        }
                    }
                    self.writeln(">;");
                    self.enums.push(enum_.name.clone());
                }
            }
            CheckedStatement::Class(class) => {
                self.write("class ");
                self.write(&class.name);
                self.write(" {\n");
                let mut public_fields: Vec<CheckedField> = Vec::new();
                let mut private_fields: Vec<CheckedField> = Vec::new();
                let mut protected_fields: Vec<CheckedField> = Vec::new();
                for field in &class.fields {
                    match field.visibility {
                        Visibility::Public => public_fields.push(field.clone()),
                        Visibility::Private => private_fields.push(field.clone()),
                        Visibility::Protected => protected_fields.push(field.clone()),
                    }
                }
                let mut public_methods: Vec<CheckedMethod> = Vec::new();
                let mut private_methods: Vec<CheckedMethod> = Vec::new();
                let mut protected_methods: Vec<CheckedMethod> = Vec::new();
                for method in &class.methods {
                    match method.visibility {
                        Visibility::Public => public_methods.push(method.clone()),
                        Visibility::Private => private_methods.push(method.clone()),
                        Visibility::Protected => protected_methods.push(method.clone()),
                    }
                }
                if public_fields.len() > 0 || public_methods.len() > 0 {
                    self.writeln("public:");
                    self.indent += 1;
                    for field in public_fields {
                        self.codegen_field(field)?;
                    }
                    for method in public_methods {
                        self.codegen_method(method)?;
                    }
                    self.indent -= 1;
                }
                if private_fields.len() > 0 || private_methods.len() > 0 {
                    self.writeln("private:");
                    self.indent += 1;
                    for field in private_fields {
                        self.codegen_field(field)?;
                    }
                    for method in private_methods {
                        self.codegen_method(method)?;
                    }
                    self.indent -= 1;
                }
                if protected_fields.len() > 0 || protected_methods.len() > 0 {
                    self.writeln("protected:");
                    self.indent += 1;
                    for field in protected_fields {
                        self.codegen_field(field)?;
                    }
                    for method in protected_methods {
                        self.codegen_method(method)?;
                    }
                    self.indent -= 1;
                }
                self.writeln("};");
            }
            CheckedStatement::Expression(expression) => {
                self.codegen_expression(expression)?;
                self.writeln(";");
            }
            _ => {}
        }
        Ok(())
    }
    fn codegen_field(&mut self, field: CheckedField) -> Result<(), OnyxError> {
        self.codegen_type(field.field_type.clone())?;
        self.write(" ");
        self.write(&field.name);
        self.writeln(";");
        Ok(())
    }
    fn codegen_method(&mut self, method: CheckedMethod) -> Result<(), OnyxError> {
        self.codegen_type(method.return_type.clone())?;
        self.write(" ");
        self.write(&method.name);
        self.write("(");
        for (i, parameter) in method.parameters.iter().enumerate() {
            self.codegen_parameter(parameter.clone())?;
            if i != method.parameters.len() - 1 {
                self.write(", ");
            }
        }
        self.writeln(") {");
        self.indent += 1;
        self.codegen_body(method.body.clone())?;
        self.indent -= 1;
        self.writeln("}");
        Ok(())
    }
    fn codegen_expression(&mut self, expression: CheckedExpression) -> Result<(), OnyxError> {
        match expression.clone() {
            CheckedExpression::MemberAccess(expression, member, _) => {
                self.codegen_expression(*expression)?;
                self.write(".");
                self.codegen_expression(*member)?;
            }
            CheckedExpression::Assignment(left, right, _) => {
                self.codegen_expression(*left)?;
                self.write(" = ");
                self.codegen_expression(*right)?;
            }
            CheckedExpression::Call(name, parameters, _) => {
                self.codegen_expression(*name)?;
                self.write("(");
                for (i, parameter) in parameters.iter().enumerate() {
                    self.codegen_expression(parameter.clone())?;
                    if i != parameters.len() - 1 {
                        self.write(", ");
                    }
                }
                self.write(")");
            }
            CheckedExpression::Identifier(name, _) => {
                self.write(&name);
            }
            _ => {}
        }
        Ok(())
    }
    fn codegen_block(&mut self, block: CheckedBlock) -> Result<(), OnyxError> {
        Ok(())
    }
    fn codegen_parameter(&mut self, parameter: CheckedParameter) -> Result<(), OnyxError> {
        Ok(())
    }
    fn codegen_type(&mut self, t: Type) -> Result<(), OnyxError> {
        match t.clone() {
            Type::Char => self.write("char"),
            Type::Bool => self.write("bool"),
            Type::I8 => self.write("int8_t"),
            Type::I16 => self.write("int16_t"),
            Type::I32 => self.write("int32_t"),
            Type::I64 => self.write("int64_t"),
            Type::U8 => self.write("uint8_t"),
            Type::U16 => self.write("uint16_t"),
            Type::U32 => self.write("uint32_t"),
            Type::U64 => self.write("uint64_t"),
            Type::F32 => self.write("float"),
            Type::F64 => self.write("double"),
            Type::Usize => self.write("size_t"),
            Type::Void => self.write("void"),
            Type::Array(t) => {
                self.codegen_type(*t)?;
                self.write("*");
            }
            Type::SizedArray(t, size) => {
                self.codegen_type(*t)?;
                self.write("[");
                self.write(&size.to_string());
                self.write("]");
            }
            Type::Optional(t) => {
                self.write("std::optional<");
                self.codegen_type(*t)?;
                self.write(">");
            }
            Type::RawPtr(t) => {
                self.codegen_type(*t)?;
                self.write("*");
            }
            Type::WeakPtr(t) => {
                self.write("std::weak_ptr<");
                self.codegen_type(*t)?;
                self.write(">");
            }
            Type::Class(name) | Type::Enum(name) => self.write(&name),
            Type::Generic(name, types) => {
                self.write(&name);
                self.write("<");
                for (i, t) in types.iter().enumerate() {
                    self.codegen_type(t.clone())?;
                    if i != types.len() - 1 {
                        self.write(", ");
                    }
                }
                self.write(">");
            }
            Type::Unknown(name) => self.write(&name),
        }
        Ok(())
    }
    fn codegen_body(&mut self, block: CheckedBody) -> Result<(), OnyxError> {
        match block {
            CheckedBody::Block(block) => {
                self.writeln("{");
                self.indent += 1;
                for statement in block.statements {
                    self.codegen_statement(statement)?;
                }
                self.indent -= 1;
                self.writeln("}");
            }
            CheckedBody::Expression(expression) => {
                self.write("return ");
                self.codegen_expression(expression)?;
                self.writeln(";");
            }
        }
        Ok(())
    }
    fn write(&mut self, s: &str) -> () {
        self.output.push_str(s);
    }
    fn writeln(&mut self, s: &str) -> () {
        self.output.push_str(s);
        self.output.push_str("\n");
    }
}

impl RustCodegen {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            indent: 0,
        }
    }
}
