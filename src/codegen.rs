use std::collections::HashMap;

use crate::{
    error::OnyxError,
    typechecker::{
        CheckedStatement,
        CheckedExpression,
        CheckedBlock,
        CheckedParameter,
        Type, CheckedAST, CheckedBody, CheckedEnumVariant, CheckedField, CheckedMethod, CheckedClass,
    }, parser::Visibility
};

pub trait Codegen {
    fn codegen_ast(&mut self, ast: CheckedAST) -> Result<String, OnyxError>;
    fn codegen_statement(&mut self, statement: CheckedStatement) -> Result<(), OnyxError>;
    fn codegen_field(&mut self, field: CheckedField) -> Result<(), OnyxError>;
    fn codegen_method(&mut self, method: CheckedMethod, class_name: String) -> Result<(), OnyxError>;
    fn codegen_expression(&mut self, expression: CheckedExpression) -> Result<(), OnyxError>;
    fn codegen_type(&mut self, t: Type) -> Result<(), OnyxError>;
    fn codegen_type_to_string(&mut self, t: Type) -> Result<String, OnyxError>;
    fn codegen_block(&mut self, block: CheckedBlock) -> Result<(), OnyxError>;
    fn codegen_parameter(&mut self, parameter: CheckedParameter) -> Result<(), OnyxError>;
    fn codegen_parameter_to_string(&mut self, parameter: CheckedParameter) -> Result<String, OnyxError>;
    fn codegen_body(&mut self, block: CheckedBody) -> Result<(), OnyxError>;

    fn write(&mut self, s: &str) -> ();
    fn writeln(&mut self, s: &str) -> ();
    fn write_to_header(&mut self, s: &str) -> ();
    fn writeln_to_header(&mut self, s: &str) -> ();
}

#[derive(Debug, Clone)] pub struct CppCodegen {
    pub filename: String,
    pub output: String,
    pub header: String,
    pub indent: usize,
    includes: Vec<String>,

    functions: Vec<String>,
    classes: HashMap<String, CheckedClass>,
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
            classes: HashMap::new(),
            enums: Vec::new(),
        }
    }

    pub fn codegen(&mut self, ast: &CheckedAST) -> Result<String, OnyxError> {
        self.header.push_str("#include <stdint.h>\n");
        self.header.push_str("#include <variant>\n");
        self.output.push_str("#include <stdint.h>\n");
        let filename: String = self.filename.clone().split("/").last().unwrap().to_string();
        self.output.push_str(format!("#include \"{}.hpp\"\n", filename).as_str());
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
                                self.write_to_header("struct ");
                                self.write_to_header(&name);
                                self.writeln_to_header(" {};");
                            }
                            CheckedEnumVariant::Struct(name, _, fields) => {
                                for field in fields {
                                    if let Type::Unknown(name) = field.1.clone() {
                                        if enum_.generic_parameters.iter().any(|x| x.0 == name) {
                                            self.write_to_header("template<");
                                            let mut index: usize = 0;
                                            for (i, parameter) in enum_.generic_parameters.iter().enumerate() {
                                                if name != parameter.0 {
                                                    continue;
                                                }
                                                self.write_to_header("typename ");
                                                self.write_to_header(&parameter.0);
                                                index += 1;
                                                if index != enum_.generic_parameters.len() - 1 {
                                                    self.write_to_header(", ");
                                                }
                                            }
                                            self.write_to_header("> ");
                                        }
                                    }
                                }
                                self.write_to_header("struct ");
                                self.write_to_header(&name);
                                self.write_to_header(" {");
                                for (i, field) in fields.iter().enumerate() {
                                    let t: String = self.codegen_type_to_string(field.1.clone())?;
                                    self.write_to_header(t.as_str());
                                    self.write_to_header(" ");
                                    self.write_to_header(&field.0);
                                    self.write_to_header(";");
                                }
                                self.writeln_to_header("};");
                            }
                        }   
                    }

                    if enum_.generic_parameters.len() > 0 {
                        self.write_to_header("template<");
                        for (i, parameter) in enum_.generic_parameters.iter().enumerate() {
                            self.write_to_header("typename ");
                            self.write_to_header(&parameter.0);
                            if i != enum_.generic_parameters.len() - 1 {
                                self.write_to_header(", ");
                            }
                        }
                        self.writeln_to_header(">");
                    }
                    self.write_to_header("using ");
                    self.write_to_header(&enum_.name);
                    self.write_to_header(" = std::variant<");
                    for (i, variant) in enum_.variants.iter().enumerate() {
                        match variant {
                            CheckedEnumVariant::Tuple(name, _, types) => {
                                self.write_to_header("std::tuple<");
                                for (i, t) in types.iter().enumerate() {
                                    let t: String = self.codegen_type_to_string(t.0.clone())?;
                                    self.write_to_header(&t);
                                    if i != types.len() - 1 {
                                        self.write_to_header(", ");
                                    }
                                }
                                self.write_to_header(">");
                            }
                            CheckedEnumVariant::Unit(name, _) => {
                                self.write_to_header(&name);
                            }
                            CheckedEnumVariant::Struct(name, _, fields) => {
                                self.write_to_header(&name);
                                self.write_to_header("<");
                                for (i, field) in fields.iter().enumerate() {
                                    if let Type::Unknown(name) = field.1.clone() {
                                        if enum_.generic_parameters.iter().any(|x| x.0 == name) {
                                            self.write_to_header(&name);
                                        } else {
                                            let t: String = self.codegen_type_to_string(field.1.clone())?;
                                            self.write_to_header(&t);
                                        }
                                    } else {
                                        let t: String = self.codegen_type_to_string(field.1.clone())?;
                                        self.write_to_header(&t);
                                    }
                                    if i != fields.len() - 1 {
                                        self.write_to_header(", ");
                                    }
                                }
                                self.write_to_header(">");
                            }
                        }
                        if i != enum_.variants.len() - 1 {
                            self.write_to_header(", ");
                        }
                    }
                    self.writeln_to_header(">;");
                    self.enums.push(enum_.name.clone());
                }
            }
            CheckedStatement::Class(class) => {
                self.classes.insert(class.name.clone(), class.clone());
                if class.generic_parameters.len() > 0 {
                    self.write_to_header("template<");
                    for (i, parameter) in class.generic_parameters.iter().enumerate() {
                        self.write_to_header("typename ");
                        self.write_to_header(&self.clone().codegen_type_to_string(parameter.0.clone())?);
                        if i != class.generic_parameters.len() - 1 {
                            self.write_to_header(", ");
                        }
                    }
                    self.writeln_to_header(">");
                }
                self.write_to_header("class ");
                self.write_to_header(&class.name);
                self.write_to_header(" {\n");
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
                    self.writeln_to_header("public:");
                    self.indent += 1;
                    self.write_to_header("~");
                    self.write_to_header(&class.name);
                    if class.generic_parameters.len() > 0 {
                        self.write_to_header("<");
                        for (i, parameter) in class.generic_parameters.iter().enumerate() {
                            self.write_to_header(&self.clone().codegen_type_to_string(parameter.0.clone())?);
                            if i != class.generic_parameters.len() - 1 {
                                self.write_to_header(", ");
                            }
                        }
                        self.write_to_header(">");
                    }
                    self.writeln_to_header("() {}");
                    for field in public_fields {
                        self.codegen_field(field)?;
                    }
                    for method in public_methods {
                        self.codegen_method(method, class.name.clone())?;
                    }
                    self.indent -= 1;
                }
                if private_fields.len() > 0 || private_methods.len() > 0 {
                    self.writeln_to_header("private:");
                    self.indent += 1;
                    // constructor
                    self.write_to_header(&class.name);
                    if class.generic_parameters.len() > 0 {
                        self.write_to_header("<");
                        for (i, parameter) in class.generic_parameters.iter().enumerate() {
                            self.write_to_header(&self.clone().codegen_type_to_string(parameter.0.clone())?);
                            if i != class.generic_parameters.len() - 1 {
                                self.write_to_header(", ");
                            }
                        }
                        self.write_to_header(">");
                    }
                    self.write_to_header("(");
                    for (i, field) in class.fields.iter().enumerate() {
                        self.write_to_header(&self.clone().codegen_type_to_string(field.field_type.clone())?);
                        self.write_to_header(" ");
                        self.write_to_header(&field.name);
                        if i != class.fields.len() - 1 {
                            self.write_to_header(", ");
                        }
                    }
                    self.write_to_header(") : ");
                    for (i, field) in class.fields.iter().enumerate() {
                        self.write_to_header(&field.name);
                        self.write_to_header("(");
                        self.write_to_header(&field.name);
                        self.write_to_header(")");
                        if i != class.fields.len() - 1 {
                            self.write_to_header(", ");
                        }
                    }
                    self.writeln_to_header(" {}");
                    for field in private_fields {
                        self.codegen_field(field)?;
                    }
                    for method in private_methods {
                        self.codegen_method(method, class.name.clone())?;
                    }
                    self.indent -= 1;
                }
                if protected_fields.len() > 0 || protected_methods.len() > 0 {
                    self.writeln_to_header("protected:");
                    self.indent += 1;
                    for field in protected_fields {
                        self.codegen_field(field)?;
                    }
                    for method in protected_methods {
                        self.codegen_method(method, class.name.clone())?;
                    }
                    self.indent -= 1;
                }
                self.writeln_to_header("};");
            }
            CheckedStatement::Expression(expression) => {
                self.codegen_expression(expression)?;
                self.writeln(";");
            }
            CheckedStatement::Return(expression, _) => {
                self.write("return ");
                self.codegen_expression(expression)?;
                self.writeln(";");
            }
            _ => {}
        }
        Ok(())
    }
    fn codegen_field(&mut self, field: CheckedField) -> Result<(), OnyxError> {
        self.write_to_header(&self.clone().codegen_type_to_string(field.field_type.clone())?);
        self.write_to_header(" ");
        self.write_to_header(&field.name);
        self.writeln_to_header(";");
        Ok(())
    }
    fn codegen_method(&mut self, method: CheckedMethod, class_name: String) -> Result<(), OnyxError> {
        let class: CheckedClass = self.clone().classes.get(&class_name).unwrap().clone();
        if method.is_static() {
            self.write_to_header("static ");
        }
        self.write_to_header(&self.clone().codegen_type_to_string(method.return_type.clone())?);
        self.write_to_header(" ");
        self.write_to_header(&method.name);
        self.write_to_header("(");
        for (i, parameter) in method.parameters.iter().enumerate() {
            if parameter.name == "this" {
                continue;
            }
            self.write_to_header(&self.clone().codegen_parameter_to_string(parameter.clone())?);
            if i != method.parameters.len() - 1 {
                self.write_to_header(", ");
            }
        }
        self.writeln_to_header(");");
        if class.generic_parameters.len() > 0 {
            self.write("template<");
            for (i, parameter) in class.generic_parameters.iter().enumerate() {
                self.write("typename ");
                self.write(&self.clone().codegen_type_to_string(parameter.0.clone())?);
                if i != class.generic_parameters.len() - 1 {
                    self.write(", ");
                }
            }
            self.writeln(">");
        }
        self.codegen_type(method.return_type.clone())?;
        self.write(" ");
        self.write(&class_name);
        if class.generic_parameters.len() > 0 {
            self.write("<");
            for (i, parameter) in class.generic_parameters.iter().enumerate() {
                self.write(&self.clone().codegen_type_to_string(parameter.0.clone())?);
                if i != class.generic_parameters.len() - 1 {
                    self.write(", ");
                }
            }
            self.write(">");
        }
        self.write("::");
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
            CheckedExpression::Call(name, parameters, generic_parameters, _) => {
                self.codegen_expression(*name)?;
                if generic_parameters.len() > 0 {
                    self.write("<");
                    for (i, parameter) in generic_parameters.iter().enumerate() {
                        self.codegen_type(parameter.0.clone())?;
                        if i != generic_parameters.len() - 1 {
                            self.write(", ");
                        }
                    }
                    self.write(">");
                }
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
            CheckedExpression::StaticMemberAccess(expression, member, _) => {
                match *expression.clone() {
                    CheckedExpression::Identifier(name, _) => {
                        if self.enums.contains(&name) {
                            match *member.clone() {
                                CheckedExpression::Call(name, parameters, generic_parameters, _) => {
                                    self.codegen_expression(*name)?;
                                    if generic_parameters.len() > 0 {
                                        self.write("<");
                                        for (i, parameter) in generic_parameters.iter().enumerate() {
                                            self.codegen_type(parameter.0.clone())?;
                                            if i != generic_parameters.len() - 1 {
                                                self.write(", ");
                                            }
                                        }
                                        self.write(">");
                                    }
                                    self.write("{");
                                    for (i, parameter) in parameters.iter().enumerate() {
                                        self.codegen_expression(parameter.clone())?;
                                        if i != parameters.len() - 1 {
                                            self.write(", ");
                                        }
                                    }
                                    self.write("}");
                                }
                                _ => {
                                    self.write(&name);
                                    self.write("::");
                                    self.codegen_expression(*member)?;
                                }
                            }
                        } else {
                            self.write(&name);
                            self.write("::");
                            self.codegen_expression(*member)?;
                        }
                    }
                    _ => {
                        self.codegen_expression(*expression)?;
                        self.write("::");
                        self.codegen_expression(*member)?;
                    }
                }
            }
            CheckedExpression::PointerAccess(expression, member, _) => {
                self.codegen_expression(*expression)?;
                self.write("->");
                self.codegen_expression(*member)?;
            }
            CheckedExpression::Integer8(value, _) => {
                self.write(&value.to_string());
            }
            CheckedExpression::Sizeof(t, _) => {
                self.write("sizeof(");
                self.codegen_type(t.clone())?;
                self.write(")");
            }
            CheckedExpression::New(expression, _) => {
                self.write("new ");
                self.codegen_expression(*expression)?;
            }
            _ => {}
        }
        Ok(())
    }
    fn codegen_block(&mut self, block: CheckedBlock) -> Result<(), OnyxError> {
        Ok(())
    }
    fn codegen_parameter(&mut self, parameter: CheckedParameter) -> Result<(), OnyxError> {
        if parameter.name.clone() == "this" {
            return Ok(());
        }
        self.codegen_type(parameter.parameter_type.clone())?;
        self.write(" ");
        self.write(&parameter.name);
        Ok(())
    }
    fn codegen_parameter_to_string(&mut self, parameter: CheckedParameter) -> Result<String, OnyxError> {
        let mut result = String::new();
        result.push_str(&self.clone().codegen_type_to_string(parameter.parameter_type.clone())?);
        result.push_str(" ");
        result.push_str(&parameter.name);
        Ok(result)
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
            Type::Generic(t, types) => {
                self.codegen_type(*t)?;
                if types.len() == 0 {
                    return Ok(());
                }
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
    fn codegen_type_to_string(&mut self, t: Type) -> Result<String, OnyxError> {
        match t.clone() {
            Type::Char => Ok("char".to_string()),
            Type::Bool => Ok("bool".to_string()),
            Type::I8 => Ok("int8_t".to_string()),
            Type::I16 => Ok("int16_t".to_string()),
            Type::I32 => Ok("int32_t".to_string()),
            Type::I64 => Ok("int64_t".to_string()),
            Type::U8 => Ok("uint8_t".to_string()),
            Type::U16 => Ok("uint16_t".to_string()),
            Type::U32 => Ok("uint32_t".to_string()),
            Type::U64 => Ok("uint64_t".to_string()),
            Type::F32 => Ok("float".to_string()),
            Type::F64 => Ok("double".to_string()),
            Type::Usize => Ok("size_t".to_string()),
            Type::Void => Ok("void".to_string()),
            Type::Array(t) => Ok(format!("{}*", self.codegen_type_to_string(*t)?)),
            Type::SizedArray(t, size) => {
                Ok(format!("{}[{}]", self.codegen_type_to_string(*t)?, size))
            }
            Type::Optional(t) => Ok(format!(
                "std::optional<{}>",
                self.codegen_type_to_string(*t)?
            )),
            Type::RawPtr(t) => Ok(format!("{}*", self.codegen_type_to_string(*t)?)),
            Type::WeakPtr(t) => Ok(format!(
                "std::weak_ptr<{}>",
                self.codegen_type_to_string(*t)?
            )),
            Type::Class(name) | Type::Enum(name) => Ok(name),
            Type::Generic(t, types) => {
                let mut s: String = self.codegen_type_to_string(*t)?;
                s.push('<');
                for (i, t) in types.iter().enumerate() {
                    s.push_str(&self.codegen_type_to_string(t.clone())?);
                    if i != types.len() - 1 {
                        s.push_str(", ");
                    }
                }
                s.push('>');
                Ok(s)
            }
            Type::Unknown(name) => Ok(name),
        }
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
    fn write_to_header(&mut self, s: &str) -> () {
        self.header.push_str(s);
    }
    fn writeln_to_header(&mut self, s: &str) -> () {
        self.header.push_str(s);
        self.header.push_str("\n");
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
