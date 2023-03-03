use crate::span::Span;
use colored::*;
#[derive(Debug, Clone)]
pub enum OnyxError {
    IOError(std::rc::Rc<std::io::Error>),
    SyntaxError(String, Span),
    TypeError(String, Span),
    ValueError(String, Span),
    RuntimeError(String, Span),
}
impl OnyxError {
    pub fn span(&self) -> Span {
        match self {
            OnyxError::SyntaxError(_, s) => s.clone(),
            OnyxError::TypeError(_, s) => s.clone(),
            OnyxError::ValueError(_, s) => s.clone(),
            OnyxError::RuntimeError(_, s) => s.clone(),
            _ => unreachable!(),
        }
    }
    fn message(&self, show_type: bool) -> String {
        match self {
            OnyxError::SyntaxError(msg, _) => format!("{}{}", if show_type { "SyntaxError: " } else { "" }, msg),
            OnyxError::TypeError(msg, _) => format!("{}{}", if show_type { "TypeError: " } else { "" }, msg),
            OnyxError::ValueError(msg, _) => format!("{}{}", if show_type { "ValueError: " } else { "" }, msg),
            OnyxError::RuntimeError(msg, _) => format!("{}{}", if show_type { "RuntimeError: " } else { "" }, msg),
            _ => unreachable!(),
        }
    }
    pub fn to_string(&self) -> String {
        let mut out: String = String::new();
        out.push_str(format!("[{}:{}:{}] {}\n", 
            self.span().get_file_name(),
            self.get_line_number(),
            self.get_column(),
            self.message(true)).as_str());
        out.push_str(format!("{}{}\n", 
            format!("{:>5} | ", self.get_line_number() - 1).bright_blue(),
            self.get_contents_of_line(self.get_line_number()).unwrap()).as_str());
        out.push_str(format!("{}{}\n",
            format!("{:>5} | ", "").bright_blue(),
            format!("{}- {}",
                " ".repeat(self.get_column() - 1) + "^",
                self.message(false)).red()).as_str());

        out
    }
    fn get_contents(&self) -> Result<String, OnyxError> {
        match std::fs::read_to_string(self.span().get_file_name()) {
            Ok(contents) => Ok(contents),
            Err(_) => return Err(OnyxError::IOError(std::rc::Rc::new(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("file '{}' not found", self.span().get_file_name())
            )))),
        }
    }
    fn get_contents_of_line(&self, line: usize) -> Result<String, OnyxError> {
        let contents = self.get_contents().unwrap();
        let mut line_number: usize = 1;
        let mut start: usize = 0;
        let mut end: usize = 0;
        for (i, c) in contents.chars().enumerate() {
            if c == '\n' {
                line_number += 1;
                if line_number == line {
                    start = i + 1;
                }
                if line_number == line + 1 {
                    end = i;
                    break;
                }
            }
        }
        Ok(contents[start..end].to_string())
    }
    fn get_line_number(&self) -> usize {
        let contents = self.get_contents().unwrap();
        let mut line_number: usize = 1;
        for (i, c) in contents.chars().enumerate() {
            if i == self.span().get_start() {
                break;
            }
            if c == '\n' {
                line_number += 1;
            }
        }
        line_number
    }
    fn get_column(&self) -> usize {
        let contents = self.get_contents().unwrap();
        let mut column: usize = 1;
        for (i, c) in contents.chars().enumerate() {
            if i == self.span().get_start() {
                break;
            }
            if c == '\n' {
                column = 1;
            } else {
                column += 1;
            }
        }
        column
    }
}
impl From<std::io::Error> for OnyxError {
    fn from(error: std::io::Error) -> Self {
        OnyxError::IOError(std::rc::Rc::new(error))
    }
}