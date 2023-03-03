use crate::span::Span;
#[derive(Debug)]
pub enum OnyxError {
    IOError(std::io::Error),
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
    fn message(&self) -> String {
        match self {
            OnyxError::SyntaxError(msg, _) => format!("SyntaxError: {}", msg),
            OnyxError::TypeError(msg, _) => format!("TypeError: {}", msg),
            OnyxError::ValueError(msg, _) => format!("ValueError: {}", msg),
            OnyxError::RuntimeError(msg, _) => format!("RuntimeError: {}", msg),
            _ => unreachable!(),
        }
    }
    pub fn to_string(&self) -> String {
        let mut out: String = String::new();
        out.push_str(format!("[{}:{}:{}] {}\n", 
            self.span().get_file_name(),
            self.get_line_number(),
            self.get_column(),
            self.message()).as_str());
        out
    }
    fn get_contents(&self) -> Result<String, OnyxError> {
        match std::fs::read_to_string(self.span().get_file_name()) {
            Ok(contents) => Ok(contents),
            Err(_) => return Err(OnyxError::IOError(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("file '{}' not found", self.span().get_file_name())
            ))),
        }
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
        OnyxError::IOError(error)
    }
}