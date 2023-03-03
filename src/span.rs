#[derive(Clone, Debug, PartialEq)]
pub struct Span { file_name: String, start: usize, end: usize }
impl Span {
    pub fn new(file_name: String, start: usize, end: usize) -> Span {
        Span { file_name, start, end }
    }
}