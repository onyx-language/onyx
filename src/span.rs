#[derive(Clone, Debug)]
pub struct Span { file_name: String, start: usize, end: usize }
impl Span {
    pub fn new(file_name: String, start: usize, end: usize) -> Span {
        Span { file_name, start, end }
    }
    pub fn get_file_name(&self) -> String { self.file_name.clone() }
    pub fn get_start(&self) -> usize { self.start }
    pub fn get_end(&self) -> usize { self.end }
}