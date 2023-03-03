use onyx::{
    error::OnyxError,
    span::Span
};

fn main() {
    let my_error: OnyxError = OnyxError::SyntaxError(
        format!("unrecognized character '{}'", 'a'),
        Span::new("examples/main.onyx".to_string(), 7, 8)
    );
    println!("{}", my_error.to_string());
}
