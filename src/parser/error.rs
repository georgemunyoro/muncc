
pub struct MCCParserError {
    message: String,
}

impl MCCParserError {
    pub fn new(message: &str) -> Self {
        Self {
            message: String::from(message),
        }
    }
}
