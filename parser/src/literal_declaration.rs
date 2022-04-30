use crate::declarations::{Declaration, DeclarationType};

#[derive(Debug, Clone)]
pub struct LiteralDeclaration {
    pub value: String,
}

impl Declaration for LiteralDeclaration {
    fn which(&self) -> DeclarationType {
        DeclarationType::Literal
    }
}

impl LiteralDeclaration {
    pub fn new(value: String) -> Self {
        Self { value }
    }
}
