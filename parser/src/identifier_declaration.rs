use crate::declarations::{Declaration, DeclarationType, TypingKind};

#[derive(Debug, Clone)]
pub struct IdentifierDeclaration {
    pub name: String,
    pub typing: TypingKind,
}

impl Declaration for IdentifierDeclaration {
    fn which(&self) -> DeclarationType {
        DeclarationType::Identifier
    }
}

impl IdentifierDeclaration {
    pub fn new(name: String, typing: TypingKind) -> Self {
        Self { name, typing }
    }
}
