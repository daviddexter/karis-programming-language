use crate::declarations::{Declaration, DeclarationType, VariableKind};
use crate::objects::Objects;

// VariableDeclaration ...
#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub kind: VariableKind,
    pub declarations: Option<Vec<Objects>>,
}

impl Declaration for VariableDeclaration {
    fn which(&self) -> DeclarationType {
        DeclarationType::VariableDeclaration
    }
}

impl VariableDeclaration {
    pub fn new(kind: VariableKind, declarations: Option<Vec<Objects>>) -> Self {
        Self { kind, declarations }
    }
}
