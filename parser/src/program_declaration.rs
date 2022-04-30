use crate::declarations::{Declaration, DeclarationType};
use crate::objects::Objects;

// ProgramDeclaration is the root declaration
// this will be at the top of the AST
#[derive(Debug, Clone)]
pub struct ProgramDeclaration {
    pub body: Option<Vec<Objects>>,
}

impl Declaration for ProgramDeclaration {
    fn which(&self) -> DeclarationType {
        DeclarationType::Program
    }
}

impl ProgramDeclaration {
    pub fn new(body: Option<Vec<Objects>>) -> ProgramDeclaration {
        Self { body }
    }

    pub fn count(&self) -> usize {
        match &self.body {
            Some(i) => i.len(),
            None => 0,
        }
    }
}
