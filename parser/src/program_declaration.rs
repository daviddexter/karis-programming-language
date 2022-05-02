use crate::declarations::{Declaration, DeclarationType};
use crate::objects::Objects;

// ProgramDeclaration is the root declaration
// this will be at the top of the AST
#[derive(Debug, Clone)]
pub struct ProgramDeclaration {
    pub body: Vec<Objects>,
}

impl Declaration for ProgramDeclaration {
    fn which(&self) -> DeclarationType {
        DeclarationType::Program
    }
}

impl ProgramDeclaration {
    pub fn new() -> ProgramDeclaration {
        Self { body : Vec::new() }
    }

    pub fn add_object(&mut self, object: Objects){
        self.body.push(object)
    }

    pub fn count(&self) -> usize {
        self.body.len()
    }
}
