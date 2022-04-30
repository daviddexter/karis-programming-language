use crate::declarations::{Declaration, DeclarationType, ObjectId};
use crate::identifier_declaration::IdentifierDeclaration;
use crate::objects::Objects;

#[derive(Debug, Clone)]
pub struct VariableDeclarator {
    pub init: Box<Objects>,
    pub id: IdentifierDeclaration,
}

impl Declaration for VariableDeclarator {
    fn which(&self) -> DeclarationType {
        DeclarationType::VariableDeclarator
    }
}

impl ObjectId for VariableDeclarator {
    fn id(&self) -> Option<IdentifierDeclaration> {
        Some(self.id.clone())
    }
}

impl VariableDeclarator {
    pub fn new(init: Objects, id: IdentifierDeclaration) -> Self {
        Self {
            init: Box::new(init),
            id,
        }
    }
}
