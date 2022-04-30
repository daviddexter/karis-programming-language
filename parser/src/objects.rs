use crate::declarations::{Declaration, DeclarationType, ObjectId};
use crate::identifier_declaration::IdentifierDeclaration;
use crate::literal_declaration::LiteralDeclaration;
use crate::program_declaration::ProgramDeclaration;
use crate::variable_declaration::VariableDeclaration;
use crate::variable_declarator::VariableDeclarator;

#[derive(Debug, Clone)]
pub enum Objects {
    IdentifierDeclaration(IdentifierDeclaration),
    LiteralDeclaration(LiteralDeclaration),
    ProgramDeclaration(ProgramDeclaration),
    VariableDeclaration(VariableDeclaration),
    VariableDeclarator(VariableDeclarator),
}

impl Declaration for Objects {
    fn which(&self) -> DeclarationType {
        match &self {
            Objects::IdentifierDeclaration(i) => i.which(),
            Objects::LiteralDeclaration(i) => i.which(),
            Objects::ProgramDeclaration(i) => i.which(),
            Objects::VariableDeclaration(i) => i.which(),
            Objects::VariableDeclarator(i) => i.which(),
        }
    }
}

impl ObjectId for Objects {
    fn id(&self) -> Option<IdentifierDeclaration> {
        match &self {
            Objects::IdentifierDeclaration(_) => None,
            Objects::LiteralDeclaration(_) => None,
            Objects::ProgramDeclaration(_) => None,
            Objects::VariableDeclaration(_) => None,
            Objects::VariableDeclarator(i) => i.id(),
        }
    }
}
