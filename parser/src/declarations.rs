use crate::identifier_declaration::IdentifierDeclaration;

#[derive(Debug, Clone)]
pub enum DeclarationType {
    Program,
    VariableDeclaration,
    VariableDeclarator,
    Identifier,
    Literal,
    BinaryExpression,
}

#[derive(Debug, Clone)]
pub enum VariableKind {
    Let,
}

#[derive(Debug, Clone)]
pub enum TypingKind {
    Int,
    String,
    Boolean,
}

// Declaration : an object must to be to tell what is it
// which returns what the object knows about itself
pub trait Declaration {
    // returns the type of the current declaration object
    fn which(&self) -> DeclarationType;
}

// ObjectId : returns the identifier of the object
pub trait ObjectId: Declaration {
    fn id(&self) -> Option<IdentifierDeclaration>;
}
