use std::cell::RefCell;
use std::rc::Rc;

use either::Either;
use enum_as_inner::EnumAsInner;

use errors::errors;
use lexer::tokens::Token;

use lexer::tokens::IdentifierKind;

// function definition of a worker that does operations on the provided token
type NudParserOp =
    fn(Token, usize, Rc<RefCell<Vec<Token>>>) -> Result<(Objects, usize), errors::KarisError>;

// operation that returns the right-hand side of an expression
// It takes left Object `Objects`, the index of the token `usize` pointing to `=` and the bucket where the tokens are present
type LedParserOp =
    fn(Objects, usize, Rc<RefCell<Vec<Token>>>) -> Result<(Objects, usize), errors::KarisError>;

// ParterType defines an type passed into the parser. This type defines how it should be parsed by
// specifying it's own unique `NudParserOp` and/or `LedParserOp` and a `binding_power`
#[derive(Debug, Default, Clone)]
pub struct ParserType {
    pub nud_fn: Option<NudParserOp>,

    pub led_fn: Option<LedParserOp>,

    pub binding_power: Option<usize>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TypingKind {
    Unknown,
    Int,
    String,
    Boolean,

    // TODO: add this in the lexer first
    Array,
}

// Declaration : an object must be able to tell what is it
// `which` returns what the object knows about itself
pub trait Declaration {
    // returns the type of the current declaration object
    fn which(&self) -> DeclarationType;
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum DeclarationType {
    Unknown,
    Program,
    Node,
}

impl Default for DeclarationType {
    fn default() -> Self {
        DeclarationType::Unknown
    }
}

#[derive(Debug, EnumAsInner, Clone)]
pub enum Objects {
    TyProgram(Program),
    TyNode(Node),
    TyUnknown,
}

impl Declaration for Objects {
    fn which(&self) -> DeclarationType {
        match &self {
            Objects::TyProgram(i) => i.which(),
            Objects::TyUnknown => DeclarationType::Unknown,
            _ => panic!(""),
        }
    }
}

impl Default for Objects {
    fn default() -> Self {
        Objects::TyUnknown
    }
}

// Program is the root declaration. It will be at the top of the AST
#[derive(Debug, Default, Clone)]
pub struct Program {
    body: Vec<Objects>,
}

impl Declaration for Program {
    fn which(&self) -> DeclarationType {
        DeclarationType::Program
    }
}

impl Program {
    pub fn add_object(&mut self, object: Objects) {
        self.body.push(object)
    }
    pub fn count(&self) -> usize {
        self.body.len()
    }
}

pub trait Value {
    // returns the type of the current declaration object
    fn kind(&self) -> TypingKind;
}

// Represents literal values definitions
#[derive(Debug, EnumAsInner, PartialEq, Eq, Clone)]
pub enum LiteralObjects {
    ObjIntergerValue(IntergerValue),
    ObjBooleanValue(BooleanValue),
    ObjStringValue(StringValue),
}

impl Value for LiteralObjects {
    fn kind(&self) -> TypingKind {
        match &self {
            LiteralObjects::ObjIntergerValue(i) => i.kind(),
            LiteralObjects::ObjBooleanValue(i) => i.kind(),
            LiteralObjects::ObjStringValue(i) => i.kind(),
        }
    }
}

// Interger values representation
#[derive(Debug, PartialEq, Eq, Default, Clone)]
pub struct IntergerValue {
    pub value: Option<isize>,
}

impl Value for IntergerValue {
    fn kind(&self) -> TypingKind {
        TypingKind::Int
    }
}

// Boolean values representation
#[derive(Debug, PartialEq, Eq, Default, Clone)]
pub struct BooleanValue {
    pub value: Option<bool>,
}

impl Value for BooleanValue {
    fn kind(&self) -> TypingKind {
        TypingKind::Boolean
    }
}

// String values representation
#[derive(Debug, PartialEq, Eq, Default, Clone)]
pub struct StringValue {
    pub value: Option<String>,
}

impl Value for StringValue {
    fn kind(&self) -> TypingKind {
        TypingKind::String
    }
}

// Node is the smallest unit of a program. Depending with what the parser outputs, a node can take
// varied structure forms.
// Example:
// let num @int = 10;
//
// In the above `let num @int` will be treated as it's own unique node. This node will have a `identifier kind ` of type `LET`,
// a `variable_name` of value `num` and a `return_type` of `INTTYPE`
//
// The `=` assign will be a it's own unique node. This node will have a `identifier kind  kind` of type `ASSIGN`, a `left_child` whose value will
// be the node in the left hand side, and a `right_child` whose value will be the another node representing the literal `10`
//
// The `10` literal will be a it's own unique node. This node will have a `kind` of type `INTLITERAL` with a `left_child` of
// type `LiteralObjects` and an `identifier kind` of type `INTLITERAL`
//
//
// The tree of the abov expression will be of the form
//
//                 Node(=)
//                /       \
//           Node(LET)     Node(10)
#[derive(Debug, Default, Clone)]
pub struct Node {
    pub variable_name: Option<String>,

    pub return_type: Option<TypingKind>,

    pub identifier_kind: Option<IdentifierKind>,

    // type of `Node`
    pub left_child: Option<Either<LiteralObjects, Box<Objects>>>,

    // the RHS can either be a literal or a node
    pub right_child: Option<Either<LiteralObjects, Box<Objects>>>,

    // this is used to populates params of a function definition.
    // This params can be of different types
    // If the function has any other return type another than `@unit`, the return type will be evaluated
    // to match that of the definition
    pub func_params: Option<Vec<LiteralObjects>>,

    // this is used for call expressions which can take the form of
    //       func(1, 2, 3)
    //       func(1,a,b) where a and b are variables of the same type
    pub call_params: Option<Vec<Either<LiteralObjects, Objects>>>,

    // this will be populated for the `main` block
    pub children: Option<Vec<Node>>,
}

impl Declaration for Node {
    fn which(&self) -> DeclarationType {
        DeclarationType::Node
    }
}
