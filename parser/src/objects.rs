use std::cell::RefCell;
use std::rc::Rc;

use either::Either;
use enum_as_inner::EnumAsInner;

use pyo3::prelude::*;

use errors::errors;
use lexer::tokens::Token;

use lexer::tokens::IdentifierKind;

use crate::inspector::assign;
use crate::inspector::default_node_edges;
use crate::inspector::function;
use crate::inspector::infix_operators;
use crate::inspector::let_and_variables;
use crate::inspector::literals;
use crate::inspector::random_name_gen;
use crate::inspector::returner;
use crate::inspector::NodeEdge;

/// function definition of a worker that does operations on the provided token
type NudParserOp =
    fn(Token, usize, Rc<RefCell<Vec<Token>>>) -> Result<(Objects, usize), errors::KarisError>;

/// operation that returns the right-hand side of an expression
/// It takes left Object `Objects`, the index of the token `usize` pointing to `=` and the bucket where the tokens are present
type LedParserOp =
    fn(Objects, usize, Rc<RefCell<Vec<Token>>>) -> Result<(Objects, usize), errors::KarisError>;

/// ParterType defines an type passed into the parser. This type defines how it should be parsed by
/// specifying it's own unique `NudParserOp` and/or `LedParserOp` and a `binding_power`
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

/// Declaration : an object must be able to tell what is it
/// `which` returns what the object knows about itself
pub trait Declaration {
    // returns the type of the current declaration object
    fn which(&self) -> DeclarationType;
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum DeclarationType {
    Unknown,

    Program,
    Node,
    Consumable,
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
    TyConsumable,
    TyUnknown,
}

impl Objects {
    pub fn inspect_and_print(&self) -> PyResult<()> {
        let nodes = self.inspect();
        Python::with_gil(|_py| {
            let py_app = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/_x/_nodes_edges.py"));

            Python::with_gil(|py| -> PyResult<Py<PyAny>> {
                let app: Py<PyAny> = PyModule::from_code(py, py_app, "", "")?
                    .getattr("draw_node_and_edges")?
                    .into();
                app.call1(py, nodes)
            })?;

            Ok(())
        })
    }

    pub(crate) fn inspect(&self) -> NodeEdge {
        match &self {
            Objects::TyProgram(program) => {
                let body = &program.body;

                let mut result_nodes: Vec<(String, String)> = Vec::new();
                let mut result_edges: Vec<(String, String)> = Vec::new();

                let root = random_name_gen();

                result_nodes.push((root.clone(), "NODE(PROGRAM)".to_string()));

                for object in body.iter() {
                    let (nodes, edges) = object.inspect();

                    println!("{:?}\n", nodes);

                    println!("{:?}\n", edges);

                    todo!("bug here: top-level LET vs function-level LET");

                    for node in nodes.iter() {
                        let kind = IdentifierKind::ASSIGN;
                        let assign_node_name = format!("NODE({kind:#?})");
                        if node.1.clone() == assign_node_name {
                            result_edges.push((root.clone(), node.0.clone()));
                        };

                        result_nodes.push(node.clone());
                    }

                    for edge in edges.iter() {
                        result_edges.push(edge.clone());
                    }
                }

                (result_nodes, result_edges)
            }

            Objects::TyNode(node) => node.inspect(),

            _ => default_node_edges(),
        }
    }
}

impl Declaration for Objects {
    fn which(&self) -> DeclarationType {
        match &self {
            Objects::TyProgram(i) => i.which(),
            Objects::TyNode(i) => i.which(),
            Objects::TyConsumable => DeclarationType::Consumable,
            Objects::TyUnknown => DeclarationType::Unknown,
        }
    }
}

impl Default for Objects {
    fn default() -> Self {
        Objects::TyUnknown
    }
}

/// Program is the root declaration. It will be at the top of the AST
#[derive(Debug, Default, Clone)]
pub struct Program {
    pub body: Vec<Objects>,
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

    fn inspect(&self) -> Vec<(String, String)>;
}

/// Represents literal values definitions
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

    fn inspect(&self) -> Vec<(String, String)> {
        match &self {
            LiteralObjects::ObjIntergerValue(i) => i.inspect(),
            LiteralObjects::ObjBooleanValue(i) => i.inspect(),
            LiteralObjects::ObjStringValue(i) => i.inspect(),
        }
    }
}

/// Interger values representation
#[derive(Debug, PartialEq, Eq, Default, Clone)]
pub struct IntergerValue {
    pub value: Option<isize>,
}

impl Value for IntergerValue {
    fn kind(&self) -> TypingKind {
        TypingKind::Int
    }

    fn inspect(&self) -> Vec<(String, String)> {
        let kind = self.kind();
        let root = random_name_gen();
        let mut nodes = Vec::new();
        nodes.push((
            root,
            format!("NODE({kind:#?} Value : {})", self.value.unwrap_or_default()),
        ));
        nodes
    }
}

/// Boolean values representation
#[derive(Debug, PartialEq, Eq, Default, Clone)]
pub struct BooleanValue {
    pub value: Option<bool>,
}

impl Value for BooleanValue {
    fn kind(&self) -> TypingKind {
        TypingKind::Boolean
    }

    fn inspect(&self) -> Vec<(String, String)> {
        let kind = self.kind();
        let root = random_name_gen();
        let mut nodes = Vec::new();
        nodes.push((
            root,
            format!("NODE({kind:#?} Value : {})", self.value.unwrap_or_default()),
        ));
        nodes
    }
}

/// String values representation
#[derive(Debug, PartialEq, Eq, Default, Clone)]
pub struct StringValue {
    pub value: Option<String>,
}

impl Value for StringValue {
    fn kind(&self) -> TypingKind {
        TypingKind::String
    }

    fn inspect(&self) -> Vec<(String, String)> {
        let kind = self.kind();
        let root = random_name_gen();
        let mut nodes = Vec::new();
        nodes.push((
            root,
            format!("NODE({kind:#?} Value : {})", self.value.as_ref().unwrap()),
        ));
        nodes
    }
}

/// Node is the smallest unit of a program. Depending with what the parser outputs, a node can take
/// varied structure forms.
/// Example:
/// let num @int = 10;
///
/// In the above `let num @int` will be treated as it's own unique node. This node will have a `identifier kind ` of type `LET`,
/// a `variable_name` of value `num` and a `return_type` of `INTTYPE`
///
/// The `=` assign will be a it's own unique node. This node will have a `identifier kind  kind` of type `ASSIGN`, a `left_child` whose value will
/// be the node in the left hand side, and a `right_child` whose value will be the another node representing the literal `10`
///
/// The `10` literal will be a it's own unique node. This node will have a `kind` of type `INTLITERAL` with a `left_child` of
/// type `LiteralObjects` and an `identifier kind` of type `INTLITERAL`
///
///
/// The tree of the abov expression will be of the form
///
///                 Node(=)
///                /       \
///           Node(LET)     Node(10)
#[derive(Debug, Default, Clone)]
pub struct Node {
    pub variable_name: Option<String>,

    pub return_type: Option<TypingKind>,

    pub identifier_kind: Option<IdentifierKind>,

    /// type of `Node`
    pub left_child: Option<Either<LiteralObjects, Box<Objects>>>,

    /// the RHS can either be a literal or a node
    pub right_child: Option<Either<LiteralObjects, Box<Objects>>>,

    /// this is used to populates params of a function definition.
    /// This params can be of different types
    /// If the function has any other return type another than `@unit`, the return type will be evaluated
    /// to match that of the definition
    pub func_params: Option<Vec<Either<LiteralObjects, Objects>>>,

    /// this is used for call expressions which can take the form of
    ///       func(1, 2, 3)
    ///       func(1,a,b) where a and b are variables of the same type
    pub call_params: Option<Vec<Either<LiteralObjects, Objects>>>,

    /// alternate will be populated with an `else` part of a conditional expression
    pub alternate: Option<Box<Objects>>,

    /// this will be populated for the `fn` block, `if` block, `else` block and `main` block
    pub block_children: Option<Vec<Objects>>,
}

impl Declaration for Node {
    fn which(&self) -> DeclarationType {
        DeclarationType::Node
    }
}

impl Node {
    pub(crate) fn inspect(&self) -> NodeEdge {
        if let Some(kind) = self.identifier_kind {
            match kind {
                IdentifierKind::LET | IdentifierKind::VARIABLE => let_and_variables(self, kind),
                IdentifierKind::ASSIGN => assign(self),

                IdentifierKind::PLUS
                | IdentifierKind::MINUS
                | IdentifierKind::SLASH
                | IdentifierKind::ASTERISK => infix_operators(self, kind),

                IdentifierKind::RETURN => returner(self),

                IdentifierKind::EOS => default_node_edges(),

                IdentifierKind::INTLITERAL
                | IdentifierKind::BOOLEANLITERAL
                | IdentifierKind::STRINGLITERAL => literals(self),

                IdentifierKind::FUNCTION => function(self),

                _ => todo!("not implemented for {:?}", kind),
            }
        } else {
            default_node_edges()
        }
    }
}
