use std::cell::RefCell;
use std::rc::Rc;

use either::Either;
use enum_as_inner::EnumAsInner;

use serde::ser::SerializeStruct;
use serde::Serialize;
use serde::Serializer;

use pyo3::prelude::*;

use errors::errors;
use lexer::tokens::IdentifierKind;
use lexer::tokens::Token;

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

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
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

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
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

#[derive(Debug, EnumAsInner, Clone, Serialize)]
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

    pub fn write_as_json_to_file(&self, json_name: Option<&str>) -> std::io::Result<()> {
        use std::io::Write;

        let json = serde_json::to_string_pretty(self).unwrap();
        let file_name = json_name.unwrap_or("object.json");
        let mut curent_directory = std::env::current_dir()?;

        let exists = std::fs::metadata(curent_directory.join("dumps"));
        if exists.is_err() {
            std::fs::create_dir(curent_directory.join("dumps"))?;
        }

        curent_directory.push("dumps/");
        let temp_file = curent_directory.join(file_name);
        let mut file = std::fs::File::create(temp_file)?;
        file.write_all(json.as_bytes())?;

        Ok(())
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
#[derive(Debug, Default, Clone, Serialize)]
pub struct Program {
    pub body: Vec<Objects>,
    pub non_root: bool,
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

    pub fn toggle_root(&mut self) {
        self.non_root = true;
    }
}

pub trait Value {
    // returns the type of the current declaration object
    fn kind(&self) -> TypingKind;

    fn inspect(&self) -> Vec<(String, String)>;
}

/// Represents literal values definitions
#[derive(Debug, EnumAsInner, PartialEq, Eq, Clone, Serialize)]
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
#[derive(Debug, PartialEq, Eq, Default, Clone, Serialize)]
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
#[derive(Debug, PartialEq, Eq, Default, Clone, Serialize)]
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
#[derive(Debug, PartialEq, Eq, Default, Clone, Serialize)]
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

    pub array_type: Option<TypingKind>,

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

    /// this will be populated for the `fn` block, `if` block, `else` block, `main` block and
    /// items af an array e.g [ 1 , 2 , 3 ]
    pub block_children: Option<Vec<Objects>>,
}

impl Declaration for Node {
    fn which(&self) -> DeclarationType {
        DeclarationType::Node
    }
}

impl Node {
    pub fn hash(&self) -> String {
        let encoded: Vec<u8> = bincode::serialize(self).unwrap();
        let mut hasher = blake3::Hasher::new();
        hasher.update(&encoded);
        hasher.finalize().to_string()
    }

    pub(crate) fn inspect(&self) -> NodeEdge {
        if let Some(kind) = self.identifier_kind {
            match kind {
                IdentifierKind::LET | IdentifierKind::VARIABLE => let_and_variables(self, kind),
                IdentifierKind::ASSIGN => assign(self),

                IdentifierKind::PLUS
                | IdentifierKind::MINUS
                | IdentifierKind::SLASH
                | IdentifierKind::ASTERISK
                | IdentifierKind::MODULUS
                | IdentifierKind::GT
                | IdentifierKind::LT
                | IdentifierKind::GTOREQ
                | IdentifierKind::LTOREQ
                | IdentifierKind::EQ
                | IdentifierKind::OR
                | IdentifierKind::AND
                | IdentifierKind::LOR
                | IdentifierKind::LAND => infix_operators(self, kind),

                IdentifierKind::RETURN => returner(self),

                IdentifierKind::EOS => default_node_edges(),

                IdentifierKind::INTLITERAL
                | IdentifierKind::BOOLEANLITERAL
                | IdentifierKind::STRINGLITERAL => literals(self),

                IdentifierKind::FUNCTION => function(self),

                IdentifierKind::ARRAY => todo!("implement for array"),

                _ => todo!("not implemented for {:?}", kind),
            }
        } else {
            default_node_edges()
        }
    }
}

impl Serialize for Node {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut s = serializer.serialize_struct("Node", 9)?;
        s.serialize_field("variable_name", &self.variable_name)?;
        s.serialize_field("return_type", &self.return_type)?;
        s.serialize_field("identifier_kind", &self.identifier_kind)?;
        s.serialize_field("alternate", &self.alternate)?;
        s.serialize_field("block_children", &self.block_children)?;

        if let Some(op) = &self.left_child {
            if op.is_left() {
                let left = op.as_ref().left().unwrap();
                s.serialize_field("left_child", &left)?;
            } else {
                let right = op.as_ref().right().unwrap();
                s.serialize_field("left_child", &right)?;
            }
        }

        if let Some(op) = &self.right_child {
            if op.is_left() {
                let left = op.as_ref().left().unwrap();
                s.serialize_field("right_child", &left)?;
            } else {
                let right = op.as_ref().right().unwrap();
                s.serialize_field("right_child", &right)?;
            }
        }

        if let Some(op) = &self.func_params {
            let mut params_lit = Vec::new();
            let mut params_obj = Vec::new();

            op.iter().for_each(|item| {
                if item.is_left() {
                    let left = item.as_ref().left().unwrap();
                    params_lit.push(left.clone());
                } else {
                    let right = item.as_ref().right().unwrap();
                    params_obj.push(right.clone());
                }
            });

            #[derive(Serialize)]
            struct Params {
                lit: Vec<LiteralObjects>,
                obj: Vec<Objects>,
            }

            let params = Params {
                lit: params_lit,
                obj: params_obj,
            };

            s.serialize_field("func_params", &params)?;
        }

        if let Some(op) = &self.call_params {
            let mut params_lit = Vec::new();
            let mut params_obj = Vec::new();

            op.iter().for_each(|item| {
                if item.is_left() {
                    let left = item.as_ref().left().unwrap();
                    params_lit.push(left.clone());
                } else {
                    let right = item.as_ref().right().unwrap();
                    params_obj.push(right.clone());
                }
            });

            #[derive(Serialize)]
            struct Params {
                lit: Vec<LiteralObjects>,
                obj: Vec<Objects>,
            }

            let params = Params {
                lit: params_lit,
                obj: params_obj,
            };

            s.serialize_field("call_params", &params)?;
        }

        s.end()
    }
}
