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

    fn inspect(&self) -> String;
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
            Objects::TyNode(i) => i.which(),
            Objects::TyUnknown => DeclarationType::Unknown,
        }
    }

    fn inspect(&self) -> String {
        match &self {
            Objects::TyProgram(i) => i.inspect(),
            Objects::TyNode(i) => i.inspect(),
            _ => unreachable!("nothing to inspect"),
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

    fn inspect(&self) -> String {
        let mut parts = Vec::new();
        for obj in self.body.iter() {
            parts.push(obj.inspect());
        }
        parts.join("")
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

    fn inspect(&self) -> String;
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

    fn inspect(&self) -> String {
        match &self {
            LiteralObjects::ObjIntergerValue(i) => i.inspect(),
            LiteralObjects::ObjBooleanValue(i) => i.inspect(),
            LiteralObjects::ObjStringValue(i) => i.inspect(),
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

    fn inspect(&self) -> String {
        if let Some(value) = &self.value {
            format!("NODE(INT:{})", value)
        } else {
            "NODE(INT:UNDEFINED)".to_string()
        }
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

    fn inspect(&self) -> String {
        if let Some(value) = &self.value {
            format!("NODE(BOOL:{})", value)
        } else {
            "NODE(BOOL:UNDEFINED)".to_string()
        }
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

    fn inspect(&self) -> String {
        if let Some(value) = &self.value {
            format!("NODE(STRING:{})", value)
        } else {
            "NODE(STRING:UNDEFINED)".to_string()
        }
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
    pub func_params: Option<Vec<Either<LiteralObjects, Objects>>>,

    // this is used for call expressions which can take the form of
    //       func(1, 2, 3)
    //       func(1,a,b) where a and b are variables of the same type
    pub call_params: Option<Vec<Either<LiteralObjects, Objects>>>,

    // alternate will be populated with an `else` part of a conditional expression
    pub alternate: Option<Box<Objects>>,

    // this will be populated for the `fn` block, `if` block, `else` block and `main` block
    pub block_children: Option<Vec<Objects>>,
}

impl Declaration for Node {
    fn which(&self) -> DeclarationType {
        DeclarationType::Node
    }

    fn inspect(&self) -> String {
        if let Some(kind) = self.identifier_kind {
            match kind {
                IdentifierKind::LET | IdentifierKind::VARIABLE => {
                    let default_return_type = &TypingKind::Unknown;
                    let default_variable_name = &"".to_string();

                    let return_type = self.return_type.as_ref().unwrap_or(default_return_type);
                    let variable_name =
                        self.variable_name.as_ref().unwrap_or(default_variable_name);
                    let out = format!("NODE({kind:#?} : RETURN_TYPE={return_type:#?} VARIABLE_NAME={variable_name})");
                    out
                }

                IdentifierKind::ASSIGN => {
                    let lhs = self.left_child.as_ref().unwrap().as_ref().right().unwrap();
                    let lhs_str = lhs.inspect();

                    let rhs_str = if self.right_child.as_ref().unwrap().as_ref().is_left() {
                        let rhs = self.right_child.as_ref().unwrap().as_ref().left().unwrap();
                        rhs.inspect()
                    } else {
                        let rhs = self.right_child.as_ref().unwrap().as_ref().right().unwrap();
                        rhs.inspect()
                    };

                    let out = format!(
                        "
NODE({kind:#?})
    ---NODE(LHS : {lhs_str})
    ---NODE(RHS : {rhs_str})"
                    );
                    out
                }

                IdentifierKind::FUNCTION => {
                    let mut params_str = String::from("");
                    if let Some(params_vec) = self.func_params.as_ref() {
                        let params = params_vec.clone();
                        for param in params.iter() {
                            if param.is_left() {
                                let left = param.as_ref().left().unwrap();
                                let left_str = left.inspect();
                                let left_str = format!("- {left_str}; ");
                                params_str.push_str(left_str.as_str());
                            } else {
                                let right = param.as_ref().right().unwrap();
                                let right_str = right.inspect();
                                let right_str = format!("- {right_str}; ");
                                params_str.push_str(right_str.as_str());
                            }
                        }
                    }

                    let mut block_children_str = String::from("");
                    if let Some(block_children_vec) = self.block_children.as_ref() {
                        let children = block_children_vec.clone();
                        for child in children.iter() {
                            let child_str = child.inspect();
                            block_children_str.push_str(child_str.as_str());
                        }
                    }

                    let out = format!(
                        "
                    NODE({kind:#?}) 
                        ---PARAMS {params_str} 
                        ---BLOCK {block_children_str}"
                    );
                    out
                }

                IdentifierKind::RETURN => {
                    let child = self.right_child.as_ref().unwrap().as_ref().right().unwrap();
                    let child_str = child.inspect();
                    let out = format!(
                        "
                            NODE({kind:#?}) {child_str}"
                    );
                    out
                }

                IdentifierKind::PLUS
                | IdentifierKind::MINUS
                | IdentifierKind::ASTERISK
                | IdentifierKind::SLASH
                | IdentifierKind::MODULUS
                | IdentifierKind::GT
                | IdentifierKind::GTOREQ
                | IdentifierKind::LT
                | IdentifierKind::LTOREQ
                | IdentifierKind::EQ
                | IdentifierKind::OR
                | IdentifierKind::AND => {
                    let left_child = self.left_child.as_ref().unwrap().as_ref().right().unwrap();
                    let left_child_str = left_child.inspect();

                    let right_child = self.right_child.as_ref().unwrap().as_ref().right().unwrap();
                    let right_child_str = right_child.inspect();

                    let out = format!(
                        "
                                NODE({kind:#?})
                                    ---NODE(LHS : {left_child_str})
                                    ---NODE(RHS : {right_child_str})"
                    );

                    out
                }

                IdentifierKind::LPAREN => {
                    println!("current {:?}", self);
                    todo!("implement")
                }

                IdentifierKind::INTLITERAL
                | IdentifierKind::BOOLEANLITERAL
                | IdentifierKind::STRINGLITERAL => {
                    let lit = self.left_child.as_ref().unwrap().as_ref().left().unwrap();
                    lit.inspect()
                }

                IdentifierKind::EOS => "".to_string(),

                _ => todo!("implement inspect method for kind '{:?}'", kind),
            }
        } else {
            "Nothing to inspect".to_string()
        }
    }
}
