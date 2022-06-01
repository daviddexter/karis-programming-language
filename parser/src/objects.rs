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

// Declaration : an object must be to tell what is it
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

// #[derive(Debug, Eq, PartialEq, Clone)]
// pub enum Operator {
//     Unknown,
//     Add,
//     Subtract,
//     Multiply,
//     Divide,
// }

// impl Default for Operator {
//     fn default() -> Self {
//         Operator::Unknown
//     }
// }

// // FunctionExpression is a definition of a function that may take arguments and/or produce results often of the type
// // `ReturnExpression`
// // Example:
// //      let add = fn(x @int, y @int) @int{
// //          return x + y;
// //      };
// //
// #[derive(Debug, Default, Clone)]
// pub struct FunctionExpression {
//     pub identifier: Option<String>,
//     pub typing: Option<TypingKind>,
//     pub params: Option<Vec<Objects>>,
//     pub block: Option<Vec<Objects>>,
// }

// impl Declaration for FunctionExpression {
//     fn which(&self) -> DeclarationType {
//         DeclarationType::FunctionExpression
//     }
// }

// impl FunctionExpression {
//     pub fn add_identifier(&mut self, identifier: String) {
//         self.identifier = Some(identifier);
//     }

//     pub fn add_typing(&mut self, typing: TypingKind) {
//         self.typing = Some(typing);
//     }

//     pub fn add_params(&mut self, params: Vec<Objects>) {
//         self.params = Some(params);
//     }
//     pub fn add_block(&mut self, body: Vec<Objects>) {
//         self.block = Some(body);
//     }
// }

// // BinaryExpression takes different forms. At it's core, there is an `operator`
// // that evaluates the `lhs` and `rhs`
// // Example:
// //     let x @int = 1 + 2;
// // The first part (before the = ) is the identifier with typing information
// // The second part (after the = ) we hav `1` on the lhs and `2` on the rhs. In the middle, `+` operator
// //
// #[derive(Debug, Default, Clone)]
// pub struct BinaryExpression {
//     pub identifier: Option<String>,
//     pub typing: Option<TypingKind>,
//     pub lhs: Option<Box<Objects>>,
//     pub operator: Option<String>,
//     pub rhs: Option<Box<Objects>>,
// }

// impl Declaration for BinaryExpression {
//     fn which(&self) -> DeclarationType {
//         DeclarationType::BinaryExpression
//     }
// }

// impl BinaryExpression {
//     pub fn add_identifier(&mut self, identifier: String) {
//         self.identifier = Some(identifier);
//     }

//     pub fn add_typing(&mut self, typing: TypingKind) {
//         self.typing = Some(typing);
//     }

//     pub fn add_operator(&mut self, operator: String) {
//         self.operator = Some(operator);
//     }

//     pub fn add_rhs(&mut self, rhs: Objects) {
//         self.rhs = Some(Box::new(rhs));
//     }
// }

// // IfExpression ..
// #[derive(Debug, Default, Clone)]
// pub struct IfExpression {
//     // the conditional to be meant
//     pub test: Option<Box<Objects>>,

//     // this is the result if the `test` passes
//     pub consequent: Option<Box<Objects>>,

//     // this can be `else if` block or a tail `else` block
//     pub alternate: Option<Box<Objects>>,
// }

// impl Declaration for IfExpression {
//     fn which(&self) -> DeclarationType {
//         DeclarationType::IfExpression
//     }
// }

// impl IfExpression {
//     pub fn add_test(&mut self, test: Objects) {
//         self.test = Some(Box::new(test));
//     }

//     pub fn add_consequent(&mut self, consequent: Objects) {
//         self.consequent = Some(Box::new(consequent));
//     }

//     pub fn add_alternate(&mut self, alternate: Objects) {
//         self.alternate = Some(Box::new(alternate));
//     }
// }

// // ReturnExpression ...
// #[derive(Debug, Default, Clone)]
// pub struct ReturnExpression {
//     pub argument: Option<Box<Objects>>,
// }

// impl Declaration for ReturnExpression {
//     fn which(&self) -> DeclarationType {
//         DeclarationType::IfExpression
//     }
// }

// impl ReturnExpression {
//     pub fn add_argument(&mut self, arg: Objects) {
//         self.argument = Some(Box::new(arg));
//     }
// }

// // CallExpression represent a call to a function that has been previously been defined.
// // The function can take any number of optional arguments of type `LiteralObjects`.
// // This expression will be used for built-in functions as well
// #[derive(Debug, Default, Clone)]
// pub struct CallExpression {
//     pub identifier: Option<String>,
//     pub arguments: Option<Vec<LiteralObjects>>,
// }

// impl Declaration for CallExpression {
//     fn which(&self) -> DeclarationType {
//         DeclarationType::CallExpression
//     }
// }

// impl CallExpression {
//     pub fn add_identifier(&mut self, ident: String) {
//         self.identifier = Some(ident);
//     }

//     pub fn add_argument(&mut self, arguments: Vec<LiteralObjects>) {
//         self.arguments = Some(arguments);
//     }
// }

// // MainExpression is the root of the program that will be executed
// #[derive(Debug, Default, Clone)]
// pub struct MainExpression {
//     pub body: Option<Vec<Objects>>,
// }

// impl Declaration for MainExpression {
//     fn which(&self) -> DeclarationType {
//         DeclarationType::MainExpression
//     }
// }

// impl MainExpression {
//     pub fn add_body(&mut self, body: Vec<Objects>) {
//         self.body = Some(body);
//     }
// }
