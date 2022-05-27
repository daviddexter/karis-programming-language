#[derive(Debug,Clone)]
pub enum DeclarationType {    
    Unknown,
    Program,
    LiteralExpression,  
    FunctionExpression,  
    BinaryExpression,
    IfExpression,
    ReturnExpression,
    CallExpression,
    MainExpression,
}

impl Default for DeclarationType {
    fn default() -> Self { DeclarationType::Unknown }
}

#[derive(Debug, Clone)]
pub enum TypingKind {
    Int,
    String,
    Boolean,

    // TODO: add this in the lexer first 
    Array,
    Map
}

// Declaration : an object must be to tell what is it
// `which` returns what the object knows about itself
pub trait Declaration {
    // returns the type of the current declaration object
    fn which(&self) -> DeclarationType;
}

pub trait Value {
    // returns the type of the current declaration object
    fn which(&self) -> TypingKind;
}


#[derive(Debug, Clone)]
pub enum Objects { 
    TyUnknown,  
    TyProgram(Program),
    TyLiteralExpression(LiteralExpression), 
    TyFunctionExpression(FunctionExpression), 
    TyBinaryExpression(BinaryExpression),
    TyIfExpression(IfExpression),
    TyReturnExpression(ReturnExpression),
    TyCallExpression(CallExpression),
    TyMainExpression(MainExpression)
}

impl Declaration for Objects {
    fn which(&self) -> DeclarationType {
        match &self {
            Objects::TyUnknown => DeclarationType::Unknown,
            Objects::TyProgram(i) => i.which(),
            Objects::TyLiteralExpression(i) => i.which(),  
            Objects::TyFunctionExpression(i) => i.which(), 
            Objects::TyBinaryExpression(i) => i.which(), 
            Objects::TyIfExpression(i) => i.which(),
            Objects::TyReturnExpression(i) => i.which(),  
            Objects::TyCallExpression(i) => i.which(),    
            Objects::TyMainExpression(i) => i.which(),     
        }
    }
}

impl Default for Objects {
    fn default() -> Self { Objects::TyUnknown}
}

// Represents literal values definitions
#[derive(Debug, Clone)]
pub enum LiteralObjects {    
    ObjIntergerValue(IntergerValue),  
    ObjBooleanValue(BooleanValue),  
    ObjStringValue(StringValue),
}

impl Value for LiteralObjects {
    fn which(&self) -> TypingKind {
        match &self {
            LiteralObjects::ObjIntergerValue(i) => i.which(),  
            LiteralObjects::ObjBooleanValue(i) => i.which(),  
            LiteralObjects::ObjStringValue(i) => i.which(),                 
        }
    }
}

// Interger values representation
#[derive(Debug, Default,Clone)]
pub struct IntergerValue {
    pub value: Option<isize>      
}

impl Value for IntergerValue {
    fn which(&self) -> TypingKind  {
        TypingKind::Int
    }
}

impl IntergerValue {
    pub fn add_value(&mut self, value:isize) {
        self.value = Some(value);
    }
}


// Boolean values representation
#[derive(Debug, Default,Clone)]
pub struct BooleanValue {
    pub value: Option<bool>      
}

impl Value for BooleanValue {
    fn which(&self) -> TypingKind  {
        TypingKind::Boolean
    }
}

impl BooleanValue {
    pub fn add_value(&mut self,value:bool) {
        self.value = Some(value);
    }
}

// String values representation
#[derive(Debug, Default,Clone)]
pub struct StringValue {
    pub value: Option<String>      
}

impl Value for StringValue {
    fn which(&self) -> TypingKind  {
        TypingKind::String
    }
}

impl StringValue {
    pub fn add_value(&mut self,value:String) {
        self.value = Some(value);
    }
}


// Program is the root declaration. It will be at the top of the AST
#[derive(Debug,Default, Clone)]
pub struct Program {
    pub body: Vec<Objects>,
}

impl Declaration for Program {
    fn which(&self) -> DeclarationType {
        DeclarationType::Program
    }
}

impl Program {   
    pub fn add_object(&mut self, object: Objects){
        self.body.push(object)
    }
    pub fn count(&self) -> usize {
        self.body.len()
    }
}



// LiteralExpression are `let` binding. These expressions have an identifier, a type information, and the value
// Example
//      let y @int = 7;
//      let active @bool = true;
//      let name @string = "Karis";
//      let numbers @array:int = [1,2,3,4,5];
//      let values @map:int:string = {0 : "0", 1 : "1", 2 : "2"};
#[derive(Debug, Default,Clone)]
pub struct LiteralExpression {
    pub identifier: Option<String>,
    pub typing: Option<TypingKind>,
    pub value: Option<LiteralObjects>,    
}

impl Declaration for LiteralExpression {
    fn which(&self) -> DeclarationType {
        DeclarationType::LiteralExpression
    }
}

impl LiteralExpression {    
    pub fn add_identifier(&mut self,identifier: String) {
        self.identifier = Some(identifier);
    }

    pub fn add_typing(&mut self, typing : TypingKind) {
        self.typing = Some(typing);
    }

    pub fn add_value(&mut self, value : LiteralObjects) {
        self.value = Some(value);
    }
}

// FunctionExpression is a definition of a function that may take arguments and/or produce results often of the type
// `ReturnExpression`
// Example:
//      let add = fn(x @int, y @int) @int{
//          return x + y;
//      };
//
#[derive(Debug, Default,Clone)]
pub struct FunctionExpression {
    pub identifier: Option<String>,
    pub typing: Option<TypingKind>,
    pub params:  Option<Vec<Objects>>,
    pub block: Option<Vec<Objects>>,   
}

impl Declaration for FunctionExpression {
    fn which(&self) -> DeclarationType {
        DeclarationType::FunctionExpression
    }
}

impl FunctionExpression {    
    pub fn add_identifier(&mut self,identifier: String) {
        self.identifier = Some(identifier);
    }

    pub fn add_typing(&mut self, typing : TypingKind) {
        self.typing = Some(typing);
    }

    pub fn add_params(&mut self, params:Vec<Objects>) {
        self.params = Some(params);
    }
    pub fn add_block(&mut self, body:Vec<Objects>) {
        self.block = Some(body);
    }
}


// BinaryExpression takes different forms. At it's core, there is an `operator`
// that evaluates the `lhs` and `rhs`
// Example:
//     let x @int = 1 + 2;
// The first part (before the = ) is the identifier with typing information
// The second part (after the = ) we hav `1` on the lhs and `2` on the rhs. In the middle, `+` operator
//  
#[derive(Debug, Default,Clone)]
pub struct BinaryExpression { 
    pub identifier: Option<String>,  
    pub typing: Option<TypingKind>, 
    pub lhs:  Option<Box<Objects>>,
    pub operator: Option<String>,
    pub rhs:  Option<Box<Objects>>,   
}

impl Declaration for BinaryExpression {
    fn which(&self) -> DeclarationType {
        DeclarationType::BinaryExpression
    }
}

impl BinaryExpression {  
    pub fn add_identifier(&mut self,identifier: String) {
        self.identifier = Some(identifier);
    }

    pub fn add_typing(&mut self,typing: TypingKind) {
        self.typing = Some(typing);
    }

    pub fn add_operator(&mut self,operator: String) {
        self.operator = Some(operator);
    }

    pub fn add_rhs(&mut self,rhs: Objects) {
        self.rhs = Some(Box::new(rhs));
    }    
}


// IfExpression .. 
#[derive(Debug, Default,Clone)]
pub struct IfExpression {
    // the conditional to be meant   
    pub test:  Option<Box<Objects>>,

    // this is the result if the `test` passes
    pub consequent: Option<Box<Objects>>,    
    
    // this can be `else if` block or a tail `else` block
    pub alternate: Option<Box<Objects>>,  
}

impl Declaration for IfExpression {
    fn which(&self) -> DeclarationType {
        DeclarationType::IfExpression
    }
}

impl IfExpression {    
    pub fn add_test(&mut self,test: Objects) {
        self.test = Some(Box::new(test));
    }

    pub fn add_consequent(&mut self,consequent: Objects) {
        self.consequent = Some(Box::new(consequent));
    }   
    
    pub fn add_alternate(&mut self,alternate: Objects) {
        self.alternate = Some(Box::new(alternate));
    }
    
}


// ReturnExpression ...
#[derive(Debug, Default,Clone)]
pub struct ReturnExpression {    
    pub argument:  Option<Box<Objects>>,         
}

impl Declaration for ReturnExpression {
    fn which(&self) -> DeclarationType {
        DeclarationType::IfExpression
    }
}

impl ReturnExpression {    
    pub fn add_argument(&mut self,arg: Objects) {
        self.argument = Some(Box::new(arg));
    }          
}


// CallExpression represent a call to a function that has been previously been defined.
// The function can take any number of optional arguments of type `LiteralObjects`.
// This expression will be used for built-in functions as well
#[derive(Debug, Default,Clone)]
pub struct CallExpression { 
    pub identifier: Option<String>,   
    pub arguments:  Option<Vec<LiteralObjects>>,         
}

impl Declaration for CallExpression {
    fn which(&self) -> DeclarationType {
        DeclarationType::CallExpression
    }
}

impl CallExpression {   
    pub fn add_identifier(&mut self, ident: String){
        self.identifier = Some(ident);
    }
    
    pub fn add_argument(&mut self,arguments: Vec<LiteralObjects>) {
        self.arguments = Some(arguments);
    }          
}


// MainExpression is the root of the program that will be executed
#[derive(Debug, Default,Clone)]
pub struct MainExpression {       
    pub body:  Option<Vec<Objects>>,         
}

impl Declaration for MainExpression {
    fn which(&self) -> DeclarationType {
        DeclarationType::MainExpression
    }
}

impl MainExpression {   
    pub fn add_body(&mut self, body: Vec<Objects>){
        self.body = Some(body);
    }   
             
}