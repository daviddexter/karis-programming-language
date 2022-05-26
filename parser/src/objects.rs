#[derive(Debug,Default, Clone)]
pub enum DeclarationType {
    #[default]
    Program,
    LiteralExpression,  
    FunctionExpression,  
    BinaryExpression,
    IfExpression,
    ReturnExpression,
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

// Declaration : an object must be to tell what is it
// `which` returns what the object knows about itself
pub trait Declaration {
    // returns the type of the current declaration object
    fn which(&self) -> DeclarationType;
}


#[derive(Debug, Clone)]
pub enum Objects {    
    TyProgram(Program),
    TyLiteralExpression(LiteralExpression), 
    TyFunctionExpression(FunctionExpression), 
    TyBinaryExpression(BinaryExpression),
    TyIfExpression(IfExpression),
    TyReturnExpression(ReturnExpression)
}


impl Declaration for Objects {
    fn which(&self) -> DeclarationType {
        match &self {
            Objects::TyProgram(i) => i.which(),
            Objects::TyLiteralExpression(i) => i.which(),  
            Objects::TyFunctionExpression(i) => i.which(), 
            Objects::TyBinaryExpression(i) => i.which(), 
            Objects::TyIfExpression(i) => i.which(),
            Objects::TyReturnExpression(i) => i.which(),        
        }
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



// LiteralExpression ...
#[derive(Debug, Default,Clone)]
pub struct LiteralExpression {
    pub identifier: Option<String>,
    pub typing: Option<TypingKind>,
    pub value: Option<String>,    
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
}



// FunctionExpression ...
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



// BinaryExpression ...
#[derive(Debug, Default,Clone)]
pub struct BinaryExpression {    
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
    pub fn add_lhs(&mut self,lhs: Objects) {
        self.lhs = Some(Box::new(lhs));
    }

    pub fn add_operator(&mut self,operator: String) {
        self.operator = Some(operator);
    }

    pub fn add_rhs(&mut self,rhs: Objects) {
        self.rhs = Some(Box::new(rhs));
    }    
}


// IfExpression ...
#[derive(Debug, Default,Clone)]
pub struct IfExpression {    
    pub test:  Option<Box<Objects>>,
    pub consequent: Option<Box<Objects>>,      
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