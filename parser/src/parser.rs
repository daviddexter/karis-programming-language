use std::cell::RefCell;
use std::rc::Rc;

use errors::errors;
use lexer::lexer::Lexer;
use lexer::tokens::{IndentifierKind, Token};

// use crate::declarations;
// use crate::identifier_declaration::IdentifierDeclaration;
// use crate::literal_declaration::LiteralDeclaration;
use crate::objects::Objects;
use crate::program_declaration::ProgramDeclaration;
// use crate::variable_declaration::VariableDeclaration;
// use crate::variable_declarator::VariableDeclarator;

pub struct Tracker {
    main_start_position: usize,  
    let_start_position: usize,   
    seen_main: bool
}

#[derive(Debug, Clone)]
pub struct Parser {
    pub lexer: Lexer,

    // holds all the tokens extracted by the lexer
    bucket: Vec<Token>,    

    // the program tree that will be built when parsing    
    program : ProgramDeclaration
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        Self {
            lexer,
            bucket: Vec::new(),                      
            program: ProgramDeclaration::new(),
        }
    }

    pub fn parse(&mut self) -> Result<Objects, errors::KarisError> {
        loop {
            let token = self.lexer.generate()?;
            if token.token_type == IndentifierKind::EOF {
                break;
            } else {
                self.bucket.push(token.clone());
            }
        } 

        let statements = Rc::new(RefCell::new(Vec::new()));
        self.combine_statements(0x0,&statements,
        &Tracker{seen_main:false,main_start_position: 0x0 ,let_start_position:0x0});

        Err(errors::KarisError{error_type: errors::KarisErrorType::MissingVariableName,
            message: String::from("expected a variable name to bind the literal string") })
    }

    fn combine_statements(&self, index:usize, statements: &Rc<RefCell<Vec<Vec<Token>>>>,track:&Tracker){
        // base case: reached end of the bucket
        if index == self.bucket.len(){
            return;
        }

        let token = self.bucket.get(index).unwrap();

        if track.seen_main && (token.token_type != IndentifierKind::END){
            self.combine_statements(index+0x1,statements, track)
        }

        if track.seen_main && (token.token_type == IndentifierKind::END){
            // combine all statements in main block
            let main_block_statements = &self.bucket[track.main_start_position..index+0x1];
            let statement_clone = statements.clone();
            let mut sts = statement_clone.borrow_mut();
            sts.push(main_block_statements.to_vec());
            self.combine_statements(index+0x1,statements, track)
        }

        if !track.seen_main && (token.token_type == IndentifierKind::EOS){
            // combine all statements in let block            
            let let_block_statements = &self.bucket[track.let_start_position..index+0x1];
            let statement_clone = statements.clone();
            let mut sts = statement_clone.borrow_mut();
            sts.push(let_block_statements.to_vec());
            self.combine_statements(index+0x1,statements, track)                                    
        }        
        
        match token.token_type{
            IndentifierKind::MAIN => {
                self.combine_statements(index+0x1,statements, &Tracker{ seen_main:true, 
                    main_start_position: index,let_start_position:0x0})
            },
            IndentifierKind::LET => {
                self.combine_statements(index+0x1,statements,&Tracker{ seen_main:false, 
                    main_start_position: 0x0,let_start_position:index})
            }
            _ => self.combine_statements(index+0x1,statements,track),
        }
    }   

}

#[cfg(test)]
mod tests {

    use super::*;   

    #[test]
    fn should_statement1() {
        let lx = Lexer::new(String::from("let name = \"alice\";"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_err())
    }

    #[test]
    fn should_statement2() {
        let lx = Lexer::new(String::from("let name @string = \"alice\";"));
        let mut parser = Parser::new(lx);
        let _res = parser.parse();
        assert_eq!(parser.bucket.len(), 7);
        if let Some(last) = parser.bucket.last() {
            assert_eq!(last.token_type, IndentifierKind::EOS);
        }        
    }

    #[test]
    fn should_statement3() -> std::io::Result<()>{         
        let cwd = std::env::current_dir()?;
        let parent = cwd.parent().unwrap();       
        let path = parent.join("testdata/test1.kr");        
        let file = std::fs::read_to_string(path)?;
        let lx = Lexer::new(file);        
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_err());
        Ok(())
    }
}
