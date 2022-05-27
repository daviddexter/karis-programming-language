use std::cell::RefCell;
use std::rc::Rc;

use errors::errors;
use lexer::lexer::Lexer;
use lexer::tokens::{IndentifierKind, Token};

use crate::objects::{Objects, Program};

pub struct Tracker {
    main_start_position: usize,
    let_start_position: usize,
    seen_main: bool,
}

#[derive(Debug, Clone)]
pub struct Parser {
    pub lexer: Lexer,

    // holds all the tokens extracted by the lexer
    bucket: Vec<Token>,

    // the program tree that will be built when parsing
    program: Program,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        Self {
            lexer,
            bucket: Vec::new(),
            program: Program::default(),
        }
    }

    pub fn parse(&mut self) -> Result<Objects, errors::KarisError> {
        // add all tokens to a bucket cache. This will be used later when doing the actual parsing
        loop {
            let token = self.lexer.generate()?;
            if token.token_type == IndentifierKind::EOF {
                break;
            } else {
                self.bucket.push(token.clone());
            }
        }

        // group statements. This is done by using the criteria of, get all tokens until a `EOS` is encountered.
        // when a `EOS` is encountered, gather all token prior and add them as a unit in the statements cache
        let statements = Rc::new(RefCell::new(Vec::new()));
        self.combine_statements(
            0x0,
            &statements,
            &Tracker {
                seen_main: false,
                main_start_position: 0x0,
                let_start_position: 0x0,
            },
        );

        self.parse_statement_tokens(&statements)?;

        Err(errors::KarisError {
            error_type: errors::KarisErrorType::MissingVariableName,
            message: String::from("expected a variable name to bind the literal string"),
        })
    }

    fn combine_statements(
        &self,
        index: usize,
        statements: &Rc<RefCell<Vec<Vec<Token>>>>,
        track: &Tracker,
    ) {
        // base case: reached end of the bucket
        if index == self.bucket.len() {
            return;
        }

        let token = self.bucket.get(index).unwrap();

        if track.seen_main && (token.token_type != IndentifierKind::END) {
            self.combine_statements(index + 0x1, statements, track)
        }

        if track.seen_main && (token.token_type == IndentifierKind::END) {
            // combine all statements in main block
            let main_block_statements = &self.bucket[track.main_start_position..index + 0x1];
            let statement_clone = statements.clone();
            let mut sts = statement_clone.borrow_mut();
            sts.push(main_block_statements.to_vec());
            self.combine_statements(index + 0x1, statements, track)
        }

        if !track.seen_main && (token.token_type == IndentifierKind::EOS) {
            // combine all statements in let block
            let let_block_statements = &self.bucket[track.let_start_position..index + 0x1];
            let statement_clone = statements.clone();
            let mut sts = statement_clone.borrow_mut();
            sts.push(let_block_statements.to_vec());
            self.combine_statements(index + 0x1, statements, track)
        }

        match token.token_type {
            IndentifierKind::MAIN => self.combine_statements(
                index + 0x1,
                statements,
                &Tracker {
                    seen_main: true,
                    main_start_position: index,
                    let_start_position: 0x0,
                },
            ),
            IndentifierKind::LET => self.combine_statements(
                index + 0x1,
                statements,
                &Tracker {
                    seen_main: false,
                    main_start_position: 0x0,
                    let_start_position: index,
                },
            ),
            _ => self.combine_statements(index + 0x1, statements, track),
        }
    }

    // responsible for constructing an expression from grouped tokens
    fn parse_statement_tokens(
        &mut self,
        statements: &Rc<RefCell<Vec<Vec<Token>>>>,
    ) -> Result<(), errors::KarisError> {
        let statement_clone = statements.clone();
        let sts = statement_clone.borrow();
        for statement in sts.iter() {
            let obj = self.build_expression(statement)?;
            self.program.body.push(obj);
        }
        Ok(())
    }

    /// build an expression that speaks to how token relate to each other
    /// These trees will be added to the program, which is the root of the tree
    /// consider these trees as nodes of the program
    fn build_expression(&self, tokens: &Vec<Token>) -> Result<Objects, errors::KarisError> {
        let default = Token::default();
        let first = tokens.first().unwrap_or(&default);

        if first.token_type == IndentifierKind::MAIN {
            todo!("main defs")
        } else if first.token_type == IndentifierKind::LET {
            let obj = Rc::new(RefCell::new(Objects::TyUnknown));
            self.parse_let_expressions(tokens, 0, &obj)
        } else {
            let msg = format!("expected `Let` or `main`l found {:?}", first.token_type);
            let err = errors::KarisError {
                error_type: errors::KarisErrorType::UnknownToken,
                message: msg,
            };
            Err(err)
        }
    }

    // recursively parses and build an expression tree from a let expression
    // results in either :
    // - LiteralExpression
    // - FunctionExpression
    // - CallExpression
    // - ReturnExpression
    fn parse_let_expressions(
        &self,
        tokens: &Vec<Token>,
        index: usize,
        object: &Rc<RefCell<Objects>>,
    ) -> Result<Objects, errors::KarisError> {
        let objects_clone = object.clone();
        let obj_ref = objects_clone.borrow();

        // base case: reached end of the bucket
        if index == tokens.len() {
            let obj = &*obj_ref;
            return Ok(obj.clone());
        }

        let token = &tokens[index];

        match token.token_type {
            IndentifierKind::LET
            | IndentifierKind::VARIABLE
            | IndentifierKind::INTTYPE
            | IndentifierKind::STRINGTYPE
            | IndentifierKind::BOOLEANTYPE => self.parse_let_expressions(tokens, index + 1, object),

            _ => self.parse_let_expressions(tokens, index + 1, object),
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn should_parse1() {
        let lx = Lexer::new(String::from("let name = \"alice\";"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_err())
    }

    #[test]
    fn should_parse2() {
        let lx = Lexer::new(String::from("let name @string = \"alice\";"));
        let mut parser = Parser::new(lx);
        let _res = parser.parse();
        assert_eq!(parser.bucket.len(), 7);
        if let Some(last) = parser.bucket.last() {
            assert_eq!(last.token_type, IndentifierKind::EOS);
        }
    }

    // #[test]
    // fn should_parse_file() -> std::io::Result<()>{
    //     let cwd = std::env::current_dir()?;
    //     let parent = cwd.parent().unwrap();
    //     let path = parent.join("testdata/test1.kr");
    //     let file = std::fs::read_to_string(path)?;
    //     let lx = Lexer::new(file);
    //     let mut parser = Parser::new(lx);
    //     let res = parser.parse();
    //     assert!(res.is_err());
    //     Ok(())
    // }
}
