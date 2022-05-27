use std::cell::RefCell;
use std::rc::Rc;

use errors::errors;
use lexer::lexer::Lexer;
use lexer::tokens::{IndentifierKind, Token};

use crate::objects::*;

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

        match self.parse_statement_tokens(&statements) {
            Ok(_) => Ok(Objects::TyProgram(self.program.clone())),
            Err(e) => Err(e),
        }
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

        match first.token_type {
            IndentifierKind::MAIN => todo!("main defs"),
            IndentifierKind::LET => {
                let obj = Rc::new(RefCell::new(Objects::TyUnknown));
                self.parse_let_expressions(tokens, 0, &obj)
            }
            _ => {
                let msg = format!("expected `Let` or `main` found {:?}", first.token_type);
                let err = errors::KarisError {
                    error_type: errors::KarisErrorType::UnknownToken,
                    message: msg,
                };
                Err(err)
            }
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
        // base case: reached end of the bucket
        if index == tokens.len() {
            let objects_clone = object.clone();
            let obj_ref = objects_clone.borrow();
            let obj = &*obj_ref;

            if obj.which() == DeclarationType::Unknown {
                let msg = format!("parse error : invalid syntax `{:?}`", tokens);
                let err = errors::KarisError {
                    error_type: errors::KarisErrorType::InvalidSyntax,
                    message: msg,
                };
                return Err(err);
            } else {
                return Ok(obj.clone());
            }
        }

        let token = &tokens[index];

        match token.token_type {
            IndentifierKind::LET
            | IndentifierKind::VARIABLE
            | IndentifierKind::INTTYPE
            | IndentifierKind::STRINGTYPE
            | IndentifierKind::BOOLEANTYPE => self.parse_let_expressions(tokens, index + 1, object),

            IndentifierKind::ASSIGN => {
                // literals : intergers, string, booleans
                let next_token = &tokens[index + 1];

                // Example: let num @int = 123;
                if next_token.token_type == IndentifierKind::INTLITERAL {
                    self.interger_literal_expression(tokens, object)?;
                }

                // Example: let name @string = "greyworm";
                if next_token.token_type == IndentifierKind::STRINGLITERAL {
                    self.string_literal_expression(tokens, object)?;
                }

                // Example: let active @bool = true;
                // Example: let active @bool = false;
                if next_token.token_type == IndentifierKind::TRUE
                    || next_token.token_type == IndentifierKind::FALSE
                {
                    self.boolean_literal_expression(tokens, object)?;
                }

                self.parse_let_expressions(tokens, index + 1, object)
            }

            _ => self.parse_let_expressions(tokens, index + 1, object),
        }
    }

    fn interger_literal_expression(
        &self,
        tokens: &[Token],
        object: &Rc<RefCell<Objects>>,
    ) -> Result<(), errors::KarisError> {
        // assert the type is of an INT and not missing
        let res = self.typing_mismatch(&tokens[0x02], IndentifierKind::INTTYPE);
        if let Err(err) = res {
            return Err(err);
        }

        let objects_clone = object.clone();
        let mut obj_ref = objects_clone.borrow_mut();

        let variable_name = &tokens[0x01].literal;
        let mut lit = LiteralExpression::default();
        lit.add_identifier(variable_name.to_string());
        lit.add_typing(TypingKind::Int);

        let semicolon_position_token = &tokens[0x05];

        if semicolon_position_token.token_type != IndentifierKind::SEMICOLON {
            //  parse expression
            todo!("implement parse expression")
        } else {
            let val = &tokens[0x04].literal;
            let val0 = val.parse::<isize>().unwrap();
            let mut val1 = IntergerValue::default();
            val1.add_value(val0);
            lit.add_value(LiteralObjects::ObjIntergerValue(val1));
        }

        *obj_ref = Objects::TyLiteralExpression(lit);

        Ok(())
    }

    fn string_literal_expression(
        &self,
        tokens: &[Token],
        object: &Rc<RefCell<Objects>>,
    ) -> Result<(), errors::KarisError> {
        // assert the type is of an STRING and not missing
        let res = self.typing_mismatch(&tokens[0x02], IndentifierKind::STRINGTYPE);
        if let Err(err) = res {
            return Err(err);
        }

        let objects_clone = object.clone();
        let mut obj_ref = objects_clone.borrow_mut();

        let variable_name = &tokens[0x01].literal;
        let mut lit = LiteralExpression::default();
        lit.add_identifier(variable_name.to_string());
        lit.add_typing(TypingKind::String);

        let val0 = &tokens[0x04].literal;
        let mut val1 = StringValue::default();
        val1.add_value(val0.to_string());
        lit.add_value(LiteralObjects::ObjStringValue(val1));

        *obj_ref = Objects::TyLiteralExpression(lit);

        Ok(())
    }

    fn boolean_literal_expression(
        &self,
        tokens: &[Token],
        object: &Rc<RefCell<Objects>>,
    ) -> Result<(), errors::KarisError> {
        // assert the type is of an BOOL and not missing
        let res = self.typing_mismatch(&tokens[0x02], IndentifierKind::BOOLEANTYPE);
        if let Err(err) = res {
            return Err(err);
        }

        let objects_clone = object.clone();
        let mut obj_ref = objects_clone.borrow_mut();

        let variable_name = &tokens[0x01].literal;
        let mut lit = LiteralExpression::default();
        lit.add_identifier(variable_name.to_string());
        lit.add_typing(TypingKind::Boolean);

        let val0 = &tokens[0x04].literal;
        let mut val1 = BooleanValue::default();
        val1.add_value(val0.parse::<bool>().unwrap());
        lit.add_value(LiteralObjects::ObjBooleanValue(val1));

        *obj_ref = Objects::TyLiteralExpression(lit);

        Ok(())
    }

    fn typing_mismatch(
        &self,
        typing_position_token: &Token,
        matcher: IndentifierKind,
    ) -> Result<(), errors::KarisError> {
        if typing_position_token.token_type != matcher {
            let msg = format!(
                "parse error : expected `@int` at position 2. Found {:?} Ln {}, Col {}",
                typing_position_token.token_type,
                typing_position_token.line_number,
                typing_position_token.column_number
            );
            let err = errors::KarisError {
                error_type: errors::KarisErrorType::MissingTypeInfo,
                message: msg,
            };
            return Err(err);
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn should_parse1() {
        let lx = Lexer::new(String::from("let num @int = 1;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse2() -> std::io::Result<()> {
        let lx = Lexer::new(String::from("let num @int = 1;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse()?;
        assert_eq!(res.which(), DeclarationType::Program);
        match res {
            Objects::TyProgram(p) => {
                assert_eq!(p.body.len(), 1);
                let first = &p.body[0];
                assert_eq!(first.which(), DeclarationType::LiteralExpression);
                match first {
                    Objects::TyLiteralExpression(l) => {
                        assert_eq!(l.identifier.as_ref().unwrap(), &String::from("num"));
                        assert_eq!(l.typing.as_ref().unwrap(), &TypingKind::Int);

                        let value = l.value.as_ref().unwrap();
                        assert_eq!(value.which(), TypingKind::Int);
                        match value {
                            LiteralObjects::ObjIntergerValue(s) => {
                                let v = s.value.as_ref().unwrap();
                                let t = 1 as isize;
                                assert_eq!(v, &t);
                            }
                            _ => panic!("expected `IntergerValue` but found {:?}", value),
                        }
                    }
                    _ => panic!("expected `LiteralExpression` but found {:?}", first),
                }
            }
            _ => panic!("expected `Program` but found {:?}", res),
        }
        Ok(())
    }

    #[test]
    fn should_parse3() {
        let lx = Lexer::new(String::from("let num = 1;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_err());
        let err = res.unwrap_err();
        println!("{:?}", err.message);
        assert_eq!(
            err.message,
            "parse error : expected `@int` at position 2. Found ASSIGN Ln 1, Col 8"
        );
        assert_eq!(err.error_type, errors::KarisErrorType::MissingTypeInfo)
    }

    #[test]
    fn should_parse4() {
        let lx = Lexer::new(String::from("let num 1;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_err());
        let err = res.unwrap_err();
        println!("{:?}", err.message);
        assert_eq!(err.error_type, errors::KarisErrorType::InvalidSyntax)
    }

    #[test]
    fn should_parse5() -> std::io::Result<()> {
        let lx = Lexer::new(String::from("let name @string = \"alice\";"));
        let mut parser = Parser::new(lx);
        let res = parser.parse()?;
        assert_eq!(res.which(), DeclarationType::Program);
        match res {
            Objects::TyProgram(p) => {
                assert_eq!(p.body.len(), 1);
                let first = &p.body[0];
                assert_eq!(first.which(), DeclarationType::LiteralExpression);
                match first {
                    Objects::TyLiteralExpression(l) => {
                        assert_eq!(l.identifier.as_ref().unwrap(), &String::from("name"));
                        assert_eq!(l.typing.as_ref().unwrap(), &TypingKind::String);

                        let value = l.value.as_ref().unwrap();
                        assert_eq!(value.which(), TypingKind::String);
                        match value {
                            LiteralObjects::ObjStringValue(s) => {
                                let v = s.value.as_ref().unwrap();
                                assert_eq!(v, &String::from("alice"));
                            }
                            _ => panic!("expected `StringValue` but found {:?}", value),
                        }
                    }
                    _ => panic!("expected `LiteralExpression` but found {:?}", first),
                }
            }
            _ => panic!("expected `Program` but found {:?}", res),
        }
        Ok(())
    }

    #[test]
    fn should_parse6() -> std::io::Result<()> {
        let lx = Lexer::new(String::from("let active @bool = true;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse()?;
        assert_eq!(res.which(), DeclarationType::Program);
        match res {
            Objects::TyProgram(p) => {
                assert_eq!(p.body.len(), 1);
                let first = &p.body[0];
                assert_eq!(first.which(), DeclarationType::LiteralExpression);
                match first {
                    Objects::TyLiteralExpression(l) => {
                        assert_eq!(l.identifier.as_ref().unwrap(), &String::from("active"));
                        assert_eq!(l.typing.as_ref().unwrap(), &TypingKind::Boolean);

                        let value = l.value.as_ref().unwrap();
                        assert_eq!(value.which(), TypingKind::Boolean);
                        match value {
                            LiteralObjects::ObjBooleanValue(s) => {
                                let v = s.value.as_ref().unwrap();
                                assert_eq!(v, &true);
                            }
                            _ => panic!("expected `BooleanValue` but found {:?}", value),
                        }
                    }
                    _ => panic!("expected `LiteralExpression` but found {:?}", first),
                }
            }
            _ => panic!("expected `Program` but found {:?}", res),
        }
        Ok(())
    }

    #[test]
    fn should_parse7() -> std::io::Result<()> {
        let lx = Lexer::new(String::from("let active @bool = false;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse()?;
        assert_eq!(res.which(), DeclarationType::Program);
        match res {
            Objects::TyProgram(p) => {
                assert_eq!(p.body.len(), 1);
                let first = &p.body[0];
                assert_eq!(first.which(), DeclarationType::LiteralExpression);
                match first {
                    Objects::TyLiteralExpression(l) => {
                        assert_eq!(l.identifier.as_ref().unwrap(), &String::from("active"));
                        assert_eq!(l.typing.as_ref().unwrap(), &TypingKind::Boolean);

                        let value = l.value.as_ref().unwrap();
                        assert_eq!(value.which(), TypingKind::Boolean);
                        match value {
                            LiteralObjects::ObjBooleanValue(s) => {
                                let v = s.value.as_ref().unwrap();
                                assert_eq!(v, &false);
                            }
                            _ => panic!("expected `BooleanValue` but found {:?}", value),
                        }
                    }
                    _ => panic!("expected `LiteralExpression` but found {:?}", first),
                }
            }
            _ => panic!("expected `Program` but found {:?}", res),
        }
        Ok(())
    }

    // #[test]
    // fn should_parse3() {
    //     let lx = Lexer::new(String::from("let name @string = \"alice\";"));
    //     let mut parser = Parser::new(lx);
    //     let _res = parser.parse();
    //     assert_eq!(parser.bucket.len(), 7);
    //     if let Some(last) = parser.bucket.last() {
    //         assert_eq!(last.token_type, IndentifierKind::EOS);
    //     }
    // }

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
