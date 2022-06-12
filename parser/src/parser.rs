use std::cell::RefCell;
use std::rc::Rc;

use errors::errors;
use lexer::lexer::Lexer;
use lexer::tokens::{IdentifierKind, Token};

use crate::objects::*;
use crate::registry::TokenRegistry;

#[derive(Debug, Clone)]
pub struct Parser {
    pub lexer: Lexer,

    // holds all the tokens extracted by the lexer
    // pub bucket: Vec<Token>,
    pub bucket: Rc<RefCell<Vec<Token>>>,

    // the program tree that will be built when parsing
    pub program: Program,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        Self {
            lexer,
            bucket: Rc::new(RefCell::new(Vec::new())),
            program: Program::default(),
        }
    }

    pub fn parse(&mut self) -> Result<Objects, errors::KarisError> {
        // add all tokens to a bucket cache. This will be used later when doing the actual parsing
        loop {
            let token = self.lexer.generate()?;
            let bucket_clone = self.bucket.clone();
            let mut token_bucket = bucket_clone.borrow_mut();
            token_bucket.push(token.clone());
            if token.token_type == IdentifierKind::EOF {
                break;
            }
        }

        self.parse_program()
    }

    fn parse_program(&mut self) -> Result<Objects, errors::KarisError> {
        let res = self.build_program_expressions(0x0);
        if let Err(err) = res {
            return Err(err);
        }

        Ok(Objects::TyProgram(self.program.clone()))
    }

    fn build_program_expressions(&mut self, index: usize) -> Result<(), errors::KarisError> {
        if index >= self.bucket.borrow().len() {
            return Ok(());
        }

        match Self::expression(0, index, self.bucket.clone()) {
            Ok((o, i)) => {
                if !o.is_ty_unknown() {
                    self.program.add_object(o);
                }
                self.build_program_expressions(i + 0x01)
            }
            Err(e) => Err(e),
        }
    }
}

impl Parser {
    // An associated function of the parser
    // return the `Object` representation and the current index that has been worked on
    pub fn expression(
        rbp: usize,
        index: usize,
        bucket: Rc<RefCell<Vec<Token>>>,
    ) -> Result<(Objects, usize), errors::KarisError> {
        // this is the current token been parsed
        let token = &bucket.borrow()[index];

        // reusable closure
        let parser_type_fn = |s: IdentifierKind| -> Result<ParserType, errors::KarisError> {
            let rg = TokenRegistry::new();
            let parser_type = rg.retrieve_from_registry(s);
            if parser_type.is_none() {
                return Err(errors::KarisError {
                    error_type: errors::KarisErrorType::UnknownToken,
                    message: format!(
                        "[UNKNOWN TOKEN] Token {:?} Ln {} Col {}",
                        token.literal, token.line_number, token.column_number
                    ),
                });
            }

            let pt = parser_type.unwrap().clone();
            Ok(pt)
        };

        let mut left: Objects;
        let mut worked_on_index: usize;

        let pt0 = parser_type_fn(token.token_type)?;
        if let Some(func) = pt0.nud_fn {
            let res = func(token.clone(), index, bucket.clone())?;
            left = res.0;
            worked_on_index = res.1;
        } else {
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::InvalidSyntax,
                message: format!(
                    "[INVALID SYNTAX] Not a Prefix; Token {:?} Ln {} Col {}",
                    token.literal, token.line_number, token.column_number
                ),
            });
        }

        if let Some(next_token) = bucket.borrow().get(worked_on_index + 0x01) {
            let pt1 = parser_type_fn(next_token.token_type)?;
            if let Some(bp) = pt1.binding_power {
                if rbp < bp {
                    if let Some(func) = pt1.led_fn {
                        let idx = worked_on_index + 0x01;
                        let res = func(left, idx, bucket.clone())?;
                        left = res.0;
                        worked_on_index = res.1;
                    } else {
                        return Err(errors::KarisError {
                            error_type: errors::KarisErrorType::InvalidSyntax,
                            message: format!(
                                "[INVALID SYNTAX] Not an infix; Token {:?} Ln {} Col {}",
                                token.literal, token.line_number, token.column_number
                            ),
                        });
                    }
                }
            } else {
                return Err(errors::KarisError {
                    error_type: errors::KarisErrorType::MalformedProgram,
                    message: format!(
                        "[MISSING BINDING POWER] ; Token {:?} Ln {} Col {}",
                        token.literal, token.line_number, token.column_number
                    ),
                });
            }
        }

        Ok((left, worked_on_index))
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
    fn should_parse2() {
        let lx = Lexer::new(String::from("let name @string = \"alice\";"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse3() {
        let lx = Lexer::new(String::from("let num @bool = true;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse4() {
        let lx = Lexer::new(String::from("let num @bool = false;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_not_parse1() {
        let lx = Lexer::new(String::from("let num @int = "));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_err())
    }

    #[test]
    fn should_not_parse2() {
        let lx = Lexer::new(String::from("let num @int = ;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_err())
    }

    #[test]
    fn should_not_parse3() {
        let lx = Lexer::new(String::from("let num @int = let;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_err())
    }

    #[test]
    fn should_not_parse4() {
        let lx = Lexer::new(String::from("let num @int = >"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_err())
    }

    #[test]
    fn should_not_parse5() {
        let lx = Lexer::new(String::from("let num @int = =="));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_err())
    }

    #[test]
    fn should_parse5() {
        let lx = Lexer::new(String::from("let num @int = 1 + 2;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse6() {
        let lx = Lexer::new(String::from("let num @int = 1 + -2;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse7() {
        let lx = Lexer::new(String::from("let num @int = 1 + +2;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse8() {
        let lx = Lexer::new(String::from("let num @int = 1 - +2;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse9() {
        let lx = Lexer::new(String::from("let num @int = 10 * 2;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse10() {
        let lx = Lexer::new(String::from("let num @int = 10 * -2;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse11() {
        let lx = Lexer::new(String::from("let num @int = 10 / 2;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse12() {
        let lx = Lexer::new(String::from("let num @int = 10 / 2 * 3;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse13() {
        let lx = Lexer::new(String::from("let num @int = 10 / 2 * 3 + 20 - 3;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse14() {
        let lx = Lexer::new(String::from("let num @int = (10 / 2) * 3 + 20 - 3;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse15() {
        let lx = Lexer::new(String::from("let num @int = 10 / (2 * 3) + 20 - 3;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse16() {
        let lx = Lexer::new(String::from("let num @int = (10 / (2 * 3)) + 20 - 3;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse16b() {
        let lx = Lexer::new(String::from(
            "let num @int = ( (10 - 5) / (2 * 3 + 50 / 4) ) + 20 - 3;",
        ));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse16c() {
        let lx = Lexer::new(String::from("let num @int = 10(23);"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse16d() {
        let lx = Lexer::new(String::from("let num @int = 10(23 * (20 * 10 + 1));"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse16e() {
        let lx = Lexer::new(String::from("let num @int = 10(23 * (true,false));"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_err())
    }

    #[test]
    fn should_parse16f() {
        let lx = Lexer::new(String::from("10(23;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_err())
    }

    #[test]
    fn should_parse16g() {
        let lx = Lexer::new(String::from("10(23 * (20 * 10 + 1))"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse17() {
        let lx = Lexer::new(String::from("let num @int = sum() +  20 - 3;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse17b() {
        let lx = Lexer::new(String::from("let num @int = 20 + sum() +  10 - 3;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse18() {
        let lx = Lexer::new(String::from("let num @int = sum(1,2) +  20 - 3;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse19() {
        let lx = Lexer::new(String::from(
            "let num @int = sum(\"name\",\"age\") +  20 - 3;",
        ));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse20() {
        let lx = Lexer::new(String::from("let num @int = sum(\"name\",2) +  20 - 3;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse21() {
        let lx = Lexer::new(String::from("let num @int = sum(true) +  20 - 3;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse22() {
        let lx = Lexer::new(String::from("let num @int = sum(true,false) +  20 - 3;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse23() {
        let lx = Lexer::new(String::from(
            "let num @int = sum(\"name\",false,5) +  20 - 3;",
        ));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse24() {
        let lx = Lexer::new(String::from("let num @int = sum(3,4,5);"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse25() {
        let lx = Lexer::new(String::from("let num @int = sum(3,4,5) + 20;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse26() {
        let lx = Lexer::new(String::from("let num @int = sum(3,4,5 + 20;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_err())
    }

    #[test]
    fn should_parse27() {
        let lx = Lexer::new(String::from("let num @int = sum(3,add(3,4));"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse28() {
        let lx = Lexer::new(String::from("let num @int = sum(3,add(3,4);"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_err())
    }

    #[test]
    fn should_parse29() {
        let lx = Lexer::new(String::from("let num @int = sum() + add(3,5);"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse30() {
        let lx = Lexer::new(String::from("let num @int = sum(x,y) + add(3,5);"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse31() {
        let lx = Lexer::new(String::from("return x + y;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse32() {
        let lx = Lexer::new(String::from("return x;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse33() {
        let lx = Lexer::new(String::from("return x"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_err())
    }

    #[test]
    fn should_parse34() {
        let lx = Lexer::new(String::from("return x + call();"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse35() {
        let lx = Lexer::new(String::from("return sum(x,y) + add(3,5);"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse36() {
        let lx = Lexer::new(String::from(
            "
        let add @int = fn(x @int, y @int){
            return x + y;
        };
        ",
        ));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse37() {
        let lx = Lexer::new(String::from(
            "
        let echo @string = fn(name @string){
            return name;
        };
        ",
        ));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        assert!(res.is_ok())
    }
}
