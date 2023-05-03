use std::cell::RefCell;
use std::rc::Rc;

use errors::errors;
use lexer::lexer::Lexer;
use lexer::tokens::{IdentifierKind, Token};

use crate::objects::*;
use crate::registry::TokenRegistry;

#[derive(Debug, Clone, Default)]
pub struct Parser {
    pub lexer: Lexer,

    /// holds all the tokens extracted by the lexer
    /// pub bucket: Vec<Token>,
    pub bucket: Rc<RefCell<Vec<Token>>>,

    /// the program tree that will be built when parsing
    pub program: Program,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Self {
            lexer,
            bucket: Rc::new(RefCell::new(Vec::new())),
            program: Program::default(),
        }
    }

    pub fn program_as_non_root(&mut self) -> Self {
        self.program.toggle_root();
        self.to_owned()
    }

    /// parse internally builds a set of tokens from the lexer. This should be treated as the main entry point to the parser
    pub fn parse(&mut self, json_name: Option<&str>) -> Result<Objects, errors::KarisError> {
        // add all tokens to a bucket cache. This will be used later when doing the actual parsing
        loop {
            let token = self.lexer.generate()?;
            let bucket_clone = self.bucket.clone();
            let mut token_bucket = bucket_clone.borrow_mut();
            if token.token_type == IdentifierKind::EOF {
                break;
            } else {
                token_bucket.push(token.clone());
            }
        }

        self.parse_program(json_name)
    }

    /// A derivative of `parse` function. This function expects to be given a list of tokens which will act as the parser input.
    pub fn parse_from_vec(
        &mut self,
        vec_tokens: Vec<Token>,
    ) -> Result<Objects, errors::KarisError> {
        for token in vec_tokens.iter() {
            let bucket_clone = self.bucket.clone();
            let mut token_bucket = bucket_clone.borrow_mut();
            token_bucket.push(token.clone());
        }

        self.parse_program(None)
    }

    fn parse_program(&mut self, json_name: Option<&str>) -> Result<Objects, errors::KarisError> {
        self.build_program_expressions(0x0)?;
        let program = Objects::TyProgram(self.program.clone());
        program.write_as_json_to_file(json_name).unwrap();
        Ok(program)
    }

    fn build_program_expressions(&mut self, index: usize) -> Result<(), errors::KarisError> {
        if index >= self.bucket.borrow().len() {
            return Ok(());
        }

        match Self::expression(0, index, self.bucket.clone()) {
            Ok((o, i)) => {
                if !o.is_ty_unknown() && !o.is_ty_consumable() {
                    self.program.add_object(o);
                };
                self.build_program_expressions(i + 0x01)
            }
            Err(e) => Err(e),
        }
    }
}

impl Parser {
    /// An associated function of the parser
    /// returns the `Object` representation and the current index that has been worked on
    pub fn expression(
        rbp: usize,
        index: usize,
        bucket: Rc<RefCell<Vec<Token>>>,
    ) -> Result<(Objects, usize), errors::KarisError> {
        // this is the current token been parsed
        let token = &bucket.borrow()[index];

        if token.token_type == IdentifierKind::EOS {
            return Ok((Objects::TyConsumable, index));
        }

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

        let pt0 = parser_type_fn(token.token_type)?;
        let (mut left, mut worked_on_index) = match pt0.nud_fn {
            Some(func) => {
                let res = func(token.clone(), index, bucket.clone())?;
                (res.0, res.1)
            }
            None => (Objects::TyUnknown, 0x00),
        };

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
                        let pt0 = parser_type_fn(token.token_type)?;
                        return match pt0.nud_fn {
                            Some(func) => {
                                let res = func(token.clone(), index, bucket.clone())?;
                                Ok((res.0, res.1))
                            }
                            None => Ok((Objects::TyUnknown, 0x00)),
                        };
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
mod parser_tests {

    use crate::retriever::{
        interger_value_from_nested_node, node_child, node_from_object, right_side_of_either,
    };

    use super::*;

    #[test]
    fn should_parse1() {
        let lx = Lexer::new(String::from("let num @int = 1;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse1.json"));
        assert!(res.is_ok());
    }

    #[test]
    fn should_parse2() {
        let lx = Lexer::new(String::from("let name @string = \"alice\";"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse2.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse3() {
        let lx = Lexer::new(String::from("let num @bool = true;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse3.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse4() {
        let lx = Lexer::new(String::from("let num @bool = false;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse4.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse4b() {
        let lx = Lexer::new(String::from(
            "
        let nums [ @int ] = [ 1, 2, 3, , 5 ];

        print(nums);
        ",
        ));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse4b.json"));
        println!("{:?}", res);
        assert!(res.is_ok())
    }

    #[test]
    fn should_not_parse1() {
        let lx = Lexer::new(String::from("let num @int = "));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_not_parse1.json"));
        assert!(res.is_err())
    }

    #[test]
    fn should_not_parse2() {
        let lx = Lexer::new(String::from("let num @int = ;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_not_parse2.json"));
        assert!(res.is_err())
    }

    #[test]
    fn should_not_parse3() {
        let lx = Lexer::new(String::from("let num @int = let;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_not_parse3.json"));
        assert!(res.is_err())
    }

    #[test]
    fn should_not_parse4() {
        let lx = Lexer::new(String::from("let num @int = >"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_not_parse4.json"));
        assert!(res.is_err())
    }

    #[test]
    fn should_not_parse5() {
        let lx = Lexer::new(String::from("let num @int = =="));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_not_parse5.json"));
        assert!(res.is_err())
    }

    #[test]
    fn should_parse5() {
        let lx = Lexer::new(String::from("let num @int = 1 + 2;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse5.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse5b() {
        let lx = Lexer::new(String::from("let num @int = 4 % 2;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse5b.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse6() {
        let lx = Lexer::new(String::from("let num @int = 1 + -2;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse6.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse7() {
        let lx = Lexer::new(String::from("let num @int = 1 + +2;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse7.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse8() {
        let lx = Lexer::new(String::from("let num @int = 1 - +2;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse8.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse9() {
        let lx = Lexer::new(String::from("let num @int = 10 * 2;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse9.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse10() {
        let lx = Lexer::new(String::from("let num @int = 10 * -2;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse10.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse11() {
        let lx = Lexer::new(String::from("let num @int = 10 / 2;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse11.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse12() {
        let lx = Lexer::new(String::from("let num @int = 10 / 2 * 3;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse12.json"));
        assert!(res.is_ok());

        let res = res.unwrap();
        let program = res.as_ty_program().unwrap();
        assert_eq!(program.body.len(), 1);

        let child_object = program.body[0].clone();
        let assign_node = node_from_object(child_object.clone());

        assert_eq!(assign_node.identifier_kind, Some(IdentifierKind::ASSIGN));

        let assign_node_left_child = node_child(&assign_node, true);

        let assign_node_let_node = right_side_of_either(assign_node_left_child);

        assert_eq!(
            assign_node_let_node.identifier_kind,
            Some(IdentifierKind::LET)
        );

        let assign_node_right_child = node_child(&assign_node, false);
        let assign_node_asterisk_node = right_side_of_either(assign_node_right_child);

        assert_eq!(
            assign_node_asterisk_node.identifier_kind,
            Some(IdentifierKind::ASTERISK)
        );

        let asterisk_node = assign_node_asterisk_node;
        let asterisk_left_child = node_child(&asterisk_node, true);
        let asterisk_node_slash_node = right_side_of_either(asterisk_left_child);

        assert_eq!(
            asterisk_node_slash_node.identifier_kind,
            Some(IdentifierKind::SLASH)
        );

        let slash_node = asterisk_node_slash_node;

        let slash_node_left_literal = interger_value_from_nested_node(slash_node, true);
        assert_eq!(slash_node_left_literal.value, Some(10));

        let slash_node_right_literal = interger_value_from_nested_node(slash_node, false);
        assert_eq!(slash_node_right_literal.value, Some(2));
    }

    #[test]
    fn should_parse13() {
        let lx = Lexer::new(String::from("let num @int = 10 / 2 * 3 + 20 - 3;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse13.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse14() {
        let lx = Lexer::new(String::from("let num @int = (10 / (2 * 3)) + 20;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse14.json"));
        assert!(res.is_ok());

        let res = res.unwrap();

        let program = res.as_ty_program().unwrap();
        assert_eq!(program.body.len(), 1);

        let child_object = program.body[0].clone();

        let assign_node = node_from_object(child_object.clone());

        assert_eq!(assign_node.identifier_kind, Some(IdentifierKind::ASSIGN));

        let assign_node_left_child = node_child(&assign_node, true);

        let assign_node_let_node = right_side_of_either(assign_node_left_child);

        assert_eq!(
            assign_node_let_node.identifier_kind,
            Some(IdentifierKind::LET)
        );

        let assign_node_right_child = node_child(&assign_node, false);
        let assign_node_plus_node = right_side_of_either(assign_node_right_child);

        assert_eq!(
            assign_node_plus_node.identifier_kind,
            Some(IdentifierKind::PLUS)
        );

        let plus_node = assign_node_plus_node;
        let plus_left_child = node_child(&plus_node, true);
        let grouping_node = right_side_of_either(plus_left_child);

        assert_eq!(
            grouping_node.identifier_kind,
            Some(IdentifierKind::GROUPING)
        );

        let group_items = node_child(&grouping_node, false);
        let grouping_item_node = right_side_of_either(group_items);

        assert_eq!(
            grouping_item_node.identifier_kind,
            Some(IdentifierKind::SLASH)
        );
    }

    #[test]
    fn should_parse14b() {
        let lx = Lexer::new(String::from("let num @int = (10 / 2);"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse14b.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse15() {
        let lx = Lexer::new(String::from("let num @int = 10 / (2 * 3) + 20 - 3;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse15.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse16() {
        let lx = Lexer::new(String::from("let num @int = (10 / (2 * 3)) + 20;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse16.json"));
        println!("{:?}", res);
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse16b() {
        let lx = Lexer::new(String::from(
            "let num @int = ( (10 - 5) / (2 * 3 + 50 / 4) ) + 20 - 3;",
        ));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse16b.json"));
        println!("{:?}", res);
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse16c() {
        let lx = Lexer::new(String::from("let num @int = 10(23);"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse16c.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse16d() {
        let lx = Lexer::new(String::from("let num @int = 10(23 * (20 * 10 + 1));"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse16d.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse16e() {
        let lx = Lexer::new(String::from("let num @int = 10(23 * (true,false));"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse16e.json"));
        println!("err : {res:?}");
        assert!(res.is_err())
    }

    #[test]
    fn should_parse16f() {
        let lx = Lexer::new(String::from("10(23;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse16f.json"));
        assert!(res.is_err())
    }

    #[test]
    fn should_parse16g() {
        let lx = Lexer::new(String::from("10(23 * (20 * 10 + 1))"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse16g.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse17() {
        let lx = Lexer::new(String::from("let num @int = sum() +  20 - 3;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse17.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse17b() {
        let lx = Lexer::new(String::from("let num @int = 20 + sum() +  10 - 3;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse17b.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse17c() {
        let lx = Lexer::new(String::from("let num @int = 4 % sum() +  10 - 3;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse17c.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse17d() {
        let lx = Lexer::new(String::from("sum();"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse17d.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse18() {
        let lx = Lexer::new(String::from("let num @int = sum(1,2) +  20 - 3;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse18.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse19() {
        let lx = Lexer::new(String::from(
            "let num @int = sum(\"name\",\"age\") +  20 - 3;",
        ));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse19.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse20() {
        let lx = Lexer::new(String::from("let num @int = sum(\"name\",2) +  20 - 3;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse20.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse21() {
        let lx = Lexer::new(String::from("let num @int = sum(true) +  20 - 3;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse21.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse22() {
        let lx = Lexer::new(String::from("let num @int = sum(true,false) +  20 - 3;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse22.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse23() {
        let lx = Lexer::new(String::from(
            "let num @int = sum(\"name\",false,5) +  20 - 3;",
        ));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse23.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse24() {
        let lx = Lexer::new(String::from("let num @int = sum(3,4,5);"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse24.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse25() {
        let lx = Lexer::new(String::from("let num @int = sum(3,4,5) + 20;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse25.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse26() {
        let lx = Lexer::new(String::from("let num @int = sum(3,4,5 + 20;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse26.json"));
        assert!(res.is_err())
    }

    #[test]
    fn should_parse27() {
        let lx = Lexer::new(String::from("let num @int = sum(3,add(3,4));"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse27.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse28() {
        let lx = Lexer::new(String::from("let num @int = sum(3,add(3,4);"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse28.json"));
        assert!(res.is_err())
    }

    #[test]
    fn should_parse29() {
        let lx = Lexer::new(String::from("let num @int = sum() + add(3,5);"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse29.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse30() {
        let lx = Lexer::new(String::from("let num @int = sum(x,y) + add(3,5);"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse30.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse31() {
        let lx = Lexer::new(String::from("return x + y;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse31.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse32() {
        let lx = Lexer::new(String::from("return x;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse32.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse32b() {
        let lx = Lexer::new(String::from("return 10 + call() * 3;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse32b.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse33() {
        let lx = Lexer::new(String::from("return x"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse33.json"));
        assert!(res.is_err())
    }

    #[test]
    fn should_parse34() {
        let lx = Lexer::new(String::from("return  x + call();"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse34.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse35() {
        let lx = Lexer::new(String::from("return sum(x,y) + add(3,5);"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse35.json"));
        println!("{:?}", res);
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
        let res = parser.parse(Some("should_parse36.json"));
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
        let res = parser.parse(Some("should_parse37.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse38() {
        let lx = Lexer::new(String::from(
            "
        let echo @bool = fn(){
            let resp @bool = true;
            return resp;
        };
        ",
        ));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse38.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse38b() {
        let lx = Lexer::new(String::from(
            "
        let num @int = fn(x @int){
            let y @int = 10;
            return x + y;
        };
        ",
        ));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse38b.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse39() {
        let lx = Lexer::new(String::from(
            "
        let echo @bool = fn(){
            return false;
        };
        ",
        ));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse39.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse40() {
        let lx = Lexer::new(String::from(
            "
        let echo @unit = fn(){
            print(1);
        };
        ",
        ));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse40.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse41() {
        let lx = Lexer::new(String::from(
            "
            let greeter @unit = fn(name @string) {
                print(\"Hi you\");
                let msg @string = format(\"Hi you #name\");
            };


        ",
        ));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse41.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse42() {
        let lx = Lexer::new(String::from(
            "
            let max @int = fn(x @int, y @int){
                if x > y{
                    return x;
                };

                return y;
            };
        ",
        ));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse42.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse42b() {
        let lx = Lexer::new(String::from(
            "
            if x > y{
                return x;
            };
        ",
        ));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse42b.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse43() {
        let lx = Lexer::new(String::from(
            "
            let minmax_or_product @int = fn(x @int, y @int){
                if x < y{
                   return x + y;
                }else x > y{
                    return x - y;
                };

                return x * y;
            };
        ",
        ));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse43.json"));
        assert!(res.is_ok());
    }

    #[test]
    fn should_parse44() {
        let lx = Lexer::new(String::from(
            "

        let fibonacci @int = fn(n @int){
            if n == 0 {
                return 0;
            };

            if n == 1 || n == 2 {
                return 1;
            };

            return fibonacci(n - 1) + fibonacci(n - 2);
        };

        ",
        ));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse44.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse45() {
        let lx = Lexer::new(String::from(
            "

        let fibonacci @int = fn(n @int){
            if n == 0 {
                return 0;
            };

            if n == 1 && n == 2 {
                return 1;
            };

            return fibonacci(n - 1) + fibonacci(n - 2);
        };

        ",
        ));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse45.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse46() {
        let lx = Lexer::new(String::from(
            "

        let add @int = fn(x @int, y @int){
            return x + y;
        };

        @main fn(){
            let x @int = 5;
            let y @int = 7;
            let name @string = \"Karis\";
            let result0 @int = add(x,y);

            print(result0);
            print(add(x,y));
        }@end;
        ",
        ));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse46.json"));
        assert!(res.is_ok())
    }

    #[test]
    fn should_parse47() -> std::io::Result<()> {
        let lx = Lexer::new(String::from(
            "
            let add @int = fn(x @int, y @int){
                return x + y;
            };

            let y @int = 10;
            let resp @int = add(y,1,20,y);
        ",
        ));
        let mut parser = Parser::new(lx);
        let _res = parser.parse(Some("should_parse47.json"))?;
        Ok(())
    }

    #[test]
    fn should_parse48() {
        let lx = Lexer::new(String::from("true && true;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse48.json"));
        assert!(res.is_ok());
    }

    #[test]
    fn should_parse49() {
        let lx = Lexer::new(String::from("true && false;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse49.json"));
        assert!(res.is_ok());
    }

    #[test]
    fn should_parse50() {
        let lx = Lexer::new(String::from("true & false;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse50.json"));
        assert!(res.is_ok());
    }

    #[test]
    fn should_parse51() {
        let lx = Lexer::new(String::from("true || false;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse51.json"));
        assert!(res.is_ok());
    }

    #[test]
    fn should_parse52() {
        let lx = Lexer::new(String::from("true | false;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse52.json"));
        assert!(res.is_ok());
    }

    #[test]
    fn should_parse53() {
        let lx = Lexer::new(String::from("!true;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse53.json"));
        assert!(res.is_ok());
    }

    #[test]
    fn should_parse54() {
        let lx = Lexer::new(String::from("!false"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse54.json"));
        assert!(res.is_ok());
    }

    #[test]
    fn should_parse55() {
        let lx = Lexer::new(String::from("!!false"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse55.json"));
        assert!(res.is_ok());
    }

    #[test]
    fn should_parse56() {
        let lx = Lexer::new(String::from("!!!!false"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse56.json"));
        assert!(res.is_ok());
    }

    #[test]
    fn should_parse57() {
        let lx = Lexer::new(String::from("10"));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse57.json"));
        assert!(res.is_ok());
    }

    #[test]
    fn should_parse58() {
        let lx = Lexer::new(String::from(
            "

            let multi_conditions @int = fn(x @int, y @int){
                if x < y{
                   return x + y;
                }else x == y {
                    return x * y;
                }else x > 5 {
                    return x + y + 5;
                } else {
                    return x - y;
                };

            };

        ",
        ));

        let mut parser = Parser::new(lx);

        let res = parser.parse(Some("should_parse58.json"));
        assert!(res.is_ok());

        let res = res.unwrap();
        let program = res.as_ty_program().unwrap();

        assert_eq!(program.non_root, false);
        assert_eq!(program.body.len(), 1);
    }

    #[test]
    fn should_parse59() {
        let lx = Lexer::new(String::from(
            "
        let add @int = fn(x @int, y @int){
            return x + y;
        };

        let result @int = add(10,20);

        ",
        ));
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_parse59.json"));
        assert!(res.is_ok());
    }
}
