use std::cell::RefCell;
use std::rc::Rc;

use either::Either::{Left, Right};
use hashbrown::HashMap;

use errors::errors;
use lexer::tokens::{IdentifierKind, Token};

use crate::{
    objects::{
        BooleanValue, IntergerValue, LiteralObjects, Node, Objects, ParserType, StringValue,
        TypingKind,
    },
    parser::Parser,
};

#[derive(Debug, Default, Clone)]
pub struct TokenRegistry {
    pub registry: HashMap<IdentifierKind, ParserType>,
}

impl TokenRegistry {
    pub fn new() -> TokenRegistry {
        let mut tg = TokenRegistry::default();
        tg.get_token_registry();
        tg
    }

    pub fn retrieve_from_registry(&self, symbol: IdentifierKind) -> Option<&ParserType> {
        self.registry.get(&symbol)
    }

    fn get_token_registry(&mut self) {
        self.consumable(IdentifierKind::SEMICOLON);
        self.consumable(IdentifierKind::EOS);
        self.consumable(IdentifierKind::EOF);
        self.consumable(IdentifierKind::RBRACE);

        self.add_let_binding();
        self.add_int_literal();
        self.add_string_literal();
        self.add_boolean_true_literal();
        self.add_boolean_false_literal();
        self.add_if_statement();
        self.add_else_statement();
        self.add_left_brace_statement();
        self.add_minus_or_plus_as_prefix(IdentifierKind::MINUS);
        self.add_minus_or_plus_as_prefix(IdentifierKind::PLUS);

        self.add_assign();

        self.add_infix(IdentifierKind::PLUS, 30);
        self.add_infix(IdentifierKind::MINUS, 30);
        self.add_infix(IdentifierKind::ASTERISK, 40);
        self.add_infix(IdentifierKind::SLASH, 40);

        self.add_bracket_opening(IdentifierKind::LPAREN, 50);
        self.add_bracket_closing(IdentifierKind::RPAREN, 55);

        self.add_infix(IdentifierKind::GT, 60);
        self.add_infix(IdentifierKind::GTOREQ, 60);
        self.add_infix(IdentifierKind::LT, 60);
        self.add_infix(IdentifierKind::LTOREQ, 60);
        self.add_infix(IdentifierKind::EQ, 60);

        self.add_variable_declaration();
        self.add_function_declaration();
        self.add_builtin_function(IdentifierKind::PRINT, 70);
    }

    fn typing_kind(tok: &Token) -> TypingKind {
        match tok.token_type {
            IdentifierKind::INTTYPE => TypingKind::Int,
            IdentifierKind::BOOLEANTYPE => TypingKind::Boolean,
            IdentifierKind::STRINGTYPE => TypingKind::String,
            _ => TypingKind::Unknown,
        }
    }

    fn consumable(&mut self, symbol: IdentifierKind) {
        let obj = ParserType {
            nud_fn: Some(Self::parse_consumable),
            led_fn: None,
            binding_power: Some(0x00),
        };
        self.register(symbol, obj);
    }

    fn add_let_binding(&mut self) {
        let obj = ParserType {
            nud_fn: Some(Self::parse_let_expressions),
            led_fn: None,
            binding_power: Some(10),
        };
        self.register(IdentifierKind::LET, obj);
    }

    fn add_int_literal(&mut self) {
        let obj = ParserType {
            nud_fn: Some(Self::parse_int_literal),
            led_fn: None,
            binding_power: Some(0x00),
        };
        self.register(IdentifierKind::INTLITERAL, obj);
    }

    fn add_string_literal(&mut self) {
        let obj = ParserType {
            nud_fn: None,
            led_fn: None,
            binding_power: Some(0x00),
        };
        self.register(IdentifierKind::STRINGLITERAL, obj);
    }

    fn add_boolean_true_literal(&mut self) {
        let obj = ParserType {
            nud_fn: None,
            led_fn: None,
            binding_power: Some(0x00),
        };
        self.register(IdentifierKind::TRUE, obj);
    }

    fn add_boolean_false_literal(&mut self) {
        let obj = ParserType {
            nud_fn: None,
            led_fn: None,
            binding_power: Some(0x00),
        };
        self.register(IdentifierKind::FALSE, obj);
    }

    fn add_if_statement(&mut self) {
        let obj = ParserType {
            nud_fn: None,
            led_fn: None,
            binding_power: Some(10),
        };
        self.register(IdentifierKind::IF, obj);
    }

    fn add_else_statement(&mut self) {
        let obj = ParserType {
            nud_fn: None,
            led_fn: None,
            binding_power: Some(10),
        };
        self.register(IdentifierKind::ELSE, obj);
    }

    fn add_minus_or_plus_as_prefix(&mut self, symbol: IdentifierKind) {
        let obj = ParserType {
            nud_fn: Some(Self::parse_minus_or_plus_as_prefix),
            led_fn: None,
            binding_power: Some(10),
        };
        self.register(symbol, obj);
    }

    fn add_left_brace_statement(&mut self) {
        let obj = ParserType {
            nud_fn: None,
            led_fn: None,
            binding_power: Some(10),
        };
        self.register(IdentifierKind::LBRACE, obj);
    }

    fn add_assign(&mut self) {
        let obj = ParserType {
            nud_fn: None,
            led_fn: Some(Self::parse_assign_operator),
            binding_power: Some(30),
        };
        self.register(IdentifierKind::ASSIGN, obj);
    }

    fn add_infix(&mut self, symbol: IdentifierKind, binding_power: usize) {
        let obj = ParserType {
            nud_fn: None,
            led_fn: Some(Self::parse_infix_operator),
            binding_power: Some(binding_power),
        };
        self.register(symbol, obj);
    }

    fn add_function_declaration(&mut self) {
        let obj = ParserType {
            nud_fn: None,
            led_fn: None,
            binding_power: Some(70),
        };
        self.register(IdentifierKind::FUNCTION, obj);
    }

    // parses function call expressions
    fn add_variable_declaration(&mut self) {
        let obj = ParserType {
            nud_fn: Some(Self::parse_function_call),
            led_fn: None,
            binding_power: Some(70),
        };
        self.register(IdentifierKind::VARIABLE, obj);
    }

    fn add_builtin_function(&mut self, symbol: IdentifierKind, binding_power: usize) {
        let obj = ParserType {
            nud_fn: None,
            led_fn: None,
            binding_power: Some(binding_power),
        };
        self.register(symbol, obj);
    }

    fn add_bracket_opening(&mut self, symbol: IdentifierKind, binding_power: usize) {
        let obj = ParserType {
            nud_fn: Some(Self::parse_opening_bracket),
            led_fn: None,
            binding_power: Some(binding_power),
        };
        self.register(symbol, obj);
    }

    fn add_bracket_closing(&mut self, symbol: IdentifierKind, binding_power: usize) {
        let obj = ParserType {
            nud_fn: None,
            led_fn: Some(Self::parse_closing_bracket),
            binding_power: Some(binding_power),
        };
        self.register(symbol, obj);
    }

    fn register(&mut self, symbol: IdentifierKind, obj: ParserType) {
        if let Some((_key, val)) = self.registry.get_key_value_mut(&symbol) {
            if let Some(f) = obj.nud_fn {
                val.nud_fn = Some(f);
            }

            if let Some(f) = obj.led_fn {
                val.led_fn = Some(f);
            }

            if let Some(f) = obj.binding_power {
                val.binding_power = Some(f);
            }
        } else {
            self.registry.insert(symbol, obj);
        }
    }
}

// parser implementations methods
impl TokenRegistry {
    // Returns an Object of `Unknown` type and the index
    // Used fot tokens that have not effect or value to the generate AST
    fn parse_consumable(
        _tok: Token,
        index: usize,
        _bucket: Rc<RefCell<Vec<Token>>>,
    ) -> Result<(Objects, usize), errors::KarisError> {
        Ok((Objects::TyUnknown, index))
    }

    fn parse_function_call(
        tok: Token,
        index: usize,
        bucket: Rc<RefCell<Vec<Token>>>,
    ) -> Result<(Objects, usize), errors::KarisError> {
        let borrow = bucket.borrow();

        let next_token = borrow.get(index + 0x01);
        if next_token.is_none() {
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::InvalidSyntax,
                message: format!(
                    "[INVALID SYNTAX] Syntax not correct. Expected something after `{}. Ln {} Col {}  '",
                    tok.literal,tok.line_number,tok.column_number
                ),
            });
        }

        let next_token = next_token.unwrap();

        if next_token.token_type != IdentifierKind::LPAREN {
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::InvalidSyntax,
                message: format!(
                    "[INVALID SYNTAX] Syntax not correct. Expected `(` after `{}. Ln {} Col {}  '",
                    tok.literal, tok.line_number, tok.column_number
                ),
            });
        }

        let closing_paren_index_fn = || -> usize {
            let mut idx: usize = 0x00;
            for i in index + 0x01..borrow.len() - 1 {
                let t = borrow.get(i).unwrap();
                if t.token_type != IdentifierKind::RPAREN {
                    idx = i;
                    break;
                }
            }
            idx
        };

        let closing_paren_index = closing_paren_index_fn();
        let params = borrow.get(index + 0x02..closing_paren_index).unwrap();
        if params.is_empty() {
            let node = Node {
                identifier_kind: Some(IdentifierKind::CALL),
                ..Default::default()
            };
            Ok((Objects::TyNode(node), closing_paren_index))
        } else {
            todo!("group args and parse their types")
        }
    }

    // evaluates a returns object enclosed in parentheses () as it's left children
    fn parse_closing_bracket(
        left: Objects,
        token_index: usize,
        bucket: Rc<RefCell<Vec<Token>>>,
    ) -> Result<(Objects, usize), errors::KarisError> {
        let borrow = bucket.borrow();
        let current_token = borrow.get(token_index).unwrap();

        let node = Node {
            identifier_kind: Some(current_token.token_type),
            left_child: Some(Right(Box::new(left))),
            ..Default::default()
        };
        Ok((Objects::TyNode(node), token_index))
    }

    // evaluates when `(` is a the beginning of an expression
    // This is valid if the next token is either `int` or `call` expression
    fn parse_opening_bracket(
        tok: Token,
        index: usize,
        bucket: Rc<RefCell<Vec<Token>>>,
    ) -> Result<(Objects, usize), errors::KarisError> {
        let borrow = bucket.borrow();

        let next_token = borrow.get(index + 0x01);
        if next_token.is_none() {
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::InvalidSyntax,
                message: format!(
                    "[INVALID SYNTAX] Syntax not correct. Expected something after `{}. Ln {} Col {}  '",
                    tok.literal,tok.line_number,tok.column_number
                ),
            });
        }

        let next_token = next_token.unwrap();

        if next_token.token_type == IdentifierKind::INTLITERAL {
            let res = Parser::expression(0, index + 0x01, bucket.clone());
            if res.is_err() {
                let err = res.err().unwrap();
                return Err(err);
            }

            let res = res.unwrap();
            let node = Node {
                identifier_kind: Some(tok.token_type),
                right_child: Some(Right(Box::new(res.0))),
                ..Default::default()
            };

            Ok((Objects::TyNode(node), res.1))
        } else {
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::InvalidSyntax,
                message: format!(
                    "[INVALID SYNTAX] Syntax not correct. Expected something after `{}. Ln {} Col {}  '",
                    tok.literal,tok.line_number,tok.column_number
                ),
            });
        }
    }

    // Evaluates a `-` or `+` tokenn as a prefix
    // it merges with the next token which should be of type `INT`
    fn parse_minus_or_plus_as_prefix(
        tok: Token,
        index: usize,
        bucket: Rc<RefCell<Vec<Token>>>,
    ) -> Result<(Objects, usize), errors::KarisError> {
        let borrow = bucket.borrow();

        let next_token = borrow.get(index + 0x01);
        if next_token.is_none() {
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::InvalidSyntax,
                message: format!(
                    "[INVALID SYNTAX] Syntax not correct. Expected something after `{}. Ln {} Col {}  '",
                    tok.literal,tok.line_number,tok.column_number
                ),
            });
        }

        let next_token = next_token.unwrap();
        if next_token.token_type != IdentifierKind::INTLITERAL {
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::InvalidSyntax,
                message: format!(
                    "[INVALID SYNTAX] Syntax not correct after `{}`. Ln {} Col {} '",
                    tok.literal, tok.line_number, tok.column_number
                ),
            });
        }

        let as_isize = next_token.literal.parse::<isize>();
        if as_isize.is_err() {
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::UnableToConvert,
                message: format!(
                    "[FAILED CONVERSION] Failed to convert to isize ; Token {:?} Ln {} Col {}",
                    next_token.literal, next_token.line_number, next_token.column_number
                ),
            });
        }

        let value_fn = || {
            let v = as_isize.unwrap();
            if tok.token_type == IdentifierKind::PLUS {
                v
            } else {
                -v
            }
        };

        let int = IntergerValue {
            value: Some(value_fn()),
        };
        let obj = LiteralObjects::ObjIntergerValue(int);
        let node = Node {
            left_child: Some(Left(obj)),
            ..Default::default()
        };

        Ok((Objects::TyNode(node), index + 0x01))
    }

    // To parse a let expression, move the cursor to the right until an `ASSIGN (=)` token is encountered.
    // Assert that the `variable name` token is available in it's designated position otherwise return an error
    // Assert that the `typing` token is available in it's designated position otherwise return an error
    // Then collect all the tokens to the left of `=` and construct a node
    // Teturn the node and the current cursor position minus 1 ( index-0x01)
    fn parse_let_expressions(
        tok: Token,
        index: usize,
        bucket: Rc<RefCell<Vec<Token>>>,
    ) -> Result<(Objects, usize), errors::KarisError> {
        if tok.token_type != IdentifierKind::ASSIGN {
            let new_index = index + 0x1;
            let next_token = &bucket.borrow()[new_index];
            return Self::parse_let_expressions(next_token.clone(), new_index, bucket.clone());
        }

        let borrow = bucket.borrow();

        if borrow.get(0x0).is_none() {
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::MissingLetBinding,
                message: format!(
                    "[MISSING LET] Expected to find `let` ; Token {:?} Ln {} Col {}",
                    tok.literal, tok.line_number, tok.column_number
                ),
            });
        }

        let variable_name_token = borrow.get(0x01);
        if variable_name_token.is_none() {
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::MissingVariableName,
                message: format!(
                    "[MISSING VARIABLE NAME] Expected to find `<variable name>` ; Token {:?} Ln {} Col {}",
                    tok.literal, tok.line_number, tok.column_number
                ),
            });
        }

        let typing_token = borrow.get(0x02);
        if typing_token.is_none() {
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::MissingTypeInfo,
                message: format!(
                    "[MISSING TYPE INFO] Expected to find either `@int | @string | @bool | @unit` ; Token {:?} Ln {} Col {}",
                    tok.literal, tok.line_number, tok.column_number
                ),
            });
        }

        let node = Node {
            variable_name: Some(variable_name_token.unwrap().literal.clone()),
            return_type: Some(Self::typing_kind(typing_token.unwrap())),
            ..Default::default()
        };

        Ok((Objects::TyNode(node), index - 0x1))
    }

    fn parse_int_literal(
        tok: Token,
        index: usize,
        _bucket: Rc<RefCell<Vec<Token>>>,
    ) -> Result<(Objects, usize), errors::KarisError> {
        let as_isize = tok.literal.parse::<isize>();
        if as_isize.is_err() {
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::UnableToConvert,
                message: format!(
                    "[FAILED CONVERSION] Failed to convert to isize ; Token {:?} Ln {} Col {}",
                    tok.literal, tok.line_number, tok.column_number
                ),
            });
        }

        let value = as_isize.unwrap();
        let int = IntergerValue { value: Some(value) };
        let obj = LiteralObjects::ObjIntergerValue(int);
        let node = Node {
            left_child: Some(Left(obj)),
            ..Default::default()
        };
        let obj_type = Objects::TyNode(node);
        Ok((obj_type, index))
    }

    // Evaluates the RHS of an arthemetic expression
    fn parse_infix_operator(
        left: Objects,
        token_index: usize,
        bucket: Rc<RefCell<Vec<Token>>>,
    ) -> Result<(Objects, usize), errors::KarisError> {
        let borrow = bucket.borrow();

        let current_token = borrow.get(token_index);
        if current_token.is_none() {
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::InvalidSyntax,
                message: "[INVALID SYNTAX] Syntax not correct. Expected position to have a token "
                    .to_string(),
            });
        }

        let current_token = current_token.unwrap();

        let next_token = borrow.get(token_index + 0x01);
        if next_token.is_none() {
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::InvalidSyntax,
                message: format!(
                    "[INVALID SYNTAX] Syntax not correct. Expected something after `{}. Ln {} Col {}  '",
                    current_token.literal,current_token.line_number, current_token.column_number
                ),
            });
        }

        let next_token = next_token.unwrap();

        if next_token.token_type == IdentifierKind::EOS
            || next_token.token_type == IdentifierKind::EOF
            || next_token.token_type == IdentifierKind::LET
            || next_token.token_type == IdentifierKind::FUNCTION
            || next_token.token_type == IdentifierKind::TRUE
            || next_token.token_type == IdentifierKind::FALSE
            || next_token.token_type == IdentifierKind::STRINGLITERAL
        {
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::InvalidSyntax,
                message: format!(
                    "[INVALID SYNTAX] Syntax not correct after `{}`. Ln {} Col {}  '",
                    current_token.literal, current_token.line_number, current_token.column_number
                ),
            });
        }

        let res = Parser::expression(0x00, token_index + 0x01, bucket.clone());
        if res.is_err() {
            let err = res.err().unwrap();
            return Err(err);
        }

        let res = res.unwrap();
        let node = Node {
            identifier_kind: Some(current_token.token_type),
            left_child: Some(Right(Box::new(left))),
            right_child: Some(Right(Box::new(res.0))),
            ..Default::default()
        };

        Ok((Objects::TyNode(node), res.1))
    }

    // Evaluates the RHS of an expression
    // Checks if the RHS is
    // - a literal => string, bool, int, array
    // - arthemetic expression
    // - function definition expression
    // - function call expression
    fn parse_assign_operator(
        left: Objects,
        token_index: usize,
        bucket: Rc<RefCell<Vec<Token>>>,
    ) -> Result<(Objects, usize), errors::KarisError> {
        let borrow = bucket.borrow();

        let current_token = borrow.get(token_index);
        if current_token.is_none() {
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::InvalidSyntax,
                message: "[INVALID SYNTAX] Syntax not correct. Expected position to have a token "
                    .to_string(),
            });
        }

        let current_token = current_token.unwrap();

        let next_token = borrow.get(token_index + 0x01);
        if next_token.is_none() {
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::InvalidSyntax,
                message: "[INVALID SYNTAX] Syntax not correct. Expected something after `=`'"
                    .to_string(),
            });
        }

        let next_token = next_token.unwrap();

        if next_token.token_type == IdentifierKind::EOS
            || next_token.token_type == IdentifierKind::EOF
            || next_token.token_type == IdentifierKind::SEMICOLON
            || next_token.token_type == IdentifierKind::LET
            || next_token.token_type == IdentifierKind::GT
            || next_token.token_type == IdentifierKind::LT
            || next_token.token_type == IdentifierKind::GTOREQ
            || next_token.token_type == IdentifierKind::LTOREQ
            || next_token.token_type == IdentifierKind::EQ
        {
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::InvalidSyntax,
                message: "[INVALID SYNTAX] Syntax not correct. Expected something after `=`'"
                    .to_string(),
            });
        }

        // check if the next token, 2 steps forward is a semicolon. If true, it means we will return a literal
        let two_step_token = borrow.get(token_index + 0x02);
        if let Some(two_step_token) = two_step_token {
            if two_step_token.token_type == IdentifierKind::SEMICOLON {
                if next_token.token_type == IdentifierKind::INTLITERAL {
                    let value = next_token
                        .literal
                        .parse::<isize>()
                        .unwrap_or_else(|_| panic!("Failed to parse to INT"));
                    let obj = IntergerValue { value: Some(value) };
                    let literal = LiteralObjects::ObjIntergerValue(obj);

                    let node = Node {
                        identifier_kind: Some(current_token.token_type),
                        left_child: Some(Right(Box::new(left))),
                        right_child: Some(Left(literal)),
                        ..Default::default()
                    };

                    // move the cursor to the end. This will finish the recursive call stask and return to the caller
                    return Ok((Objects::TyNode(node), token_index + 0x02));
                }

                if next_token.token_type == IdentifierKind::STRINGLITERAL {
                    let obj = StringValue {
                        value: Some(next_token.literal.clone()),
                    };
                    let literal = LiteralObjects::ObjStringValue(obj);

                    let node = Node {
                        identifier_kind: Some(current_token.token_type),
                        left_child: Some(Right(Box::new(left))),
                        right_child: Some(Left(literal)),
                        ..Default::default()
                    };

                    // move the cursor to the end. This will finish the recursive call stask and return to the caller
                    return Ok((Objects::TyNode(node), token_index + 0x02));
                }

                if next_token.token_type == IdentifierKind::TRUE {
                    let value = next_token
                        .literal
                        .parse::<bool>()
                        .unwrap_or_else(|_| panic!("Failed to parse to BOOL"));
                    let obj = BooleanValue { value: Some(value) };
                    let literal = LiteralObjects::ObjBooleanValue(obj);

                    let node = Node {
                        identifier_kind: Some(current_token.token_type),
                        left_child: Some(Right(Box::new(left))),
                        right_child: Some(Left(literal)),
                        ..Default::default()
                    };

                    // move the cursor to the end. This will finish the recursive call stask and return to the caller
                    return Ok((Objects::TyNode(node), token_index + 0x02));
                }

                if next_token.token_type == IdentifierKind::FALSE {
                    let value = next_token
                        .literal
                        .parse::<bool>()
                        .unwrap_or_else(|_| panic!("Failed to parse to BOOL"));
                    let obj = BooleanValue { value: Some(value) };
                    let literal = LiteralObjects::ObjBooleanValue(obj);

                    let node = Node {
                        identifier_kind: Some(current_token.token_type),
                        left_child: Some(Right(Box::new(left))),
                        right_child: Some(Left(literal)),
                        ..Default::default()
                    };

                    // move the cursor to the end. This will finish the recursive call stask and return to the caller
                    return Ok((Objects::TyNode(node), token_index + 0x02));
                }
            }
        }

        let res = Parser::expression(0x00, token_index + 0x01, bucket.clone());
        if res.is_err() {
            let err = res.err().unwrap();
            return Err(err);
        }

        let res = res.unwrap();
        let node = Node {
            identifier_kind: Some(current_token.token_type),
            left_child: Some(Right(Box::new(left))),
            right_child: Some(Right(Box::new(res.0))),
            ..Default::default()
        };

        Ok((Objects::TyNode(node), res.1))
    }
}
