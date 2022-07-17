use std::cell::RefCell;
use std::rc::Rc;

use either::Either::{Left, Right};

use errors::errors;
use lexer::tokens::{IdentifierKind, Token};

use crate::registry::TokenRegistry;
use crate::{
    objects::{BooleanValue, IntergerValue, LiteralObjects, Node, Objects},
    parser::Parser,
};

// parser implementations methods for NUD
// consumes to the right with no left-context
impl TokenRegistry {
    // Returns an Object of `Unknown` type and the index
    // Used fot tokens that have not effect or value to the generate AST
    pub(crate) fn parse_consumable(
        _tok: Token,
        index: usize,
        _bucket: Rc<RefCell<Vec<Token>>>,
    ) -> Result<(Objects, usize), errors::KarisError> {
        Ok((Objects::TyUnknown, index))
    }

    pub(crate) fn parse_variable(
        tok: Token,
        index: usize,
        _bucket: Rc<RefCell<Vec<Token>>>,
    ) -> Result<(Objects, usize), errors::KarisError> {
        let node = Node {
            variable_name: Some(tok.literal),
            identifier_kind: Some(IdentifierKind::VARIABLE),
            ..Default::default()
        };

        Ok((Objects::TyNode(node), index))
    }

    pub(crate) fn parse_return(
        tok: Token,
        index: usize,
        bucket: Rc<RefCell<Vec<Token>>>,
    ) -> Result<(Objects, usize), errors::KarisError> {
        let (obj, index) = Parser::expression(0, index + 0x01, bucket.clone())?;

        let borrow = bucket.borrow();

        let next_index = index + 0x01;
        let next_token = borrow.get(next_index);
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

        if next_token.token_type != IdentifierKind::SEMICOLON {
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::InvalidSyntax,
                message: format!(
                    "[INVALID SYNTAX] Syntax not correct. Expected tail `;` `{}. Ln {} Col {}  '",
                    tok.literal, tok.line_number, tok.column_number
                ),
            });
        }

        let node = Node {
            identifier_kind: Some(IdentifierKind::RETURN),
            right_child: Some(Right(Box::new(obj))),
            ..Default::default()
        };

        // move the index to the adjacent `rbrace` token
        Ok((Objects::TyNode(node), index + 0x01))
    }

    // When function definition is encountered with the token `fn`, `arse_function_definition`
    // parses the entire function block inclusive of args until tails `}` is encountered
    // For the body, it recursively call `Self::expression`, progressively building children nodes
    //
    // Example:
    // let div @int = fn(x @int, y @int){
    //     return x * y;
    // };

    // let echo @string = fn(name @string){
    //     return name;
    // };

    // let printer @unit = fn(name @string){
    //     print("Name #name");
    // };

    // let max @int = fn(x @int, y @int){
    //     if x > y{
    //         return x;
    //     };
    //     return y;
    // };

    // let factorial @int = fn(n @int){
    //     if n == 1 {
    //         return 1;
    //     };
    //     return n * factorial(n-1);
    // }

    // let fibnacci @int = fn(n @int){
    //     if n == 0 {
    //         return 0;
    //     };

    //     if n == 1 || n == 2 {
    //         return 1
    //     };

    //     return fibnacci(n-1) + fibnacci(n-2);
    // }
    pub(crate) fn parse_function_definition(
        tok: Token,
        index: usize,
        bucket: Rc<RefCell<Vec<Token>>>,
    ) -> Result<(Objects, usize), errors::KarisError> {
        let borrow = bucket.borrow();

        let next_index = index + 0x01;
        let next_token = borrow.get(next_index);
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

        let (function_args, lbrace_index) = Self::collect_function_definition_args(
            next_index + 0x01,
            bucket.clone(),
            Vec::new(),
            0x00,
        )?;

        let lbrace = borrow.get(lbrace_index).unwrap();
        if lbrace.token_type != IdentifierKind::LBRACE {
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::InvalidSyntax,
                message: format!(
                    "[INVALID SYNTAX] Syntax not correct. Expected left-brace. Found {}. Ln {} Col {}  '",
                    lbrace.literal, lbrace.line_number, lbrace.column_number
                ),
            });
        }

        let (block_children, last_index) =
            Self::collect_block_children(lbrace_index, bucket.clone(), Vec::new(), 0x00)?;

        let node = Node {
            identifier_kind: Some(IdentifierKind::FUNCTION),
            func_params: Some(function_args),
            block_children: Some(block_children),
            ..Default::default()
        };

        Ok((Objects::TyNode(node), last_index))
    }

    // Parses a block recursively
    // `collect_block_children` is the one that actually does the work
    pub(crate) fn parse_block(
        tok: Token,
        index: usize,
        bucket: Rc<RefCell<Vec<Token>>>,
    ) -> Result<(Objects, usize), errors::KarisError> {
        if tok.token_type != IdentifierKind::LBRACE {
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::InvalidSyntax,
                message: format!(
                    "[INVALID SYNTAX] Syntax not correct.`{}. Ln {} Col {}  '",
                    tok.literal, tok.line_number, tok.column_number
                ),
            });
        }

        let (block_children, last_index) =
            Self::collect_block_children(index, bucket, Vec::new(), 0x00)?;

        let node = Node {
            identifier_kind: Some(IdentifierKind::BLOCK),
            block_children: Some(block_children),
            ..Default::default()
        };

        Ok((Objects::TyNode(node), last_index))
    }

    // A function call can takes many forms
    // Example:
    //
    // add(1, 2, 3)  -> args as literal
    // add() -> without args
    // add(one(),two()) - > args as function call
    // add(x,y) -> args as variables.
    pub(crate) fn parse_function_call(
        tok: Token,
        index: usize,
        bucket: Rc<RefCell<Vec<Token>>>,
    ) -> Result<(Objects, usize), errors::KarisError> {
        let borrow = bucket.borrow();

        let next_index = index + 0x01;
        let next_token = borrow.get(next_index);
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

        let (params, closing_paren_index) =
            Self::collect_function_call_params(next_index, bucket.clone(), Vec::new(), 0x00)?;

        let node = if tok.token_type == IdentifierKind::PRINT
            || tok.token_type == IdentifierKind::FORMAT
        {
            Node {
                identifier_kind: Some(tok.token_type),
                call_params: Some(params),
                ..Default::default()
            }
        } else {
            Node {
                identifier_kind: Some(IdentifierKind::CALLER),
                call_params: Some(params),
                ..Default::default()
            }
        };

        Ok((Objects::TyNode(node), closing_paren_index))
    }

    // evaluates when `(` is a the beginning of an expression
    // This is valid if the next token is either `int` or `call` expression
    pub(crate) fn parse_opening_parenthesis(
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

        Self::traverse_forward_until(tok.clone(), index, bucket.clone(), IdentifierKind::RPAREN)?;

        let next_token = next_token.unwrap();

        if next_token.token_type == IdentifierKind::INTLITERAL
            || next_token.token_type == IdentifierKind::LPAREN
        {
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
            Err(errors::KarisError {
                error_type: errors::KarisErrorType::InvalidSyntax,
                message: format!(
                    "[INVALID SYNTAX] Syntax not correct. Expected something after `{}. Ln {} Col {}  '",
                    tok.literal,tok.line_number,tok.column_number
                ),
            })
        }
    }

    // Evaluates a `-` or `+` tokenn as a prefix
    // it merges with the next token which should be of type `INT`
    pub(crate) fn parse_minus_or_plus_as_prefix(
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
            identifier_kind: Some(IdentifierKind::INTLITERAL),
            left_child: Some(Left(obj)),
            ..Default::default()
        };

        Ok((Objects::TyNode(node), index + 0x01))
    }

    // To parse a `if` or `else` expression, we move the cursor to the right unit we encounter a `{`.
    // We collect item in between and parse them. After that we collect items between `{` and `}` and add them
    // to the node's `block_children`. If there is an `else` token, we do the same thing then append the resulting node
    // as part of Node `alternate`
    // Example syntax:
    //
    // let minmax_or_product @int = fn(x @int, y @int){
    //     if x < y{
    //        return x + y;
    //     }else x > y {
    //         result x - y;
    //     };

    //     result x * y;
    // };

    // let factorial @int = fn(n @int){
    //     if n == 1 {
    //         return 1;
    //     };

    //     return n * factorial(n-1);
    // };
    //
    pub(crate) fn parse_if_else_expressions(
        tok: Token,
        index: usize,
        bucket: Rc<RefCell<Vec<Token>>>,
    ) -> Result<(Objects, usize), errors::KarisError> {
        let borrow = bucket.borrow();
        if borrow.get(0x0).is_none() {
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::MissingConditionalIndentifier,
                message: format!(
                    "[MISSING CONDITIONAL IDENTIFIER] Expected to find `if` or `else` ; Token {:?} Ln {} Col {}",
                    tok.literal, tok.line_number, tok.column_number
                ),
            });
        }

        let mut end_index: usize;

        #[allow(clippy::redundant_clone)]
        let index_at_if_lbrace = Self::traverse_forward_until(
            tok.clone(),
            index,
            bucket.clone(),
            IdentifierKind::LBRACE,
        )?;

        let items_before_if_lbrace = borrow.get(index + 0x01..index_at_if_lbrace).unwrap();
        let exp_vec_tokens = Vec::from(items_before_if_lbrace);
        let expression_node = Parser::default().parse_from_vec(exp_vec_tokens)?;
        // if block items
        let index_at_if_rbrace = Self::traverse_forward_until(
            tok.clone(),
            index,
            bucket.clone(),
            IdentifierKind::RBRACE,
        )?;

        let _l2 = borrow.get(index_at_if_rbrace).unwrap();

        let items_after_lbrace = borrow
            .get(index_at_if_lbrace + 0x01..index_at_if_rbrace)
            .unwrap();
        let if_block_vec_tokens = Vec::from(items_after_lbrace);
        let if_block_node = Parser::default().parse_from_vec(if_block_vec_tokens)?;

        // set the end to match the index of the item before RIGHT BRACE in the if_block
        end_index = index_at_if_rbrace;

        // compute if the `if` condition has an alternate condition
        let mut alternate_node = None;
        let else_token_index = index_at_if_rbrace + 0x01;
        let else_token = borrow.get(else_token_index).unwrap();

        if else_token.token_type == IdentifierKind::ELSE {
            #[allow(clippy::redundant_clone)]
            let index_at_else_lbrace = Self::traverse_forward_until(
                else_token.clone(),
                else_token_index,
                bucket.clone(),
                IdentifierKind::LBRACE,
            )?;

            let items = borrow
                .get(else_token_index + 0x01..index_at_else_lbrace)
                .unwrap();
            let vec_tokens = Vec::from(items);
            let else_expression_node = Parser::default().parse_from_vec(vec_tokens)?;

            // else block items
            #[allow(clippy::redundant_clone)]
            let index_at_else_rbrace = Self::traverse_forward_until(
                tok.clone(),
                index_at_else_lbrace + 0x01,
                bucket.clone(),
                IdentifierKind::RBRACE,
            )?;

            let items_after_else_lbrace = borrow
                .get(index_at_else_lbrace + 0x01..index_at_else_rbrace)
                .unwrap();
            let else_block_vec_tokens = Vec::from(items_after_else_lbrace);
            let else_block_node = Parser::default().parse_from_vec(else_block_vec_tokens)?;

            let alt_node = Node {
                identifier_kind: Some(IdentifierKind::ELSE),
                right_child: Some(Right(Box::new(else_expression_node))),
                block_children: Some(Vec::from([else_block_node])),
                ..Default::default()
            };

            let obj = Objects::TyNode(alt_node);
            alternate_node = Some(Box::new(obj));

            // set the end to match the index of the item before RIGHT BRACE in the else_block
            end_index = index_at_else_rbrace;
        }

        let node = Node {
            identifier_kind: Some(IdentifierKind::IF),
            right_child: Some(Right(Box::new(expression_node))),
            alternate: alternate_node,
            block_children: Some(Vec::from([if_block_node])),
            ..Default::default()
        };

        Ok((Objects::TyNode(node), end_index))
    }

    // To parse the `main` block, move the cursor to the right unitil we encounter a `@end`.
    // We then collect all items in between and parse them
    pub(crate) fn parse_main_expressions(
        tok: Token,
        index: usize,
        bucket: Rc<RefCell<Vec<Token>>>,
    ) -> Result<(Objects, usize), errors::KarisError> {
        let borrow = bucket.borrow();
        if borrow.get(0x0).is_none() {
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::MissingEntryPoint,
                message: format!(
                    "[MISSING ENTRY POINT] Expected to find `main`; Token {:?} Ln {} Col {}",
                    tok.literal, tok.line_number, tok.column_number
                ),
            });
        }

        let index_lbrace = Self::traverse_forward_until(
            tok.clone(),
            index,
            bucket.clone(),
            IdentifierKind::LBRACE,
        )?;
        let _l0 = borrow.get(index_lbrace).unwrap();

        // index_before_end points to the the `}` token just before the `@end` token
        #[allow(clippy::redundant_clone)]
        let index_end =
            Self::traverse_forward_until(tok.clone(), index, bucket.clone(), IdentifierKind::END)?;
        let _l1 = borrow.get(index_end).unwrap();

        // parse validation.
        // END must end with semicolon
        let last_token = borrow.get(index_end + 0x01).unwrap();
        if last_token.token_type != IdentifierKind::SEMICOLON {
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::InvalidSyntax,
                message: format!(
                    "[INVALID SYNTAX] Expected to find `;`. Token {:?} Ln {} Col {}",
                    tok.literal, tok.line_number, tok.column_number
                ),
            });
        }

        let items_in_block = borrow.get(index_lbrace + 0x01..index_end - 0x02).unwrap();
        let block_tokens = Vec::from(items_in_block);
        let block_node = Parser::default().parse_from_vec(block_tokens)?;

        // points to the tail semicolon
        let end_index = index_end + 0x01;

        let node = Node {
            identifier_kind: Some(IdentifierKind::MAIN),
            block_children: Some(Vec::from([block_node])),
            ..Default::default()
        };

        Ok((Objects::TyNode(node), end_index))
    }

    // To parse a let expression, move the cursor to the right until an `ASSIGN (=)` token is encountered.
    // Assert that the `variable name` token is available in it's designated position otherwise return an error
    // Assert that the `typing` token is available in it's designated position otherwise return an error
    // Then collect all the tokens to the left of `=` and construct a node
    // Teturn the node and the current cursor position minus 1 ( index-0x01)
    pub(crate) fn parse_let_expressions(
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
            identifier_kind: Some(IdentifierKind::LET),
            variable_name: Some(variable_name_token.unwrap().literal.clone()),
            return_type: Some(Self::typing_kind(typing_token.unwrap())),
            ..Default::default()
        };

        Ok((Objects::TyNode(node), index - 0x1))
    }

    pub(crate) fn parse_int_literal(
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
            identifier_kind: Some(IdentifierKind::INTLITERAL),
            left_child: Some(Left(obj)),
            ..Default::default()
        };
        let obj_type = Objects::TyNode(node);
        Ok((obj_type, index))
    }

    // given a token, it parses it as a boolean literal
    pub(crate) fn parse_boolean_literal(
        tok: Token,
        index: usize,
        _bucket: Rc<RefCell<Vec<Token>>>,
    ) -> Result<(Objects, usize), errors::KarisError> {
        let as_bool = tok.literal.parse::<bool>();
        if as_bool.is_err() {
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::UnableToConvert,
                message: format!(
                    "[FAILED CONVERSION] Failed to convert to bool ; Token {:?} Ln {} Col {}",
                    tok.literal, tok.line_number, tok.column_number
                ),
            });
        }

        let value = as_bool.unwrap();
        let bool_val = BooleanValue { value: Some(value) };
        let obj = LiteralObjects::ObjBooleanValue(bool_val);
        let node = Node {
            identifier_kind: Some(IdentifierKind::BOOLEANLITERAL),
            left_child: Some(Left(obj)),
            ..Default::default()
        };
        let obj_type = Objects::TyNode(node);

        Ok((obj_type, index))
    }
}
