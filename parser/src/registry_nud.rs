use std::cell::RefCell;
use std::rc::Rc;

use either::Either::{Left, Right};

use errors::errors;
use lexer::tokens::{IdentifierKind, Token};

use crate::objects::StringValue;
use crate::registry::TokenRegistry;
use crate::retriever::reorganize_parenthesis_object;
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

    // parses an array starting from the beginning. It moves along the length until the closing square bracket is reached.
    // It validates that each element of the array is of the same type matching the declaration type.
    pub(crate) fn parse_opening_array(
        tok: Token,
        index: usize,
        bucket: Rc<RefCell<Vec<Token>>>,
    ) -> Result<(Objects, usize), errors::KarisError> {
        let last_item_index = Self::traverse_forward_until(
            tok.clone(),
            index,
            bucket.clone(),
            IdentifierKind::RSQUAREBRACE,
        )?;
        let rsquare_index = last_item_index;

        let borrow = bucket.borrow();

        // get the type of the array declaration by checking backwards three-steps
        let array_typing_info_index = index - 0x03;
        let array_typing_info = borrow.get(array_typing_info_index).unwrap(); // safe to unwrap here

        // get the items of the array
        let items = borrow.get(index + 0x01..rsquare_index).unwrap();
        let items = items
            .iter()
            .filter(|item| item.token_type != IdentifierKind::COMMA)
            .collect::<Vec<&Token>>();

        let mut array_items: Vec<Objects> = Vec::new();
        let tok = &tok;

        for item in items.iter() {
            let item = *item;

            if Self::literal_typing_match(item) != array_typing_info.token_type {
                return Err(errors::KarisError {
                    error_type: errors::KarisErrorType::InvalidSyntax,
                    message: format!(
                        "[INVALID SYNTAX] Syntax not correct. Expected typing of {:?}. Ln {} Col {}  '",
                        array_typing_info.token_type ,item.line_number,item.column_number
                    ),
                });
            }

            match item.token_type {
                IdentifierKind::INTLITERAL => {
                    let int = Self::parse_int_literal(item.clone(), index, bucket.clone())?;
                    array_items.push(int.0);
                }

                IdentifierKind::BOOLEANLITERAL | IdentifierKind::TRUE | IdentifierKind::FALSE => {
                    let int = Self::parse_boolean_literal(item.clone(), index, bucket.clone())?;
                    array_items.push(int.0);
                }

                // use this an assignment project
                IdentifierKind::STRINGLITERAL => {
                    let obj = StringValue {
                        value: Some(item.literal.clone()),
                    };
                    let literal = LiteralObjects::ObjStringValue(obj);

                    let node = Node {
                        identifier_kind: Some(IdentifierKind::STRINGLITERAL),
                        left_child: Some(Left(literal)),
                        ..Default::default()
                    };
                    let obj_type = Objects::TyNode(node);

                    array_items.push(obj_type);
                }

                _ => {
                    return Err(errors::KarisError {
                         error_type: errors::KarisErrorType::InvalidSyntax,
                         message: format!(
                             "[INVALID SYNTAX] Syntax not correct. Expected something after `{}. Ln {} Col {}  '",
                             tok.literal,tok.line_number,tok.column_number
                         ),
                     });
                }
            }
        }

        array_items = array_items
            .iter()
            .filter(|item| !item.is_ty_unknown())
            .cloned()
            .collect::<Vec<Objects>>();

        let node = Node {
            identifier_kind: Some(IdentifierKind::ARRAY),
            array_type: Some(Self::typing_kind(array_typing_info)),
            block_children: Some(array_items),
            ..Default::default()
        };

        // move cursor to the next token. We don't want `RSQUAREBRACE` to be parsed

        Ok((Objects::TyNode(node), rsquare_index + 0x01))
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
    //
    //     if n == 1 || n == 2 {
    //        return 1
    //     };
    //
    //     return fibnacci(n-1) + fibnacci(n-2);
    // }
    //
    pub(crate) fn parse_function_definition(
        tok: Token,
        index: usize,
        bucket: Rc<RefCell<Vec<Token>>>,
    ) -> Result<(Objects, usize), errors::KarisError> {
        let borrow = bucket.borrow();

        // parser validation : fn definition should be complete
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

        // parser validation : FUNCTION should be followed by `(`
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

        // parser validation : FUNCTION should have a body
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

        let block_children_filtered = block_children
            .iter()
            .filter(|item| !item.is_ty_unknown())
            .cloned()
            .collect::<Vec<Objects>>();

        let node = Node {
            identifier_kind: Some(IdentifierKind::FUNCTION),
            func_params: Some(function_args),
            block_children: Some(block_children_filtered),
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

        let block_children_filtered = block_children
            .iter()
            .filter(|item| !item.is_ty_unknown())
            .cloned()
            .collect::<Vec<Objects>>();

        let node = Node {
            identifier_kind: Some(IdentifierKind::BLOCK),
            block_children: Some(block_children_filtered),
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
                variable_name: Some(tok.literal),
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
            let res = Parser::expression(0, index + 0x01, bucket.clone())?;

            // we re-organize the object. We want the UNION kind to be the top-level object, not the root of the
            // enclosed arthemetic expression.
            let object = reorganize_parenthesis_object(res.0.clone());

            Ok((object, res.1))
        } else {
            Err(errors::KarisError {
                error_type: errors::KarisErrorType::InvalidSyntax,
                message: format!(
                    "[INVALID SYNTAX] Syntax not correct. Expected INT or CALLER after `{}. Ln {} Col {}  '",
                    tok.literal,tok.line_number,tok.column_number
                ),
            })
        }
    }

    // Evaluates a `-` or `+` token as a prefix
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
                    "[INVALID SYNTAX] Syntax not correct. Expected something after `{}. Ln {} Col {}",
                    tok.literal,tok.line_number,tok.column_number
                ),
            });
        }

        let next_token = next_token.unwrap();
        if next_token.token_type != IdentifierKind::INTLITERAL {
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::InvalidSyntax,
                message: format!(
                    "[INVALID SYNTAX] Syntax not correct after `{}`. Expected integer. Ln {} Col {}",
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

        match tok.token_type {
            IdentifierKind::PLUS | IdentifierKind::MINUS => {
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
            _ => Err(errors::KarisError {
                error_type: errors::KarisErrorType::InvalidSyntax,
                message: format!(
                    "[INVALID SYNTAX] Syntax not correct. Expected `-` or `+` `{}. Ln {} Col {}",
                    tok.literal, tok.line_number, tok.column_number
                ),
            }),
        }
    }

    pub(crate) fn parse_bang_as_prefix(
        tok: Token,
        index: usize,
        bucket: Rc<RefCell<Vec<Token>>>,
    ) -> Result<(Objects, usize), errors::KarisError> {
        let borrow = bucket.borrow();

        let next_token_index = index + 0x01;

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
        match next_token.token_type {
            IdentifierKind::TRUE | IdentifierKind::FALSE => {
                let (bool_object, last_index) = Self::parse_boolean_literal(
                    next_token.clone(),
                    next_token_index,
                    bucket.clone(),
                )?;

                let node = Node {
                    identifier_kind: Some(IdentifierKind::BANG),
                    right_child: Some(Right(Box::new(bool_object))),
                    ..Default::default()
                };

                Ok((Objects::TyNode(node), last_index))
            }

            IdentifierKind::BANG => {
                let (bang_object, last_index) = Self::parse_bang_as_prefix(
                    next_token.clone(),
                    next_token_index,
                    bucket.clone(),
                )?;

                let node = Node {
                    identifier_kind: Some(IdentifierKind::BANG),
                    right_child: Some(Right(Box::new(bang_object))),
                    ..Default::default()
                };

                Ok((Objects::TyNode(node), last_index))
            }

            _ => Err(errors::KarisError {
                error_type: errors::KarisErrorType::InvalidSyntax,
                message: format!(
                    "[INVALID SYNTAX] Syntax not correct after `{}` Expected boolean. Ln {} Col {}",
                    tok.literal, tok.line_number, tok.column_number
                ),
            }),
        }
    }

    // To parse a `if` or `else` expression, we move the cursor to the right unit we encounter a `{`.
    // We collect items in between and parse them. After that we collect items between `{` and `}` and add them
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
    //
    //     result x * y;
    // };
    //
    // let factorial @int = fn(n @int){
    //     if n == 1 {
    //         return 1;
    //     };
    //
    //     return n * factorial(n-1);
    // };
    //
    pub(crate) fn parse_if_expressions(
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

        #[allow(clippy::redundant_clone)]
        let index_at_if_lbrace = Self::traverse_forward_until(
            tok.clone(),
            index,
            bucket.clone(),
            IdentifierKind::LBRACE,
        )?;

        let items_before_if_lbrace = borrow.get(index + 0x01..index_at_if_lbrace).unwrap();
        let exp_vec_tokens = Vec::from(items_before_if_lbrace);
        let expression_node = Parser::default()
            .program_as_non_root()
            .parse_from_vec(exp_vec_tokens)?;

        // if block items
        let index_at_if_rbrace =
            Self::traverse_forward_until(tok, index, bucket.clone(), IdentifierKind::RBRACE)?;

        let items_after_lbrace = borrow
            .get(index_at_if_lbrace + 0x01..index_at_if_rbrace)
            .unwrap();

        let if_block_vec_tokens = Vec::from(items_after_lbrace);
        let if_block_node = Parser::default()
            .program_as_non_root()
            .parse_from_vec(if_block_vec_tokens)?;

        // compute alternate condition
        let alt_token_index = index_at_if_rbrace + 0x01;
        let alt_token = borrow.get(alt_token_index).unwrap();

        let (alternate_node, end_index) =
            Self::parse_else_expressions(alt_token.clone(), alt_token_index, bucket.clone())
                .unwrap();

        let alternate_node = match alternate_node {
            Objects::TyProgram(_) => None,
            Objects::TyConsumable => None,
            Objects::TyUnknown => None,
            Objects::TyNode(node) => {
                let kind = node.identifier_kind.unwrap();
                match kind {
                    IdentifierKind::ELSE => {
                        let obj = Objects::TyNode(node);
                        Some(Box::new(obj))
                    }
                    _ => None,
                }
            }
        };

        let node = Node {
            identifier_kind: Some(IdentifierKind::IF),
            right_child: Some(Right(Box::new(expression_node))),
            alternate: alternate_node,
            block_children: Some(Vec::from([if_block_node])),
            ..Default::default()
        };

        Ok((Objects::TyNode(node), end_index))
    }

    pub(crate) fn parse_else_expressions(
        tok: Token,
        index: usize,
        bucket: Rc<RefCell<Vec<Token>>>,
    ) -> Result<(Objects, usize), errors::KarisError> {
        if tok.token_type != IdentifierKind::ELSE {
            return Ok((Objects::TyUnknown, index));
        }

        let borrow = bucket.borrow();

        #[allow(clippy::redundant_clone)]
        let index_at_else_lbrace = Self::traverse_forward_until(
            tok.clone(),
            index,
            bucket.clone(),
            IdentifierKind::LBRACE,
        )?;

        let items = borrow.get(index + 0x01..index_at_else_lbrace).unwrap();
        let vec_tokens = Vec::from(items);
        let else_expression_node = Parser::default()
            .program_as_non_root()
            .parse_from_vec(vec_tokens)?;

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
        let else_block_node = Parser::default()
            .program_as_non_root()
            .parse_from_vec(else_block_vec_tokens)?;

        let alt_token_index = index_at_else_rbrace + 0x01;
        let alt_token = borrow.get(alt_token_index).unwrap();

        let (alternate_node, end_index) =
            Self::parse_else_expressions(alt_token.clone(), alt_token_index, bucket.clone())
                .unwrap();

        let alternate_node = match alternate_node {
            Objects::TyProgram(_) => None,
            Objects::TyConsumable => None,
            Objects::TyUnknown => None,
            Objects::TyNode(node) => {
                let kind = node.identifier_kind.unwrap();
                match kind {
                    IdentifierKind::ELSE => {
                        let obj = Objects::TyNode(node);
                        Some(Box::new(obj))
                    }
                    _ => None,
                }
            }
        };

        let node = Node {
            identifier_kind: Some(IdentifierKind::ELSE),
            right_child: Some(Right(Box::new(else_expression_node))),
            block_children: Some(Vec::from([else_block_node])),
            alternate: alternate_node,
            ..Default::default()
        };

        let obj = Objects::TyNode(node);

        Ok((obj, end_index))
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

        let items_in_block = borrow.get(index_lbrace + 0x01..index_end - 0x02);

        if items_in_block.is_none() {
            let msg = "Main block should not be empty".to_string();
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::InvalidSyntax,
                message: format!("{:?}", msg),
            });
        }

        let items_in_block = items_in_block.unwrap();

        let block_tokens = Vec::from(items_in_block);
        let block_node = Parser::default()
            .program_as_non_root()
            .parse_from_vec(block_tokens)?;

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
    // Return the node and the current cursor position minus 1 ( index-0x01)
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

        let index_of_let_ident = {
            let previuos = borrow.get(index - 0x01).unwrap();
            if previuos.token_type == IdentifierKind::RSQUAREBRACE {
                index - 0x05
            } else {
                index - 0x03
            }
        };

        // parser validation : check if variable name has been specified
        let variable_name_token = borrow.get(index_of_let_ident + 0x01);
        if variable_name_token.is_none() {
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::MissingVariableName,
                message: format!(
                    "[MISSING VARIABLE NAME] Expected to find `<variable name>` ; Token {:?} Ln {} Col {}",
                    tok.literal, tok.line_number, tok.column_number
                ),
            });
        }

        // parser validation : check if typing information has been provided
        let typing_token = borrow.get(index_of_let_ident + 0x02);
        if typing_token.is_none() {
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::MissingTypeInfo,
                message: format!(
                    "[MISSING TYPE INFO] Expected to find either `@int | @string | @bool | @unit` ; Token {:?} Ln {} Col {}",
                    tok.literal, tok.line_number, tok.column_number
                ),
            });
        }

        let typing_token = typing_token.unwrap();

        if typing_token.token_type == IdentifierKind::LSQUAREBRACE {
            let array_typing_token = borrow.get(index_of_let_ident + 0x03);

            if array_typing_token.is_none() {
                return Err(errors::KarisError {
                    error_type: errors::KarisErrorType::MissingTypeInfo,
                    message: format!(
                        "[MISSING TYPE INFO] Expected to find either `@int | @string | @bool | @unit` ; Token {:?} Ln {} Col {}",
                        tok.literal, tok.line_number, tok.column_number
                    ),
                });
            }

            let array_typing_token = array_typing_token.unwrap();

            match array_typing_token.token_type {
                IdentifierKind::INTTYPE
                | IdentifierKind::STRINGTYPE
                | IdentifierKind::BOOLEANTYPE => {
                    let node = Node {
                        identifier_kind: Some(IdentifierKind::LET),
                        variable_name: Some(variable_name_token.unwrap().literal.clone()),
                        return_type: Some(Self::typing_kind(typing_token)),
                        array_type: Some(Self::typing_kind(array_typing_token)),
                        ..Default::default()
                    };

                    // return index that points to the `RSQUAREBRACE` not the `Assign`
                    Ok((Objects::TyNode(node), index_of_let_ident + 0x04))
                }
                _ => {
                    Err(errors::KarisError {
                        error_type: errors::KarisErrorType::MissingTypeInfo,
                        message: format!(
                            "[MISSING TYPE INFO] Expected to find either `@int | @string | @bool | @unit` as array typing ; Token {:?} Ln {} Col {}",
                            array_typing_token.literal, array_typing_token.line_number, array_typing_token.column_number
                        ),
                    })
                }
            }
        } else {
            let node = Node {
                identifier_kind: Some(IdentifierKind::LET),
                variable_name: Some(variable_name_token.unwrap().literal.clone()),
                return_type: Some(Self::typing_kind(typing_token)),
                ..Default::default()
            };

            // return index that points to the `typing` not the `Assign`
            Ok((Objects::TyNode(node), index - 0x1))
        }
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
