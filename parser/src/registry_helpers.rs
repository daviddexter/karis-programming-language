use std::cell::RefCell;
use std::rc::Rc;

use either::Either;
use either::Either::{Left, Right};

use errors::errors;
use lexer::tokens::{IdentifierKind, Token};

use crate::registry::TokenRegistry;
use crate::{
    objects::{
        BooleanValue, IntergerValue, LiteralObjects, Node, Objects, StringValue, TypingKind,
    },
    parser::Parser,
};

/// parser implementations helpers
impl TokenRegistry {
    /// given an expression, we move the cursor along the length of the expression
    /// until we encounter a token of the given kind then returns the index before it
    pub(crate) fn traverse_forward_until(
        tok: Token,
        index: usize,
        bucket: Rc<RefCell<Vec<Token>>>,
        kind: IdentifierKind,
    ) -> Result<usize, errors::KarisError> {
        let borrow = bucket.borrow();
        if index >= borrow.len() - 0x01 {
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::InvalidSyntax,
                message: format!(
                    "[INVALID SYNTAX] Syntax not correct. Expected matching closing parentheses. Ln {} Col {}",
                    tok.line_number,tok.column_number
                ),
            });
        }

        if tok.token_type != kind {
            let new_index = index + 0x01;
            let next_token = &bucket.borrow()[new_index];
            Self::traverse_forward_until(next_token.clone(), new_index, bucket.clone(), kind)
        } else {
            Ok(index)
        }
    }

    /// given a `call` token, it moves forward recursively gathering args of the call
    /// if it encounters another `call` token, it calls parse_function_call then adds the result
    /// as a call param
    pub(crate) fn collect_function_call_params(
        idx: usize,
        bucket: Rc<RefCell<Vec<Token>>>,
        mut params: Vec<Either<LiteralObjects, Objects>>,
        mut closing_index: usize,
    ) -> Result<(Vec<Either<LiteralObjects, Objects>>, usize), errors::KarisError> {
        let borrow = bucket.borrow();

        if idx >= borrow.len() {
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::InvalidSyntax,
                message: "[MALFORMED PROGRAM] Failed to match closing parenthesis".to_string(),
            });
        }

        let arg = borrow.get(idx).unwrap();
        match arg.token_type {
            IdentifierKind::STRINGLITERAL => {
                let obj = StringValue {
                    value: Some(arg.literal.clone()),
                };
                let literal: LiteralObjects = LiteralObjects::ObjStringValue(obj);
                params.push(Left(literal));
                Self::collect_function_call_params(
                    idx + 0x01,
                    bucket.clone(),
                    params,
                    closing_index,
                )
            }
            IdentifierKind::INTLITERAL => {
                let value = arg
                    .literal
                    .parse::<isize>()
                    .unwrap_or_else(|_| panic!("Failed to parse to INT"));
                let obj = IntergerValue { value: Some(value) };

                let literal = LiteralObjects::ObjIntergerValue(obj);
                params.push(Left(literal));
                Self::collect_function_call_params(
                    idx + 0x01,
                    bucket.clone(),
                    params,
                    closing_index,
                )
            }
            IdentifierKind::TRUE | IdentifierKind::FALSE => {
                let value = arg
                    .literal
                    .parse::<bool>()
                    .unwrap_or_else(|_| panic!("Failed to parse to BOOL"));
                let obj = BooleanValue { value: Some(value) };

                let literal = LiteralObjects::ObjBooleanValue(obj);
                params.push(Left(literal));
                Self::collect_function_call_params(
                    idx + 0x01,
                    bucket.clone(),
                    params,
                    closing_index,
                )
            }
            IdentifierKind::VARIABLE => {
                let node = Node {
                    variable_name: Some(arg.literal.clone()),
                    identifier_kind: Some(IdentifierKind::VARIABLE),
                    ..Default::default()
                };

                let obj = Objects::TyNode(node);
                params.push(Right(obj));
                Self::collect_function_call_params(
                    idx + 0x01,
                    bucket.clone(),
                    params,
                    closing_index,
                )
            }
            IdentifierKind::CALLER => {
                let (obj, last_index) =
                    Self::parse_function_call(arg.clone(), idx, bucket.clone())?;
                params.push(Right(obj));
                Self::collect_function_call_params(
                    last_index + 0x01,
                    bucket.clone(),
                    params,
                    closing_index,
                )
            }
            IdentifierKind::RPAREN => {
                closing_index = idx;
                Ok((params, closing_index))
            }
            _ => Self::collect_function_call_params(
                idx + 0x01,
                bucket.clone(),
                params,
                closing_index,
            ),
        }
    }

    /// walks the function collecting its arguments until a `)` token is encountered.
    /// returns the token as a vec and the index of `{` token
    pub(crate) fn collect_function_definition_args(
        idx: usize,
        bucket: Rc<RefCell<Vec<Token>>>,
        mut params: Vec<Either<LiteralObjects, Objects>>,
        mut closing_index: usize,
    ) -> Result<(Vec<Either<LiteralObjects, Objects>>, usize), errors::KarisError> {
        let borrow = bucket.borrow();

        if idx >= borrow.len() {
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::InvalidSyntax,
                message: "[MALFORMED PROGRAM] Failed to match closing parenthesis".to_string(),
            });
        }

        let arg = borrow.get(idx).unwrap();

        match arg.token_type {
            IdentifierKind::RPAREN => {
                closing_index = idx;
                Ok((params, closing_index + 0x01))
            }

            IdentifierKind::COMMA => Self::collect_function_definition_args(
                idx + 0x01,
                bucket.clone(),
                params,
                closing_index,
            ),

            IdentifierKind::VARIABLE => {
                let next_idx = idx + 0x01;
                if let Some(next_token) = borrow.get(next_idx) {
                    match next_token.token_type {
                            IdentifierKind::STRINGTYPE => {
                                let node = Node {
                                    variable_name: Some(arg.literal.clone()),
                                    return_type: Some(TypingKind::String),
                                    identifier_kind: Some(IdentifierKind::VARIABLE),..Default::default()
                                };
                                let obj = Objects::TyNode(node);
                                params.push(Right(obj));
                                Self::collect_function_definition_args(next_idx+0x01, bucket.clone(), params, closing_index)
                            },
                            IdentifierKind::INTTYPE => {
                                let node = Node {
                                    variable_name: Some(arg.literal.clone()),
                                    return_type: Some(TypingKind::Int),
                                    identifier_kind: Some(IdentifierKind::VARIABLE),..Default::default()
                                };
                                let obj = Objects::TyNode(node);
                                params.push(Right(obj));
                                Self::collect_function_definition_args(next_idx+0x01, bucket.clone(), params, closing_index)
                            },
                            IdentifierKind::BOOLEANTYPE => {
                                let node = Node {
                                    variable_name: Some(arg.literal.clone()),
                                    return_type: Some(TypingKind::Int),
                                    identifier_kind: Some(IdentifierKind::VARIABLE),..Default::default()
                                };
                                let obj = Objects::TyNode(node);
                                params.push(Right(obj));
                                Self::collect_function_definition_args(next_idx+0x01, bucket.clone(), params, closing_index)
                            },_ => {
                                Err(errors::KarisError {
                                    error_type: errors::KarisErrorType::InvalidSyntax,
                                    message: format!(
                                        "[INVALID SYNTAX] Syntax not correct. Expected either `@int`, `@string`, or `@bool` {}. Ln {} Col {}  '",
                                        next_token.literal,next_token.line_number,next_token.column_number
                                    ),
                                })
                            }
                        }
                } else {
                    Err(errors::KarisError {
                            error_type: errors::KarisErrorType::InvalidSyntax,
                            message: format!(
                                "[INVALID SYNTAX] Syntax not correct. Expected something after `{}. Ln {} Col {}  '",
                                arg.literal,arg.line_number,arg.column_number
                            ),
                        })
                }
            }

            IdentifierKind::STRINGLITERAL => {
                let value = StringValue {
                    value: Some(arg.literal.clone()),
                };
                let literal = LiteralObjects::ObjStringValue(value);

                params.push(Left(literal));
                Self::collect_function_definition_args(
                    idx + 0x01,
                    bucket.clone(),
                    params,
                    closing_index,
                )
            }

            IdentifierKind::INTLITERAL => {
                let value = arg
                    .literal
                    .parse::<isize>()
                    .unwrap_or_else(|_| panic!("Failed to parse to INT"));
                let obj = IntergerValue { value: Some(value) };

                let literal = LiteralObjects::ObjIntergerValue(obj);
                params.push(Left(literal));
                Self::collect_function_definition_args(
                    idx + 0x01,
                    bucket.clone(),
                    params,
                    closing_index,
                )
            }

            IdentifierKind::TRUE | IdentifierKind::FALSE => {
                let value = arg
                    .literal
                    .parse::<bool>()
                    .unwrap_or_else(|_| panic!("Failed to parse to BOOL"));
                let obj = BooleanValue { value: Some(value) };

                let literal = LiteralObjects::ObjBooleanValue(obj);
                params.push(Left(literal));
                Self::collect_function_definition_args(
                    idx + 0x01,
                    bucket.clone(),
                    params,
                    closing_index,
                )
            }

            _ => Err(errors::KarisError {
                error_type: errors::KarisErrorType::InvalidSyntax,
                message: format!(
                    "[INVALID SYNTAX] Syntax not correct.`Token {}` Ln {} Col {}  '",
                    arg.literal, arg.line_number, arg.column_number
                ),
            }),
        }
    }

    /// recursively collects expressions enclosed in a `{}` block
    /// These expressions are parsed individuallly then appended to the `children` vector
    /// The `closing_index` returned is the index of the `;` (semicolon) token at the end of the `{}` block
    pub(crate) fn collect_block_children(
        idx: usize,
        bucket: Rc<RefCell<Vec<Token>>>,
        mut children: Vec<Objects>,
        mut closing_index: usize,
    ) -> Result<(Vec<Objects>, usize), errors::KarisError> {
        let borrow = bucket.borrow();

        if idx >= borrow.len() {
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::InvalidSyntax,
                message: "[MALFORMED PROGRAM] Failed to match closing parenthesis".to_string(),
            });
        }

        let next_index = idx + 0x01;
        let child = borrow.get(next_index).unwrap();

        match child.token_type {
            // collect all children between braces and remove them.
            // the expectation is that these children have already been parsed.
            IdentifierKind::RBRACE => {
                closing_index = idx;
                Ok((children, closing_index))
            }
            IdentifierKind::RETURN | IdentifierKind::SEMICOLON | IdentifierKind::LET => {
                let (node, last_index) = Parser::expression(0, next_index, bucket.clone())?;
                children.push(node);
                Self::collect_block_children(last_index, bucket.clone(), children, closing_index)
            }

            IdentifierKind::IF => {
                let (node, last_index) =
                    Self::parse_if_else_expressions(child.clone(), next_index, bucket.clone())?;
                children.push(node);
                Self::collect_block_children(last_index, bucket.clone(), children, closing_index)
            }

            _ => Self::collect_block_children(idx + 0x01, bucket.clone(), children, closing_index),
        }
    }
}
