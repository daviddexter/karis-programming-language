use std::cell::RefCell;
use std::rc::Rc;

use either::Either::{Left, Right};

use errors::errors;
use lexer::tokens::{IdentifierKind, Token};

use crate::registry::TokenRegistry;
use crate::{
    objects::{
        BooleanValue, IntergerValue, LiteralObjects, Node, Objects,StringValue,       
    },
    parser::Parser,
};


// parser implementations methods for LEDs
// consumes to the right with a left-context its "Left-Denotation"
impl TokenRegistry {
    fn preconditions(
        token_index: usize,
        bucket: Rc<RefCell<Vec<Token>>>,
        identifiers: Vec<IdentifierKind>,
    ) -> Result<(Token, Token), errors::KarisError> {
        let borrow = bucket.borrow();

        // defensive programming
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

        if identifiers.contains(&next_token.token_type) {
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::InvalidSyntax,
                message: format!(
                    "[INVALID SYNTAX] Syntax not correct after `{}`. Ln {} Col {}  '",
                    current_token.literal, current_token.line_number, current_token.column_number
                ),
            });
        }

        Ok((current_token.clone(), next_token.clone()))
    }

    // evaluates a returns object enclosed in parentheses () as it's left children
    pub(crate) fn parse_closing_parenthesis(
        left: Objects,
        token_index: usize,
        bucket: Rc<RefCell<Vec<Token>>>,
    ) -> Result<(Objects, usize), errors::KarisError> {
        let borrow = bucket.borrow();
        let current_token = borrow.get(token_index).unwrap();

        if current_token.token_type != IdentifierKind::RPAREN {
            return Err(errors::KarisError {
                error_type: errors::KarisErrorType::InvalidSyntax,
                message: format!(
                    "[INVALID SYNTAX] Syntax not correct. Expected `)` found {}. Ln {} Col {}  '",
                    current_token.literal, current_token.line_number, current_token.column_number
                ),
            });
        }

        let node = Node {
            identifier_kind: Some(current_token.token_type),
            left_child: Some(Right(Box::new(left))),
            ..Default::default()
        };

        Ok((Objects::TyNode(node), token_index))
    }

    // Evaluates the RHS of an arthemetic expression
    // These expressions can take varied forms.
    // Example:
    //      10 + 1 + 2 + 3;
    //      (10 + 1) + 2 * 3
    //      mul(2,3) / 10 + 1
    //      10(23 * (20 * 10 + 1))
    pub(crate) fn parse_infix_operator(
        left: Objects,
        token_index: usize,
        bucket: Rc<RefCell<Vec<Token>>>,
    ) -> Result<(Objects, usize), errors::KarisError> {
        let (current_token, _next_token) = Self::preconditions(
            token_index,
            bucket.clone(),
            vec![
                IdentifierKind::EOS,
                IdentifierKind::EOF,
                IdentifierKind::LET,
                IdentifierKind::FUNCTION,
                IdentifierKind::TRUE,
                IdentifierKind::FALSE,
                IdentifierKind::STRINGLITERAL,
            ],
        )?;

        // if the current token is a opening parentheses, call it's NUD function instead
        let res: (Objects, usize) = if current_token.token_type == IdentifierKind::LPAREN {
            Self::parse_opening_parenthesis(current_token.clone(), token_index, bucket)?
        } else {
            let result = Parser::expression(0x00, token_index + 0x01, bucket);
            if result.is_err() {
                let err = result.err().unwrap();
                return Err(err);
            }
            result.unwrap()
        };

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
    pub(crate) fn parse_assign_operator(
        left: Objects,
        token_index: usize,
        bucket: Rc<RefCell<Vec<Token>>>,
    ) -> Result<(Objects, usize), errors::KarisError> {
        let (current_token, next_token) = Self::preconditions(
            token_index,
            bucket.clone(),
            vec![
                IdentifierKind::EOS,
                IdentifierKind::EOF,
                IdentifierKind::SEMICOLON,
                IdentifierKind::LET,
                IdentifierKind::GT,
                IdentifierKind::LT,
                IdentifierKind::GTOREQ,
                IdentifierKind::LTOREQ,
                IdentifierKind::EQ,
            ],
        )?;

        let borrow = bucket.borrow();

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
                        value: Some(next_token.literal),
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

                if next_token.token_type == IdentifierKind::TRUE
                    || next_token.token_type == IdentifierKind::FALSE
                {
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