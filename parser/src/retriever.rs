use either::Either;
use lexer::tokens::IdentifierKind;
use Either::Right;

use crate::objects::{BooleanValue, IntegerValue, LiteralObjects, Node, Objects, StringValue};

/// given an owned object, return an owned node
pub(crate) fn node_from_object(object: Objects) -> Node {
    object.as_ty_node().unwrap().clone()
}

/// given a reference to a node, returns a reference to it's child on either the left side or the right side
/// the child itself is of type `Either`
#[allow(dead_code)]
pub(crate) fn node_child(node: &Node, left_child: bool) -> &Either<LiteralObjects, Box<Objects>> {
    if left_child {
        node.left_child.as_ref().unwrap()
    } else {
        node.right_child.as_ref().unwrap()
    }
}

/// given a reference to an `Either` type, return the RHS which is a reference to a `Node`
#[allow(dead_code)]
pub(crate) fn right_side_of_either(e: &Either<LiteralObjects, Box<Objects>>) -> &Node {
    e.as_ref().right().unwrap().as_ty_node().unwrap()
}

/// given a reference to an `Either` type, return the LHS which is a reference to a `LiteralObjects`
#[allow(dead_code)]
pub(crate) fn left_side_of_either(e: &Either<LiteralObjects, Box<Objects>>) -> &LiteralObjects {
    e.as_ref().left().unwrap()
}

#[allow(dead_code)]
fn integer_from_literal_object(literal_object: &LiteralObjects) -> &IntegerValue {
    literal_object.as_obj_integer_value().unwrap()
}

#[allow(dead_code)]
fn boolean_from_literal_object(literal_object: &LiteralObjects) -> &BooleanValue {
    literal_object.as_obj_boolean_value().unwrap()
}

#[allow(dead_code)]
fn string_from_literal_object(literal_object: &LiteralObjects) -> &StringValue {
    literal_object.as_obj_string_value().unwrap()
}

/// given a reference to a node, recursively walk down the node and return a reference to it's Integer value.
#[allow(dead_code)]
pub(crate) fn integer_value_from_nested_node(node: &Node, left_child: bool) -> &IntegerValue {
    let child = node_child(node, left_child);
    if child.is_right() {
        let node0 = right_side_of_either(child);
        integer_value_from_nested_node(node0, true)
    } else {
        let lit = left_side_of_either(child);
        integer_from_literal_object(lit)
    }
}

// given a parenthesis object, re-organize it's internal structure to place the `GROUPING` object at
// the root
pub(crate) fn reorganize_parenthesis_object(object: Objects) -> Objects {
    let root_node = node_from_object(object);
    let node = unwind_node(root_node);

    let new_root_node = Node {
        identifier_kind: Some(IdentifierKind::GROUPING),
        right_child: Some(Right(Box::new(Objects::TyNode(node)))),
        ..Default::default()
    };

    Objects::TyNode(new_root_node)
}

fn unwind_node(mut node: Node) -> Node {
    if node.identifier_kind.unwrap() == IdentifierKind::GROUPING {
        let node_child = node_child(&node, false);
        let rhs = right_side_of_either(node_child);
        rhs.clone()
    } else {
        let node_child = node_child(&node, false);
        let rhs = right_side_of_either(node_child);
        let rhs_unwound = unwind_node(rhs.clone());
        node.right_child = Some(Right(Box::new(Objects::TyNode(rhs_unwound))));
        node
    }
}
