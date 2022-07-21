use lexer::tokens::IdentifierKind;

use crate::objects::{Declaration, Node, TypingKind, Value};

pub(crate) fn let_and_variables(node: &Node, kind: IdentifierKind) -> String {
    let default_return_type = &TypingKind::Unknown;
    let default_variable_name = &"".to_string();

    let return_type = node.return_type.as_ref().unwrap_or(default_return_type);
    let variable_name = node.variable_name.as_ref().unwrap_or(default_variable_name);
    let out =
        format!("NODE({kind:#?} : RETURN_TYPE={return_type:#?} VARIABLE_NAME={variable_name})");
    out
}

pub(crate) fn assign(node: &Node) -> String {
    let kind = IdentifierKind::ASSIGN;

    let lhs = node.left_child.as_ref().unwrap().as_ref().right().unwrap();
    let lhs_str = lhs.inspect();

    let rhs_str = if node.right_child.as_ref().unwrap().as_ref().is_left() {
        let rhs = node.right_child.as_ref().unwrap().as_ref().left().unwrap();
        rhs.inspect()
    } else {
        let rhs = node.right_child.as_ref().unwrap().as_ref().right().unwrap();
        rhs.inspect()
    };

    let out = format!(
        "
NODE({kind:#?})
    ---NODE(LHS : {lhs_str})
    ---NODE(RHS : {rhs_str})"
    );
    out
}

pub(crate) fn function(node: &Node) -> String {
    let kind = IdentifierKind::FUNCTION;

    let mut params_str = String::from("");
    if let Some(params_vec) = node.func_params.as_ref() {
        let params = params_vec.clone();
        for param in params.iter() {
            if param.is_left() {
                let left = param.as_ref().left().unwrap();
                let left_str = left.inspect();
                let left_str = format!("- {left_str}; ");
                params_str.push_str(left_str.as_str());
            } else {
                let right = param.as_ref().right().unwrap();
                let right_str = right.inspect();
                let right_str = format!("- {right_str}; ");
                params_str.push_str(right_str.as_str());
            }
        }
    }

    let mut block_children_str = String::from("");
    if let Some(block_children_vec) = node.block_children.as_ref() {
        let children = block_children_vec.clone();
        for child in children.iter() {
            let child_str = child.inspect();
            block_children_str.push_str(child_str.as_str());
        }
    }

    let out = format!(
        "
                    NODE({kind:#?}) 
                        ---PARAMS {params_str} 
                        ---BLOCK {block_children_str}"
    );
    out
}

pub(crate) fn infix_operators(node: &Node, kind: IdentifierKind) -> String {
    let left_child = node.left_child.as_ref().unwrap().as_ref().right().unwrap();
    let left_child_str = left_child.inspect();

    let right_child = node.right_child.as_ref().unwrap().as_ref().right().unwrap();
    let right_child_str = right_child.inspect();

    let out = format!(
        "
                                NODE({kind:#?})
                                    ---NODE(LHS : {left_child_str})
                                    ---NODE(RHS : {right_child_str})"
    );

    out
}

pub(crate) fn returner(node: &Node) -> String {
    let kind = IdentifierKind::RETURN;

    let child = node.right_child.as_ref().unwrap().as_ref().right().unwrap();
    let child_str = child.inspect();
    let out = format!(
        "
                            NODE({kind:#?}) {child_str}"
    );
    out
}

pub(crate) fn literals(node: &Node) -> String {
    let lit = node.left_child.as_ref().unwrap().as_ref().left().unwrap();
    lit.inspect()
}
