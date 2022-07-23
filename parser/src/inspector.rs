use crate::objects::{Node, TypingKind, Value};
use lexer::tokens::IdentifierKind;
use random_string::generate;

pub type NodeEdge = (Vec<(String, String)>, Vec<(String, String)>);

pub(crate) fn random_name_gen() -> String {
    let charset = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
    generate(7, charset)
}

pub(crate) fn let_and_variables(node: &Node, kind: IdentifierKind) -> NodeEdge {
    let default_return_type = &TypingKind::Unknown;
    let default_variable_name = &"".to_string();

    let return_type = node.return_type.as_ref().unwrap_or(default_return_type);
    let variable_name = node.variable_name.as_ref().unwrap_or(default_variable_name);

    let root = random_name_gen();

    let mut nodes = Vec::new();
    nodes.push((
        root,
        format!("NODE({kind:#?} : RETURN_TYPE={return_type:#?} VARIABLE_NAME={variable_name})"),
    ));

    let edges: Vec<(String, String)> = Vec::new();
    (nodes, edges)
}

pub(crate) fn assign(node: &Node) -> NodeEdge {
    let kind = IdentifierKind::ASSIGN;

    let lhs = node.left_child.as_ref().unwrap().as_ref().right().unwrap();
    let (left_child_nodes, left_child_edges) = lhs.as_ty_node().unwrap().inspect();

    let (right_child_nodes, right_child_edges) =
        if node.right_child.as_ref().unwrap().as_ref().is_left() {
            let rhs = node.right_child.as_ref().unwrap().as_ref().left().unwrap();
            let nodes = rhs.inspect();
            let edges: Vec<(String, String)> = Vec::new();
            (nodes, edges)
        } else {
            let rhs = node.right_child.as_ref().unwrap().as_ref().right().unwrap();
            rhs.as_ty_node().unwrap().inspect()
        };

    let root = random_name_gen();

    let mut nodes = Vec::new();
    nodes.push((root.clone(), format!("NODE({kind:#?})")));

    for ln in left_child_nodes.iter() {
        nodes.push(ln.clone());
    }
    for rn in right_child_nodes.iter() {
        nodes.push(rn.clone());
    }

    let mut edges = Vec::new();

    edges.push((root.clone(), left_child_nodes[0x00].0.clone()));
    edges.push((root, right_child_nodes[0x00].0.clone()));

    for le in left_child_edges.iter() {
        edges.push(le.clone());
    }

    for re in right_child_edges.iter() {
        edges.push(re.clone());
    }

    (nodes, edges)
}

pub(crate) fn infix_operators(node: &Node, kind: IdentifierKind) -> NodeEdge {
    let left_child = node.left_child.as_ref().unwrap().as_ref().right().unwrap();
    let (left_child_nodes, left_child_edges): NodeEdge = left_child.as_ty_node().unwrap().inspect();

    let right_child = node.right_child.as_ref().unwrap().as_ref().right().unwrap();
    let (right_child_nodes, right_child_edges): NodeEdge =
        right_child.as_ty_node().unwrap().inspect();

    let root = random_name_gen();

    let mut nodes = Vec::new();
    nodes.push((root.clone(), format!("NODE({kind:#?})")));

    for ln in left_child_nodes.iter() {
        nodes.push(ln.clone());
    }
    for rn in right_child_nodes.iter() {
        nodes.push(rn.clone());
    }

    let mut edges = Vec::new();

    edges.push((root.clone(), left_child_nodes[0x00].0.clone()));
    edges.push((root, right_child_nodes[0x00].0.clone()));

    for le in left_child_edges.iter() {
        edges.push(le.clone());
    }

    for re in right_child_edges.iter() {
        edges.push(re.clone());
    }

    (nodes, edges)
}

pub(crate) fn literals(node: &Node) -> NodeEdge {
    let lit = node.left_child.as_ref().unwrap().as_ref().left().unwrap();
    let nodes = lit.inspect();
    let edges: Vec<(String, String)> = Vec::new();
    (nodes, edges)
}

pub(crate) fn returner(node: &Node) -> NodeEdge {
    let kind = IdentifierKind::RETURN;

    let child = node.right_child.as_ref().unwrap().as_ref().right().unwrap();
    let (child_nodes, child_edges): NodeEdge = child.as_ty_node().unwrap().inspect();

    let root = random_name_gen();

    let mut nodes = Vec::new();
    nodes.push((root.clone(), format!("NODE({kind:#?})")));

    for ln in child_nodes.iter() {
        nodes.push(ln.clone());
    }

    let mut edges = Vec::new();
    edges.push((root, child_edges[0x00].0.clone()));

    for e in child_edges.iter() {
        edges.push(e.clone());
    }

    (nodes, edges)
}

pub(crate) fn default_node_edges() -> NodeEdge {
    let blank: Vec<(String, String)> = Vec::new();
    (blank.clone(), blank)
}
