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

    left_child_nodes
        .iter()
        .for_each(|ln| nodes.push(ln.clone()));
    right_child_nodes
        .iter()
        .for_each(|rn| nodes.push(rn.clone()));

    let mut edges = Vec::new();

    edges.push((root.clone(), left_child_nodes[0x00].0.clone()));
    edges.push((root, right_child_nodes[0x00].0.clone()));

    left_child_edges
        .iter()
        .for_each(|le| edges.push(le.clone()));
    right_child_edges
        .iter()
        .for_each(|re| edges.push(re.clone()));

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

    left_child_nodes
        .iter()
        .for_each(|ln| nodes.push(ln.clone()));
    right_child_nodes
        .iter()
        .for_each(|rn| nodes.push(rn.clone()));

    let mut edges = Vec::new();

    if !left_child_nodes.is_empty() {
        edges.push((root.clone(), left_child_nodes[0x00].0.clone()));
    }
    edges.push((root, right_child_nodes[0x00].0.clone()));

    left_child_edges
        .iter()
        .for_each(|le| edges.push(le.clone()));
    right_child_edges
        .iter()
        .for_each(|re| edges.push(re.clone()));

    (nodes, edges)
}

pub(crate) fn function(node: &Node) -> NodeEdge {
    let kind = IdentifierKind::FUNCTION;

    let root = random_name_gen();
    let mut nodes = Vec::new();
    nodes.push((root.clone(), format!("NODE({kind:#?})")));

    let mut edges = Vec::new();

    if let Some(args) = node.func_params.as_ref() {
        args.iter().for_each(|arg| {
            if arg.is_left() {
                let a = arg.as_ref().left().unwrap();
                let a_nodes = a.inspect();
                let a_nodes = a_nodes
                    .iter()
                    .map(|n| (n.0.clone(), format!("FnARG : {}", n.1)))
                    .collect::<Vec<(String, String)>>();

                edges.push((root.clone(), a_nodes[0x00].0.clone()));

                a_nodes.iter().for_each(|a_node| nodes.push(a_node.clone()));
            } else {
                let a = arg.as_ref().right().unwrap();
                let (a_nodes, a_edges) = a.inspect();
                let a_nodes = a_nodes
                    .iter()
                    .map(|n| (n.0.clone(), format!("FnARG : {}", n.1)))
                    .collect::<Vec<(String, String)>>();

                edges.push((root.clone(), a_nodes[0x00].0.clone()));

                a_nodes.iter().for_each(|node| nodes.push(node.clone()));
                a_edges.iter().for_each(|edge| edges.push(edge.clone()));
            }
        });
    }

    if let Some(children) = node.block_children.as_ref() {
        children.iter().for_each(|child| {
            let (c_nodes, c_edges) = child.inspect();
            let c_nodes = c_nodes
                .iter()
                .map(|n| (n.0.clone(), format!("FnBODY : {}", n.1)))
                .collect::<Vec<(String, String)>>();

            if !c_nodes.is_empty() {
                edges.push((root.clone(), c_nodes[0x00].0.clone()));
            }

            c_nodes.iter().for_each(|node| nodes.push(node.clone()));
            c_edges.iter().for_each(|edge| edges.push(edge.clone()));
        });
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

    child_nodes.iter().for_each(|ln| nodes.push(ln.clone()));

    let mut edges = Vec::new();
    edges.push((root, child_edges[0x00].0.clone()));

    child_edges.iter().for_each(|e| edges.push(e.clone()));

    (nodes, edges)
}

pub(crate) fn default_node_edges() -> NodeEdge {
    let blank: Vec<(String, String)> = Vec::new();
    (blank.clone(), blank)
}
