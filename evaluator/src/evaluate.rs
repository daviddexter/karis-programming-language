use std::cell::RefCell;
use std::fmt;
use std::iter::zip;
use std::rc::Rc;

use colored::*;
use log::error;

use enum_as_inner::EnumAsInner;
use Either::{Left, Right};

use either::Either;
use lexer::tokens::IdentifierKind;
use parser::objects::{LiteralObjects, Node, Objects, Program};
use parser::parser::Parser;

type ScopeBindingResolver = hashbrown::HashMap<String, EvaluationObject>;

#[derive(Default, Debug, Clone, EnumAsInner)]
pub enum EvaluationObject {
    Integer(isize),
    Boolean(bool),
    String(String),
    ReturnValue(Rc<EvaluationObject>),
    Function(Node),

    #[default]
    Empty,
}

impl fmt::Display for EvaluationObject {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            EvaluationObject::Integer(i) => write!(f, "{}", i),
            EvaluationObject::Boolean(b) => write!(f, "{}", b),
            EvaluationObject::String(s) => write!(f, "{}", s),
            EvaluationObject::ReturnValue(obj) => write!(f, "{}", obj),
            EvaluationObject::Function(_) | EvaluationObject::Empty => Ok(()),
        }
    }
}

pub trait Evaluate {
    // Recurvisely runs througth the AST and returns an evaluation result.
    // We inject the `ScopeBindingResolver` to cache bindings in the scope.
    // The scope can be global or local. The default scope is global
    //
    fn eval(
        &self,
        scope: Rc<RefCell<ScopeBindingResolver>>,
        global: Option<Rc<RefCell<ScopeBindingResolver>>>,
    ) -> EvaluationObject;
}

pub struct Evaluator {
    parser: Parser,
}

impl Evaluator {
    pub fn new(parser: Parser) -> Self {
        Self { parser }
    }

    pub fn repl_evaluate_program(&mut self, scope: Rc<RefCell<ScopeBindingResolver>>) {
        match self.parser.parse(Some("repl_evaluate_program.json")) {
            Ok(program) => {
                let evaluated = program.eval(scope, None);
                println!("{}", evaluated);
            }
            Err(err) => println!("{}", err.to_string().red()),
        }
    }
}

impl Evaluate for Objects {
    fn eval(
        &self,
        scope: Rc<RefCell<ScopeBindingResolver>>,
        global: Option<Rc<RefCell<ScopeBindingResolver>>>,
    ) -> EvaluationObject {
        match self {
            Self::TyProgram(program) => program.eval(scope, global),
            Self::TyNode(node) => node.eval(scope, global),
            _ => unreachable!("cannot evaluate UNKNOWN or CONSUMABLE"),
        }
    }
}

impl Evaluate for Program {
    fn eval(
        &self,
        scope: Rc<RefCell<ScopeBindingResolver>>,
        global: Option<Rc<RefCell<ScopeBindingResolver>>>,
    ) -> EvaluationObject {
        let mut evaluation_result = EvaluationObject::Empty;

        for item in self.body.iter() {
            let val = item.eval(scope.clone(), global.clone());
            match val {
                EvaluationObject::ReturnValue(r) => {
                    let v = r.as_ref().clone();
                    evaluation_result = v
                }
                _ => evaluation_result = val,
            }
        }

        evaluation_result
    }
}

impl Evaluate for Node {
    fn eval(
        &self,
        scope: Rc<RefCell<ScopeBindingResolver>>,
        global: Option<Rc<RefCell<ScopeBindingResolver>>>,
    ) -> EvaluationObject {
        let kind = self.identifier_kind.unwrap();

        let scope_clone = scope.clone();

        match kind {
            IdentifierKind::INTLITERAL => {
                let lit = self.left_child.as_ref().unwrap().as_ref().left().unwrap();
                let int_value = lit.as_obj_interger_value().unwrap();
                let int_lit = int_value.value.unwrap();
                EvaluationObject::Integer(int_lit)
            }
            IdentifierKind::STRINGLITERAL => {
                let lit = self.left_child.as_ref().unwrap().as_ref().left().unwrap();
                let string_value = lit.as_obj_string_value().unwrap();
                let string_lit = string_value.value.as_ref().unwrap();
                EvaluationObject::String(string_lit.clone())
            }
            IdentifierKind::BOOLEANLITERAL => {
                let lit = self.left_child.as_ref().unwrap().as_ref().left().unwrap();
                let bool_value = lit.as_obj_boolean_value().unwrap();
                let bool_lit = bool_value.value.unwrap();
                EvaluationObject::Boolean(bool_lit)
            }

            IdentifierKind::ASSIGN => {
                let binding = self
                    .left_child
                    .as_ref()
                    .unwrap()
                    .as_ref()
                    .right()
                    .unwrap()
                    .as_ty_node()
                    .unwrap();

                let binding_key = binding.variable_name.as_ref().unwrap();

                let rhs = self.right_child.as_ref().unwrap();
                let rhs = left_or_right(rhs, scope, global);

                let mut scope_inner_mut = scope_clone.borrow_mut();
                scope_inner_mut.insert(binding_key.to_string(), rhs);
                EvaluationObject::Empty
            }

            IdentifierKind::VARIABLE => {
                let variable = self.variable_name.as_ref().unwrap();
                let scope_inner = scope_clone.borrow();

                if let Some(val) = scope_inner.get(variable) {
                    return val.clone();
                }
                EvaluationObject::Empty
            }

            IdentifierKind::PLUS => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, scope.clone(), global.clone());
                let left = match left {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, scope, global);
                let right = match right {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let sum = left + right;
                EvaluationObject::Integer(sum)
            }
            IdentifierKind::MINUS => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, scope.clone(), global.clone());
                let left = match left {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, scope, global);
                let right = match right {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let diff = left - right;
                EvaluationObject::Integer(diff)
            }
            IdentifierKind::ASTERISK => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, scope.clone(), global.clone());
                let left = match left {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, scope, global);
                let right = match right {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let product = left * right;
                EvaluationObject::Integer(product)
            }
            IdentifierKind::SLASH => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, scope.clone(), global.clone());
                let left = match left {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, scope, global);
                let right = match right {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let division = left / right;
                EvaluationObject::Integer(division)
            }
            IdentifierKind::MODULUS => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, scope.clone(), global.clone());
                let left = match left {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, scope, global);
                let right = match right {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let modulus = left % right;
                EvaluationObject::Integer(modulus)
            }
            IdentifierKind::GROUPING | IdentifierKind::RETURN => {
                let right = self.right_child.as_ref().unwrap();
                left_or_right(right, scope, global.clone())
            }
            IdentifierKind::LPAREN => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, scope.clone(), global.clone());
                let left = match left {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, scope, global);
                let right = match right {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let mul = left * right;
                EvaluationObject::Integer(mul)
            }
            IdentifierKind::FUNCTION => EvaluationObject::Function(self.clone()),
            IdentifierKind::CALLER => {
                let call_result = EvaluationObject::Empty;

                let m = hashbrown::HashMap::new();
                let global_scope = match global {
                    Some(m) => m,
                    None => Rc::new(RefCell::new(m)),
                };

                let global_scope_inner = global_scope.borrow();
                // use the global_scope to tell if the function is called outside `main` or another function
                if global_scope_inner.is_empty() {
                    error!(
                        "A function can only be called inside `@main` or inside another function",
                    );
                    return call_result;
                }

                let empty_params = Vec::new();
                let call_params = match &self.call_params {
                    Some(p) => p,
                    None => &empty_params,
                };

                let scope_inner = scope_clone.borrow();
                let retrieve_function_def = |from_global: bool, function_name: &str| {
                    if from_global {
                        global_scope_inner.get(function_name).unwrap()
                    } else {
                        scope_inner.get(function_name).unwrap()
                    }
                };

                // closure responsible for reconstructing the function from definition object and executing it
                let function_construct = |from_global: bool, function_name: &str| {
                    let func_def = retrieve_function_def(from_global, function_name);
                    let func_def = func_def.as_function().unwrap();

                    let func_params = match &func_def.func_params {
                        Some(p) => p,
                        None => &empty_params,
                    };

                    if call_params.len() != func_params.len() {
                        error!(
                            "Unexpected number of arguments for function `{}`",
                            function_name
                        );
                        return EvaluationObject::Empty;
                    }

                    // check that the function body is not empty. Otherwise print an error
                    if func_def.block_children.is_none() {
                        error!("Function body is empty. Nothing to run`{}`", function_name);
                        return EvaluationObject::Empty;
                    }

                    if func_def.block_children.as_ref().unwrap().is_empty() {
                        error!("Function body is empty. Nothing to run`{}`", function_name);
                        return EvaluationObject::Empty;
                    }

                    let mut params = Vec::new();
                    for param in zip(func_params, call_params) {
                        params.push(param);
                    }

                    let mut local_binding_resolver = hashbrown::HashMap::new();

                    // add params to local_binding_resolver
                    for (from_func, from_call) in params {
                        if from_func.is_right() == from_call.is_right() {
                            let from_call = from_call.clone();
                            let call_node =
                                from_call.right().unwrap().as_ty_node().unwrap().clone();

                            // use the call node to retrieve the actual value from scope
                            let key = call_node.variable_name.unwrap_or_default();
                            let value = scope_inner.get(&key).unwrap_or(&EvaluationObject::Empty);
                            // insert in function scope
                            local_binding_resolver.insert(key, value.clone());
                        }

                        if from_func.is_right() == from_call.is_left() {
                            let func_call = from_func.clone();
                            let func_node =
                                func_call.right().unwrap().as_ty_node().unwrap().clone();

                            let from_call = from_call.clone();
                            let call_lit = from_call.left().unwrap();
                            let call_lit = match call_lit {
                                LiteralObjects::ObjIntergerValue(int) => {
                                    let int_lit = int.value.unwrap();
                                    EvaluationObject::Integer(int_lit)
                                }
                                LiteralObjects::ObjBooleanValue(bool) => {
                                    let bool_lit = bool.value.unwrap();
                                    EvaluationObject::Boolean(bool_lit)
                                }
                                LiteralObjects::ObjStringValue(string) => {
                                    let string_lit = string.value.as_ref().unwrap();
                                    EvaluationObject::String(string_lit.clone())
                                }
                            };

                            // insert in function scope
                            let key = func_node.variable_name.unwrap_or_default();
                            local_binding_resolver.insert(key, call_lit);
                        }
                    }

                    // evaluate the function itself
                    evaluate_function(
                        func_def.clone(),
                        Rc::new(RefCell::new(local_binding_resolver)),
                    )
                };

                // retrieve the function definition from global scope
                match scope_inner.get(self.variable_name.as_ref().unwrap()) {
                    Some(_func) => function_construct(false, self.variable_name.as_ref().unwrap()),
                    None => match global_scope_inner.get(self.variable_name.as_ref().unwrap()) {
                        Some(_) => function_construct(true, self.variable_name.as_ref().unwrap()),
                        None => {
                            error!(
                                "No function named `{}` found in scope",
                                self.variable_name.as_ref().unwrap()
                            );
                            call_result
                        }
                    },
                }
            }
            IdentifierKind::MAIN => {
                let mut main_result = EvaluationObject::Empty;
                let local_binding_resolver = hashbrown::HashMap::new();
                let local_binding_resolver = Rc::new(RefCell::new(local_binding_resolver));

                let program = self
                    .block_children
                    .as_ref()
                    .unwrap()
                    .get(0)
                    .unwrap()
                    .as_ty_program()
                    .unwrap()
                    .clone();

                let program_items = program.body;
                for item in program_items {
                    match item {
                        Objects::TyNode(node) => {
                            let kind = node.identifier_kind.unwrap();
                            match kind {
                                IdentifierKind::ASSIGN
                                | IdentifierKind::PRINT
                                | IdentifierKind::RETURN => {
                                    main_result = node
                                        .eval(local_binding_resolver.clone(), Some(scope.clone()));
                                }
                                _ => todo!("missing impl for {:?}", kind),
                            }
                        }
                        _ => unreachable!(),
                    }
                }

                main_result
            }

            IdentifierKind::PRINT => {
                let params = self.call_params.as_ref().unwrap().first().unwrap();
                let scope_inner = scope_clone.borrow();

                match params {
                    Left(lit) => match lit {
                        LiteralObjects::ObjIntergerValue(i) => {
                            println!("{:?}", i.value.unwrap_or_default());
                        }
                        LiteralObjects::ObjBooleanValue(b) => {
                            println!("{:?}", b.value.unwrap_or_default());
                        }
                        LiteralObjects::ObjStringValue(s) => {
                            println!("{:?}", s.value.clone());
                        }
                    },
                    Right(obj) => {
                        let node = obj.as_ty_node().unwrap();
                        let kind = node.identifier_kind.unwrap();

                        match kind {
                            IdentifierKind::CALLER => {
                                let value = node.eval(scope, global);
                                println!("{}", value);
                            }
                            _ => {
                                let variable_name = node.variable_name.as_ref().unwrap();
                                let value = scope_inner.get(variable_name).unwrap();
                                println!("{}", value);
                            }
                        }
                    }
                }

                EvaluationObject::Empty
            }
            IdentifierKind::BLOCK => todo!(),
            IdentifierKind::HASH => todo!(),
            IdentifierKind::BANG => todo!(),
            IdentifierKind::LT => todo!(),
            IdentifierKind::GT => todo!(),
            IdentifierKind::EQ => todo!(),
            IdentifierKind::NOTEQ => todo!(),
            IdentifierKind::GTOREQ => todo!(),
            IdentifierKind::LTOREQ => todo!(),
            IdentifierKind::AND => todo!(),
            IdentifierKind::OR => todo!(),
            IdentifierKind::LAND => todo!(),
            IdentifierKind::LOR => todo!(),

            IdentifierKind::ELSE => todo!(),
            IdentifierKind::FORMAT => todo!(),
            IdentifierKind::ARRAY => todo!(),

            _ => EvaluationObject::Empty,
        }
    }
}

fn left_or_right(
    object: &Either<LiteralObjects, Box<Objects>>,
    scope: Rc<RefCell<ScopeBindingResolver>>,
    global: Option<Rc<RefCell<ScopeBindingResolver>>>,
) -> EvaluationObject {
    match object {
        Left(left) => match left {
            LiteralObjects::ObjIntergerValue(int) => {
                let int_lit = int.value.unwrap();
                EvaluationObject::Integer(int_lit)
            }
            LiteralObjects::ObjBooleanValue(bool) => {
                let bool_lit = bool.value.unwrap();
                EvaluationObject::Boolean(bool_lit)
            }
            LiteralObjects::ObjStringValue(string) => {
                let string_lit = string.value.as_ref().unwrap();
                EvaluationObject::String(string_lit.clone())
            }
        },
        Right(right) => right.eval(scope, global),
    }
}

fn evaluate_function(func: Node, scope: Rc<RefCell<ScopeBindingResolver>>) -> EvaluationObject {
    let mut main_result = EvaluationObject::Empty;

    for item in func.block_children.unwrap() {
        main_result = item.eval(scope.clone(), None)
    }

    main_result
}

#[cfg(test)]
mod evaluator_tests {

    use lexer::lexer::Lexer;

    use super::*;

    #[test]
    fn should_evaluate_function_call() {
        let lx = Lexer::new(String::from(
            "
        let add @int = fn(x @int, y @int){
            return x + y;
        };

        @main fn(){
            let result0 @int = add(10,20);
            print(result0);
        }@end;
        ",
        ));

        let global_binding_resolver = hashbrown::HashMap::new();
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_evaluate1.json"));
        let mut evaluator = Evaluator::new(parser);
        evaluator.repl_evaluate_program(Rc::new(RefCell::new(global_binding_resolver)));

        assert!(res.is_ok());
    }
}
