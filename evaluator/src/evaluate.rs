use std::cell::RefCell;
use std::fmt;
use std::iter::zip;
use std::rc::Rc;

use colored::*;
use errors::errors::{KarisError, KarisErrorType};
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
    Array(Vec<EvaluationObject>),
    ReturnValue(Rc<EvaluationObject>),
    Function(Node),

    AlternateCondition,
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
            EvaluationObject::Array(list) => {
                let mut items = Vec::new();

                for item in list.iter() {
                    let m = format!("{}", item);
                    items.push(m);
                }

                write!(f, "{:?}", items)
            }
            EvaluationObject::Function(_)
            | EvaluationObject::Empty
            | EvaluationObject::AlternateCondition => Ok(()),
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
    ) -> Result<EvaluationObject, KarisError>;
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
                // if let Ok(EvaluationObject::ReturnValue(res)) = program.eval(scope, None) {
                //     println!("{}", res)
                // }

                match program.eval(scope, None) {
                    Ok(EvaluationObject::Integer(val)) => println!("{:?}", val),
                    Ok(EvaluationObject::String(val)) => println!("{:?}", val),
                    Ok(EvaluationObject::Boolean(val)) => println!("{:?}", val),
                    Ok(_) => {}
                    Err(_) => todo!(),
                }
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
    ) -> Result<EvaluationObject, KarisError> {
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
    ) -> Result<EvaluationObject, KarisError> {
        let mut evaluation_result = Ok(EvaluationObject::Empty);

        for item in self.body.iter() {
            if let Ok(val) = item.eval(scope.clone(), global.clone()) {
                match val {
                    EvaluationObject::ReturnValue(r) => {
                        let v = r.as_ref().clone();
                        evaluation_result = Ok(v)
                    }
                    _ => evaluation_result = Ok(val),
                }
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
    ) -> Result<EvaluationObject, KarisError> {
        let kind = self.identifier_kind.unwrap();

        let scope_clone = scope.clone();

        match kind {
            IdentifierKind::INTLITERAL => {
                let lit = self.left_child.as_ref().unwrap().as_ref().left().unwrap();
                let int_value = lit.as_obj_interger_value().unwrap();
                let int_lit = int_value.value.unwrap();
                Ok(EvaluationObject::Integer(int_lit))
            }
            IdentifierKind::STRINGLITERAL => {
                let lit = self.left_child.as_ref().unwrap().as_ref().left().unwrap();
                let string_value = lit.as_obj_string_value().unwrap();
                let string_lit = string_value.value.as_ref().unwrap();
                Ok(EvaluationObject::String(string_lit.clone()))
            }
            IdentifierKind::BOOLEANLITERAL => {
                let lit = self.left_child.as_ref().unwrap().as_ref().left().unwrap();
                let bool_value = lit.as_obj_boolean_value().unwrap();
                let bool_lit = bool_value.value.unwrap();
                Ok(EvaluationObject::Boolean(bool_lit))
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
                if let Ok(rhs) = left_or_right(rhs, scope, global) {
                    let mut scope_inner_mut = scope_clone.borrow_mut();
                    scope_inner_mut.insert(binding_key.to_string(), rhs);
                }

                Ok(EvaluationObject::Empty)
            }

            IdentifierKind::VARIABLE => {
                let variable = self.variable_name.as_ref().unwrap();
                let scope_inner = scope_clone.borrow();

                if let Some(val) = scope_inner.get(variable) {
                    return Ok(val.clone());
                }
                Ok(EvaluationObject::Empty)
            }

            IdentifierKind::PLUS => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, scope.clone(), global.clone());
                let left = match left.unwrap() {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, scope, global);
                let right = match right.unwrap() {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let sum = left + right;
                Ok(EvaluationObject::Integer(sum))
            }
            IdentifierKind::MINUS => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, scope.clone(), global.clone());
                let left = match left.unwrap() {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, scope, global);
                let right = match right.unwrap() {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let diff = left - right;
                Ok(EvaluationObject::Integer(diff))
            }
            IdentifierKind::ASTERISK => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, scope.clone(), global.clone());
                let left = match left.unwrap() {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, scope, global);
                let right = match right.unwrap() {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let product = left * right;
                Ok(EvaluationObject::Integer(product))
            }
            IdentifierKind::SLASH => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, scope.clone(), global.clone());
                let left = match left.unwrap() {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, scope, global);
                let right = match right.unwrap() {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let division = left / right;
                Ok(EvaluationObject::Integer(division))
            }
            IdentifierKind::MODULUS => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, scope.clone(), global.clone());
                let left = match left.unwrap() {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, scope, global);
                let right = match right.unwrap() {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let modulus = left % right;
                Ok(EvaluationObject::Integer(modulus))
            }

            IdentifierKind::GT => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, scope.clone(), global.clone());
                let left = match left.unwrap() {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, scope, global);
                let right = match right.unwrap() {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let gt = left > right;
                Ok(EvaluationObject::Boolean(gt))
            }

            IdentifierKind::GTOREQ => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, scope.clone(), global.clone());
                let left = match left.unwrap() {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, scope, global);
                let right = match right.unwrap() {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let gteq = left >= right;
                Ok(EvaluationObject::Boolean(gteq))
            }

            IdentifierKind::LT => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, scope.clone(), global.clone());
                let left = match left.unwrap() {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, scope, global);
                let right = match right.unwrap() {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let lt = left < right;
                Ok(EvaluationObject::Boolean(lt))
            }

            IdentifierKind::LTOREQ => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, scope.clone(), global.clone());
                let left = match left.unwrap() {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, scope, global);
                let right = match right.unwrap() {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let lteq = left <= right;
                Ok(EvaluationObject::Boolean(lteq))
            }

            IdentifierKind::EQ => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, scope.clone(), global.clone());
                let left = match left.unwrap() {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, scope, global);
                let right = match right.unwrap() {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let eq = left == right;
                Ok(EvaluationObject::Boolean(eq))
            }

            IdentifierKind::NOTEQ => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, scope.clone(), global.clone());
                let left = match left.unwrap() {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, scope, global);
                let right = match right.unwrap() {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let noteq = left != right;
                Ok(EvaluationObject::Boolean(noteq))
            }

            IdentifierKind::AND => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, scope.clone(), global.clone());
                let left = match left.unwrap() {
                    EvaluationObject::Boolean(b) => b,
                    _ => false,
                };

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, scope, global);
                let right = match right.unwrap() {
                    EvaluationObject::Boolean(b) => b,
                    _ => false,
                };

                let and = left && right;
                Ok(EvaluationObject::Boolean(and))
            }

            IdentifierKind::LAND => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, scope.clone(), global.clone());
                let left = match left.unwrap() {
                    EvaluationObject::Boolean(b) => b,
                    _ => false,
                };

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, scope, global);
                let right = match right.unwrap() {
                    EvaluationObject::Boolean(b) => b,
                    _ => false,
                };

                let land = left & right;
                Ok(EvaluationObject::Boolean(land))
            }

            IdentifierKind::OR => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, scope.clone(), global.clone());
                let left = match left.unwrap() {
                    EvaluationObject::Boolean(b) => b,
                    _ => false,
                };

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, scope, global);
                let right = match right.unwrap() {
                    EvaluationObject::Boolean(b) => b,
                    _ => false,
                };

                let or = left || right;
                Ok(EvaluationObject::Boolean(or))
            }

            IdentifierKind::LOR => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, scope.clone(), global.clone());
                let left = match left.unwrap() {
                    EvaluationObject::Boolean(b) => b,
                    _ => false,
                };

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, scope, global);
                let right = match right.unwrap() {
                    EvaluationObject::Boolean(b) => b,
                    _ => false,
                };

                let or = left | right;
                Ok(EvaluationObject::Boolean(or))
            }

            IdentifierKind::BANG => {
                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, scope, global);
                let right = match right.unwrap() {
                    EvaluationObject::Boolean(b) => b,
                    _ => false,
                };
                let negate = !right;
                Ok(EvaluationObject::Boolean(negate))
            }

            IdentifierKind::GROUPING => {
                let right = self.right_child.as_ref().unwrap();
                left_or_right(right, scope, global.clone())
            }
            IdentifierKind::LPAREN => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, scope.clone(), global.clone());
                let left = match left.unwrap() {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, scope, global);
                let right = match right.unwrap() {
                    EvaluationObject::Integer(int) => int,
                    _ => 0,
                };

                let mul = left * right;
                Ok(EvaluationObject::Integer(mul))
            }
            IdentifierKind::FUNCTION => Ok(EvaluationObject::Function(self.clone())),
            IdentifierKind::CALLER => {
                let m = hashbrown::HashMap::new();
                let global_scope = match global {
                    Some(m) => m,
                    None => Rc::new(RefCell::new(m)),
                };

                let global_scope_inner = global_scope.borrow();
                // use the global_scope to tell if the function is called outside `main` or another function
                if global_scope_inner.is_empty() {
                    let err:Result<EvaluationObject, KarisError> =  Err(KarisError {
                        error_type: KarisErrorType::MissingFunctionInScope,
                        message: "A function can only be called inside `@main` or inside another function".to_string(),
                    });
                    panic!("{:?}", err)
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
                        let err: Result<EvaluationObject, KarisError> = Err(KarisError {
                            error_type: KarisErrorType::IncorrectFunctionCall,
                            message: format!("Unexpected number of arguments for function `{}`. Wanted {} but got {}. If you passed an expression, bind it to a varible first",
                                function_name, func_params.len(),call_params.len()
                            ),
                        });
                        panic!("{:?}", err)
                    }

                    // check that the function body is not empty. Otherwise print an error
                    if func_def.block_children.is_none() {
                        let err: Result<EvaluationObject, KarisError> = Err(KarisError {
                            error_type: KarisErrorType::IncorrectFunctionCall,
                            message: format!(
                                "Function body is empty. Nothing to run`{}`",
                                function_name
                            ),
                        });
                        panic!("{:?}", err)
                    }

                    if func_def.block_children.as_ref().unwrap().is_empty() {
                        let err: Result<EvaluationObject, KarisError> = Err(KarisError {
                            error_type: KarisErrorType::IncorrectFunctionCall,
                            message: format!(
                                "Function body is empty. Nothing to run`{}`",
                                function_name
                            ),
                        });
                        panic!("{:?}", err)
                    }

                    let mut params = Vec::new();
                    for param in zip(func_params, call_params) {
                        params.push(param);
                    }

                    let mut local_binding_resolver = hashbrown::HashMap::new();

                    // add params to local_binding_resolver
                    for (from_func, from_call) in params {
                        if from_func.is_right() == from_call.is_right() {
                            let from_func_param = from_func.clone();
                            let from_call_param = from_call.clone();

                            let func_node = from_func_param
                                .right()
                                .unwrap()
                                .as_ty_node()
                                .unwrap()
                                .clone();

                            let call_node = from_call_param
                                .right()
                                .unwrap()
                                .as_ty_node()
                                .unwrap()
                                .clone();

                            let func_key = func_node.variable_name.unwrap_or_default();
                            // use the call node to retrieve the actual value from scope
                            let call_key = call_node.variable_name.unwrap_or_default();
                            let call_value = scope_inner
                                .get(&call_key)
                                .unwrap_or(&EvaluationObject::Empty);
                            // insert in function scope
                            local_binding_resolver.insert(func_key, call_value.clone());
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

                    let global_scope_closure = global_scope.clone();
                    // evaluate the function itself
                    evaluate_function(
                        func_def.clone(),
                        Rc::new(RefCell::new(local_binding_resolver)),
                        Some(global_scope_closure),
                    )
                };

                // retrieve the function definition from global scope
                match scope_inner.get(self.variable_name.as_ref().unwrap()) {
                    Some(_func) => function_construct(false, self.variable_name.as_ref().unwrap()),
                    None => match global_scope_inner.get(self.variable_name.as_ref().unwrap()) {
                        Some(_) => function_construct(true, self.variable_name.as_ref().unwrap()),
                        None => Err(KarisError {
                            error_type: KarisErrorType::MissingFunctionInScope,
                            message: format!(
                                "No function named `{}` found in scope",
                                self.variable_name.as_ref().unwrap()
                            ),
                        }),
                    },
                }
            }
            IdentifierKind::MAIN => {
                let mut main_result = Ok(EvaluationObject::Empty);
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
                                _ => unreachable!("invalid syntax"),
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

                let mut message = String::new();

                match params {
                    Left(lit) => match lit {
                        LiteralObjects::ObjIntergerValue(i) => {
                            let msg = format!("{:?}", i.value.unwrap_or_default());
                            message = msg;
                        }
                        LiteralObjects::ObjBooleanValue(b) => {
                            let msg = format!("{:?}", b.value.unwrap_or_default());
                            message = msg;
                        }
                        LiteralObjects::ObjStringValue(s) => {
                            let msg = format!("{:?}", s.value.clone());
                            message = msg;
                        }
                    },
                    Right(obj) => {
                        let node = obj.as_ty_node().unwrap();
                        let kind = node.identifier_kind.unwrap();

                        match kind {
                            IdentifierKind::CALLER => {
                                if let Ok(value) = node.eval(scope, global) {
                                    let msg = format!("{}", value);
                                    message = msg;
                                }
                            }
                            _ => {
                                let variable_name = node.variable_name.as_ref().unwrap();
                                if let Some(value) = scope_inner.get(variable_name) {
                                    let msg = format!("{}", value);
                                    message = msg;
                                } else {
                                    error!("missing variable {}", variable_name);
                                }
                            }
                        }
                    }
                }

                // we do the actual printing here to std::out
                println!("{}", message);

                Ok(EvaluationObject::Empty)
            }
            IdentifierKind::IF | IdentifierKind::ELSE => {
                let condition_statement_result = Ok(EvaluationObject::AlternateCondition);

                // clone the scope to create an new local binding resolver scope. If the body contains a variables with the same name,
                // they will be overwritten (block scoped)
                let local_binding_resolver = scope.clone();

                // get the condition first and evaluate it. It should always return a boolean
                let condition = self.right_child.as_ref().unwrap();
                let condition = left_or_right(condition, scope, global);
                let condition = match condition.unwrap() {
                    EvaluationObject::Boolean(b) => b,
                    _ => false,
                };

                let program_func_worker = |program: Program| {
                    let mut result = Ok(EvaluationObject::Empty);

                    let program_items = program.body;
                    for item in program_items {
                        match item {
                            Objects::TyNode(node) => {
                                let kind = node.identifier_kind.unwrap();
                                match kind {
                                    IdentifierKind::ASSIGN | IdentifierKind::PRINT => {
                                        result = node.eval(
                                            local_binding_resolver.clone(),
                                            Some(local_binding_resolver.clone()), // points to the global binding scope
                                        );
                                    }
                                    IdentifierKind::RETURN => {
                                        result = node.eval(
                                            local_binding_resolver.clone(),
                                            Some(local_binding_resolver.clone()), // points to the global binding scope
                                        );
                                        break;
                                    }
                                    _ => {}
                                }
                            }
                            _ => unreachable!(),
                        }
                    }
                    result
                };

                if condition {
                    let program = self
                        .block_children
                        .as_ref()
                        .unwrap()
                        .get(0)
                        .unwrap()
                        .as_ty_program()
                        .unwrap()
                        .clone();

                    return program_func_worker(program);
                }

                let has_alternate = self.alternate.as_ref().is_some();
                if !condition && has_alternate {
                    let alternate = self.alternate.as_ref().unwrap();
                    let alternate = alternate.as_ty_node().unwrap();

                    let program = alternate
                        .block_children
                        .as_ref()
                        .unwrap()
                        .get(0)
                        .unwrap()
                        .as_ty_program()
                        .unwrap()
                        .clone();

                    return program_func_worker(program);
                }

                condition_statement_result
            }

            IdentifierKind::ARRAY => {
                let mut result = Vec::new();
                for child in self.block_children.as_ref().unwrap() {
                    if let Ok(res) = child.eval(scope.clone(), global.clone()) {
                        result.push(res);
                    }
                }

                Ok(EvaluationObject::Array(result))
            }
            IdentifierKind::RETURN => {
                let right = self.right_child.as_ref().unwrap();
                let resp = left_or_right(right, scope, global.clone());
                match resp {
                    Ok(res) => Ok(EvaluationObject::ReturnValue(Rc::new(res))),
                    Err(_) => unreachable!(),
                }
            }

            _ => Ok(EvaluationObject::Empty),
        }
    }
}

fn left_or_right(
    object: &Either<LiteralObjects, Box<Objects>>,
    scope: Rc<RefCell<ScopeBindingResolver>>,
    global: Option<Rc<RefCell<ScopeBindingResolver>>>,
) -> Result<EvaluationObject, KarisError> {
    match object {
        Left(left) => match left {
            LiteralObjects::ObjIntergerValue(int) => {
                let int_lit = int.value.unwrap();
                Ok(EvaluationObject::Integer(int_lit))
            }
            LiteralObjects::ObjBooleanValue(bool) => {
                let bool_lit = bool.value.unwrap();
                Ok(EvaluationObject::Boolean(bool_lit))
            }
            LiteralObjects::ObjStringValue(string) => {
                let string_lit = string.value.as_ref().unwrap();
                Ok(EvaluationObject::String(string_lit.clone()))
            }
        },
        Right(right) => right.eval(scope, global),
    }
}

fn evaluate_function(
    func: Node,
    scope: Rc<RefCell<ScopeBindingResolver>>,
    global: Option<Rc<RefCell<ScopeBindingResolver>>>,
) -> Result<EvaluationObject, KarisError> {
    let mut main_result = Ok(EvaluationObject::Empty);

    for item in func.block_children.unwrap() {
        if let Ok(res) = item.eval(scope.clone(), global.clone()) {
            match res {
                EvaluationObject::ReturnValue(result) => {
                    let v = result.as_ref().clone();
                    main_result = Ok(v);
                    break;
                }
                _ => main_result = Ok(res),
            }
        }
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

    #[test]
    fn should_evaluate_function_call2() {
        let lx = Lexer::new(String::from(
            "
        let sub @int = fn(x @int, y @int){
            return x - y;
        };

        @main fn(){
            let twenty @int = 20;
            let result0 @int = sub(10,twenty);
            print(result0);
        }@end;
        ",
        ));

        let global_binding_resolver = hashbrown::HashMap::new();
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_evaluate1b.json"));
        let mut evaluator = Evaluator::new(parser);
        evaluator.repl_evaluate_program(Rc::new(RefCell::new(global_binding_resolver)));

        assert!(res.is_ok());
    }

    #[test]
    fn should_evaluate_function_recursive_call() {
        let lx = Lexer::new(String::from(
            "
        let factorial @int = fn(n @int){
            if n == 1 {
                return 1;
            };

            let n1 @int = n - 1;
            return n * factorial(n1);
        };

        @main fn(){
            let result0 @int = factorial(2);
            print(result0);
        }@end;
        ",
        ));

        let global_binding_resolver = hashbrown::HashMap::new();
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_evaluate2.json"));
        let mut evaluator = Evaluator::new(parser);
        evaluator.repl_evaluate_program(Rc::new(RefCell::new(global_binding_resolver)));

        assert!(res.is_ok());
    }

    #[test]
    fn should_evaluate_function_with_multi_conditions() {
        let lx = Lexer::new(String::from(
            "
        let minmax_or_product @int = fn(x @int, y @int){
                if x < y{
                   return x + y;
                }else x == y {
                    return x - y;
                } else {
                    return x * y;
                };
        };

        @main fn(){
            let result0 @int = minmax_or_product(5,10);
            print(result0);
        }@end;
        ",
        ));

        let global_binding_resolver = hashbrown::HashMap::new();
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_evaluate3.json"));
        let mut evaluator = Evaluator::new(parser);
        evaluator.repl_evaluate_program(Rc::new(RefCell::new(global_binding_resolver)));

        assert!(res.is_ok());
    }

    #[test]
    fn should_evaluate_function_with_multi_conditions2() {
        let lx = Lexer::new(String::from(
            "
            let minmax_or_product @int = fn(x @int, y @int){
                if x < y{
                   return x + y;
                }else x > y{
                    return x - y;
                };

                return x * y;
            };

            let factorial @int = fn(n @int){
                if n == 1 {
                    return 1;
                };

                let n1 @int = n - 1;
                return n * factorial(n1);
            };

            let fibonacci @int = fn(n @int){
                if n == 0 {
                    return 0;
                };

                if n == 1 || n == 2 {
                    return 1;
                };

                let n0 @int = n - 1;
                let n1 @int = n - 2;
                return fibonacci(n0) + fibonacci(n1);
            };

            @main fn(){
                let result1 @int = minmax_or_product(5,10);
                let result2 @int = factorial(5);
                let result3 @int = fibonacci(3);

                print(result1);
                print(result2);
                print(result3);
            }@end;
        ",
        ));

        let global_binding_resolver = hashbrown::HashMap::new();
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_evaluate4.json"));
        let mut evaluator = Evaluator::new(parser);
        evaluator.repl_evaluate_program(Rc::new(RefCell::new(global_binding_resolver)));

        assert!(res.is_ok());
    }

    #[test]
    fn should_evaluate_array() {
        let lx = Lexer::new(String::from(
            "
        let numbers [ @int ] = [ 1,2,3,4,5 ];

        ",
        ));

        let global_binding_resolver = hashbrown::HashMap::new();
        let mut parser = Parser::new(lx);
        let res = parser.parse(Some("should_evaluate5.json"));
        let mut evaluator = Evaluator::new(parser);
        evaluator.repl_evaluate_program(Rc::new(RefCell::new(global_binding_resolver)));

        assert!(res.is_ok());
    }
}
