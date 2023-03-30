use std::{cell::RefCell, process, rc::Rc};

use either::Either::{self, Left, Right};
use errors::errors::{KarisError, KarisErrorType};
use lexer::tokens::IdentifierKind;
use parser::objects::{LiteralObjects, Node, Objects, Program};

use crate::{
    compile::CompileWorker,
    defs::{CallerParamType, OpCode, SymbolScope},
    objects::CompileObject,
};

pub trait Compiler {
    fn compile(
        &self,
        worker: Rc<RefCell<&CompileWorker>>,
        scope: SymbolScope,
        scope_id: [u8; 2],
    ) -> Option<Vec<Vec<u8>>>;
}

impl Compiler for Objects {
    fn compile(
        &self,
        worker: Rc<RefCell<&CompileWorker>>,
        scope: SymbolScope,
        scope_id: [u8; 2],
    ) -> Option<Vec<Vec<u8>>> {
        match self {
            Self::TyProgram(program) => program.compile(worker.clone(), scope, scope_id),
            Self::TyNode(node) => node.compile(worker.clone(), scope, scope_id),
            _ => unreachable!("cannot compile UNKNOWN or CONSUMABLE"),
        }
    }
}

impl Compiler for Program {
    fn compile(
        &self,
        worker: Rc<RefCell<&CompileWorker>>,
        scope: SymbolScope,
        scope_id: [u8; 2],
    ) -> Option<Vec<Vec<u8>>> {
        for item in self.body.iter() {
            item.compile(worker.clone(), scope.clone(), scope_id);
        }

        None
    }
}

impl Compiler for Node {
    fn compile(
        &self,
        worker: Rc<RefCell<&CompileWorker>>,
        scope: SymbolScope,
        scope_id: [u8; 2],
    ) -> Option<Vec<Vec<u8>>> {
        let kind = self.identifier_kind.unwrap();

        match kind {
            IdentifierKind::INTLITERAL => {
                let lit = self.left_child.as_ref().unwrap().as_ref().left().unwrap();
                let int_value = lit.as_obj_interger_value().unwrap();
                let int_lit = int_value.value.unwrap();

                let obj = CompileObject::Interger(int_lit);
                let wrk = worker.borrow();

                let instructions = wrk.add_constant(scope, scope_id, obj);
                Some(vec![instructions])
            }

            IdentifierKind::STRINGLITERAL => {
                let lit = self.left_child.as_ref().unwrap().as_ref().left().unwrap();
                let string_value = lit.as_obj_string_value().unwrap();
                let string_lit = string_value.value.as_ref().unwrap();

                let obj = CompileObject::String(string_lit.to_string());
                let wrk = worker.borrow();

                let instructions = wrk.add_constant(scope, scope_id, obj);
                Some(vec![instructions])
            }

            IdentifierKind::BOOLEANLITERAL => {
                let lit = self.left_child.as_ref().unwrap().as_ref().left().unwrap();
                let bool_value = lit.as_obj_boolean_value().unwrap();
                let bool_lit = bool_value.value.unwrap();

                let obj = CompileObject::Boolean(bool_lit);
                let wrk = worker.borrow();

                let instructions = wrk.add_constant(scope, scope_id, obj);
                Some(vec![instructions])
            }

            IdentifierKind::PLUS => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, worker.clone(), scope.clone(), scope_id).unwrap();
                let left = &left[0];

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, worker.clone(), scope, scope_id).unwrap();
                let right = &right[0];

                let wrk = worker.borrow();
                let instructions = wrk.add_infix(OpCode::OpAdd, left.to_vec(), right.to_vec());
                Some(vec![instructions])
            }

            IdentifierKind::MINUS => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, worker.clone(), scope.clone(), scope_id).unwrap();
                let left = &left[0];

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, worker.clone(), scope, scope_id).unwrap();
                let right = &right[0];

                let wrk = worker.borrow();
                let instructions = wrk.add_infix(OpCode::OpMinus, left.to_vec(), right.to_vec());
                Some(vec![instructions])
            }

            IdentifierKind::ASTERISK => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, worker.clone(), scope.clone(), scope_id).unwrap();
                let left = &left[0];

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, worker.clone(), scope, scope_id).unwrap();
                let right = &right[0];

                let wrk = worker.borrow();
                let instructions = wrk.add_infix(OpCode::OpMultiply, left.to_vec(), right.to_vec());
                Some(vec![instructions])
            }

            IdentifierKind::SLASH => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, worker.clone(), scope.clone(), scope_id).unwrap();
                let left = &left[0];

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, worker.clone(), scope, scope_id).unwrap();
                let right = &right[0];

                let wrk = worker.borrow();
                let instructions = wrk.add_infix(OpCode::OpDivide, left.to_vec(), right.to_vec());
                Some(vec![instructions])
            }

            IdentifierKind::MODULUS => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, worker.clone(), scope.clone(), scope_id).unwrap();
                let left = &left[0];

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, worker.clone(), scope, scope_id).unwrap();
                let right = &right[0];

                let wrk = worker.borrow();
                let instructions = wrk.add_infix(OpCode::OpModulus, left.to_vec(), right.to_vec());
                Some(vec![instructions])
            }

            IdentifierKind::VARIABLE => {
                let variable_name = self.variable_name.as_ref().unwrap();

                let variable_name_as_bytes = variable_name.as_bytes().to_vec();
                let wrk = worker.borrow();
                let instructions = wrk.generate_opcode_with_scope_and_vec_parameter(
                    OpCode::OpGetVariable,
                    scope,
                    scope_id,
                    variable_name_as_bytes,
                );
                Some(vec![instructions])
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
                let binding_key_as_bytes = binding_key.as_bytes().to_vec();

                let rhs = self.right_child.as_ref().unwrap();
                if let Some(instructions) =
                    left_or_right(rhs, worker.clone(), scope.clone(), scope_id)
                {
                    let wrk = worker.borrow();
                    wrk.add_symbol(binding_key_as_bytes.clone(), instructions);

                    wrk.add_variable(scope, scope_id, binding_key_as_bytes);
                }
                None
            }

            IdentifierKind::FUNCTION => {
                let empty_params = Vec::new();
                let func_params = match self.func_params.as_ref() {
                    Some(p) => p,
                    None => &empty_params,
                };

                let empty_function_err: Result<(), KarisError> = Err(KarisError {
                    error_type: KarisErrorType::MissingFunctionBody,
                    message: "Function body is empty. Nothing to run".to_string(),
                });

                // check that the function body is not empty. Otherwise end the compilation
                if self.block_children.is_none() {
                    eprintln!("{:?}", empty_function_err);
                    process::exit(0x0100)
                }

                if self.block_children.as_ref().unwrap().is_empty() {
                    eprintln!("{:?}", empty_function_err);
                    process::exit(0x0100)
                }

                let mut function_instructions = Vec::new();
                let wrk = worker.borrow();
                let func_scope_id = wrk.generate_scope_id_from_scope(SymbolScope::Local);

                wrk.add_function(func_scope_id);

                // for each function parameter, we add an instruction to access them
                for param in func_params {
                    // check that literals are not in the function definition. It's an invalid syntax
                    if param.is_left() {
                        eprintln!("Invalid parameter type: {:?}", param);
                        process::exit(0x0100)
                    }

                    // param will always be a `Objects` (RIGHT of Either)
                    let obj = param.clone().right().as_ref().unwrap().clone();

                    if let Some(instructions) = function_or_caller_object_param_access_instructions(
                        &obj,
                        worker.clone(),
                        SymbolScope::Local,
                        func_scope_id,
                        OpCode::OpGetFunctionParameter,
                    ) {
                        function_instructions.push(instructions);
                    }
                }

                let block_children = self.block_children.as_ref().unwrap();
                for body_item in block_children.iter() {
                    let inst = body_item.compile(worker.clone(), SymbolScope::Local, func_scope_id);
                    let inst = inst.unwrap();
                    for i in inst.iter() {
                        function_instructions.push(i.clone())
                    }
                }

                Some(function_instructions)
            }

            IdentifierKind::RETURN => {
                let right = self.right_child.as_ref().unwrap();
                let instructions =
                    left_or_right(right, worker.clone(), scope.clone(), scope_id).unwrap();

                let wrk = worker.borrow();
                wrk.add_return(scope, scope_id, instructions.clone());

                Some(instructions)
            }

            IdentifierKind::CALLER => {
                let func_name = self.variable_name.as_ref().unwrap();
                let func_name_as_bytes = func_name.as_bytes().to_vec();

                let wrk = worker.borrow();

                // we check that the function exists in global scope
                if let Some(_function_symbol) = wrk.get_symbol(func_name_as_bytes.clone()) {
                    let mut caller_instructions = Vec::new();

                    let wrk = worker.borrow();
                    let caller_scope_id = wrk.generate_scope_id_from_scope(SymbolScope::Local);
                    wrk.add_caller(scope.clone(), caller_scope_id, func_name_as_bytes);

                    if let Some(caller_params) = &self.call_params {
                        // first we create a caller instructions vector. These vector will store all parameter instructions that
                        // will support when calling the function
                        for param in caller_params.iter() {
                            match param {
                                Left(left) => match left {
                                    LiteralObjects::ObjIntergerValue(int) => {
                                        let int_lit = int.value.unwrap();
                                        let obj = CompileObject::Interger(int_lit);
                                        let wrk = worker.borrow();
                                        let const_location = wrk.append_to_constant_pool(obj);

                                        let mut instructions = vec![
                                            OpCode::OpSetCallerParameter as u8,
                                            scope.clone() as u8,
                                        ];

                                        for id in caller_scope_id {
                                            instructions.push(id);
                                        }

                                        let mut other = vec![
                                            OpCode::OpTerminal as u8,
                                            CallerParamType::Literal as u8,
                                            OpCode::OpTerminal as u8,
                                            const_location,
                                        ];

                                        instructions.append(&mut other);

                                        caller_instructions.push(instructions);
                                    }
                                    LiteralObjects::ObjBooleanValue(bool) => {
                                        let bool_lit = bool.value.unwrap();
                                        let obj = CompileObject::Boolean(bool_lit);
                                        let wrk = worker.borrow();
                                        let const_location = wrk.append_to_constant_pool(obj);

                                        let mut instructions = vec![
                                            OpCode::OpSetCallerParameter as u8,
                                            scope.clone() as u8,
                                        ];

                                        for id in caller_scope_id {
                                            instructions.push(id);
                                        }

                                        let mut other = vec![
                                            OpCode::OpTerminal as u8,
                                            CallerParamType::Literal as u8,
                                            OpCode::OpTerminal as u8,
                                            const_location,
                                        ];

                                        instructions.append(&mut other);

                                        caller_instructions.push(instructions);
                                    }
                                    LiteralObjects::ObjStringValue(string) => {
                                        let string_lit = string.value.as_ref().unwrap();
                                        let obj = CompileObject::String(string_lit.to_string());
                                        let wrk = worker.borrow();

                                        let const_location = wrk.append_to_constant_pool(obj);

                                        let mut instructions = vec![
                                            OpCode::OpSetCallerParameter as u8,
                                            scope.clone() as u8,
                                        ];

                                        for id in caller_scope_id {
                                            instructions.push(id);
                                        }

                                        let mut other = vec![
                                            OpCode::OpTerminal as u8,
                                            CallerParamType::Literal as u8,
                                            OpCode::OpTerminal as u8,
                                            const_location,
                                            OpCode::OpTerminal as u8,
                                        ];

                                        instructions.append(&mut other);

                                        caller_instructions.push(instructions);
                                    }
                                },
                                Right(right) => {
                                    if let Some(insts) =
                                        function_or_caller_object_param_access_instructions(
                                            right,
                                            worker.clone(),
                                            scope.clone(),
                                            caller_scope_id,
                                            OpCode::OpGetCallerParameter,
                                        )
                                    {
                                        let mut instructions = vec![
                                            OpCode::OpSetCallerParameter as u8,
                                            scope.clone() as u8,
                                        ];

                                        for id in caller_scope_id {
                                            instructions.push(id);
                                        }

                                        let mut other = vec![
                                            OpCode::OpTerminal as u8,
                                            CallerParamType::Literal as u8,
                                            OpCode::OpTerminal as u8,
                                        ];

                                        instructions.append(&mut other);

                                        for i in insts {
                                            instructions.push(i);
                                        }

                                        instructions.push(OpCode::OpTerminal as u8);

                                        caller_instructions.push(instructions);
                                    }
                                }
                            };
                        }
                    }

                    Some(caller_instructions)
                } else {
                    eprintln!("function of name {:?} not found in global scope", func_name);
                    process::exit(0x0100)
                }
            }

            IdentifierKind::MAIN => {
                let wrk = worker.borrow();
                let main_scope_id = wrk.generate_scope_id_from_scope(SymbolScope::Local);
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
                            node.compile(worker.clone(), SymbolScope::Local, main_scope_id)
                        }
                        _ => unreachable!(),
                    };
                }

                None
            }

            _ => {
                todo!("implement for {:?}", kind)
            }
        }
    }
}

fn left_or_right(
    object: &Either<LiteralObjects, Box<Objects>>,
    worker: Rc<RefCell<&CompileWorker>>,
    scope: SymbolScope,
    scope_id: [u8; 2],
) -> Option<Vec<Vec<u8>>> {
    match object {
        Left(left) => match left {
            LiteralObjects::ObjIntergerValue(int) => {
                let int_lit = int.value.unwrap();
                let obj = CompileObject::Interger(int_lit);
                let wrk = worker.borrow();
                let instructions = wrk.add_constant(scope, scope_id, obj);
                Some(vec![instructions])
            }
            LiteralObjects::ObjBooleanValue(bool) => {
                let bool_lit = bool.value.unwrap();
                let obj = CompileObject::Boolean(bool_lit);
                let wrk = worker.borrow();
                let instructions = wrk.add_constant(scope, scope_id, obj);
                Some(vec![instructions])
            }
            LiteralObjects::ObjStringValue(string) => {
                let string_lit = string.value.as_ref().unwrap();
                let obj = CompileObject::String(string_lit.to_string());
                let wrk = worker.borrow();
                let instructions = wrk.add_constant(scope, scope_id, obj);
                Some(vec![instructions])
            }
        },
        Right(right) => right.compile(worker, scope, scope_id),
    }
}

fn function_or_caller_object_param_access_instructions(
    obj: &Objects,
    worker: Rc<RefCell<&CompileWorker>>,
    scope: SymbolScope,
    scope_id: [u8; 2],
    opcode: OpCode,
) -> Option<Vec<u8>> {
    match obj {
        Objects::TyNode(node) => {
            let kind = node.identifier_kind.unwrap();
            match kind {
                IdentifierKind::VARIABLE => {
                    let variable_name = node.variable_name.as_ref().unwrap();

                    let variable_name_as_bytes = variable_name.as_bytes().to_vec();
                    let wrk = worker.borrow();
                    let instructions = wrk.generate_opcode_with_scope_and_vec_parameter(
                        opcode,
                        scope,
                        scope_id,
                        variable_name_as_bytes,
                    );
                    Some(instructions)
                }
                _ => {
                    eprintln!("Invalid parameter type: {:?}", kind);
                    process::exit(0x0100)
                }
            }
        }
        _ => {
            eprintln!("Invalid parameter type: {:?}", obj);
            process::exit(0x0100)
        }
    }
}
