use std::{cell::RefCell, process, rc::Rc};

use either::Either::{self, Left, Right};
use errors::errors::{KarisError, KarisErrorType};
use lexer::tokens::IdentifierKind;
use parser::objects::{LiteralObjects, Node, Objects, Program};
use random_string::generate;

use crate::{
    compile::CompileWorker,
    defs::{BindingType, CallerParamType, OpCode, SymbolScope},
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
            _ => unreachable!("Cannot compile UNKNOWN or CONSUMABLE"),
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
        match self.non_root {
            true => {
                let mut res = None;

                for item in self.body.iter() {
                    res = item.compile(worker.clone(), scope.clone(), scope_id);
                }

                res
            }
            false => {
                for item in self.body.iter() {
                    item.compile(worker.clone(), scope.clone(), scope_id);
                }

                None
            }
        }
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

            IdentifierKind::BANG => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, worker.clone(), scope.clone(), scope_id).unwrap();
                let left = &left[0];

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, worker.clone(), scope, scope_id).unwrap();
                let right = &right[0];

                let wrk = worker.borrow();
                let instructions = wrk.add_infix(OpCode::OpBang, left.to_vec(), right.to_vec());
                Some(vec![instructions])
            }
            IdentifierKind::LT => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, worker.clone(), scope.clone(), scope_id).unwrap();
                let left = &left[0];

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, worker.clone(), scope, scope_id).unwrap();
                let right = &right[0];

                let wrk = worker.borrow();
                let instructions = wrk.add_infix(OpCode::OpLessThan, left.to_vec(), right.to_vec());
                Some(vec![instructions])
            }
            IdentifierKind::GT => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, worker.clone(), scope.clone(), scope_id).unwrap();
                let left = &left[0];

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, worker.clone(), scope, scope_id).unwrap();
                let right = &right[0];

                let wrk = worker.borrow();
                let instructions =
                    wrk.add_infix(OpCode::OpGreaterThan, left.to_vec(), right.to_vec());
                Some(vec![instructions])
            }
            IdentifierKind::EQ => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, worker.clone(), scope.clone(), scope_id).unwrap();
                let left = &left[0];

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, worker.clone(), scope, scope_id).unwrap();
                let right = &right[0];

                let wrk = worker.borrow();
                let instructions = wrk.add_infix(OpCode::OpEqualTo, left.to_vec(), right.to_vec());
                Some(vec![instructions])
            }
            IdentifierKind::NOTEQ => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, worker.clone(), scope.clone(), scope_id).unwrap();
                let left = &left[0];

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, worker.clone(), scope, scope_id).unwrap();
                let right = &right[0];

                let wrk = worker.borrow();
                let instructions =
                    wrk.add_infix(OpCode::OpNotEqualTo, left.to_vec(), right.to_vec());
                Some(vec![instructions])
            }
            IdentifierKind::GTOREQ => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, worker.clone(), scope.clone(), scope_id).unwrap();
                let left = &left[0];

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, worker.clone(), scope, scope_id).unwrap();
                let right = &right[0];

                let wrk = worker.borrow();
                let instructions =
                    wrk.add_infix(OpCode::OpGreaterThanOrEqual, left.to_vec(), right.to_vec());
                Some(vec![instructions])
            }
            IdentifierKind::LTOREQ => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, worker.clone(), scope.clone(), scope_id).unwrap();
                let left = &left[0];

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, worker.clone(), scope, scope_id).unwrap();
                let right = &right[0];

                let wrk = worker.borrow();
                let instructions =
                    wrk.add_infix(OpCode::OpLessThanOrEqual, left.to_vec(), right.to_vec());
                Some(vec![instructions])
            }
            IdentifierKind::AND => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, worker.clone(), scope.clone(), scope_id).unwrap();
                let left = &left[0];

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, worker.clone(), scope, scope_id).unwrap();
                let right = &right[0];

                let wrk = worker.borrow();
                let instructions = wrk.add_infix(OpCode::OpAND, left.to_vec(), right.to_vec());
                Some(vec![instructions])
            }
            IdentifierKind::OR => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, worker.clone(), scope.clone(), scope_id).unwrap();
                let left = &left[0];

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, worker.clone(), scope, scope_id).unwrap();
                let right = &right[0];

                let wrk = worker.borrow();
                let instructions = wrk.add_infix(OpCode::OpOR, left.to_vec(), right.to_vec());
                Some(vec![instructions])
            }
            IdentifierKind::LAND => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, worker.clone(), scope.clone(), scope_id).unwrap();
                let left = &left[0];

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, worker.clone(), scope, scope_id).unwrap();
                let right = &right[0];

                let wrk = worker.borrow();
                let instructions = wrk.add_infix(OpCode::OpLAND, left.to_vec(), right.to_vec());
                Some(vec![instructions])
            }
            IdentifierKind::LOR => {
                let left = self.left_child.as_ref().unwrap();
                let left = left_or_right(left, worker.clone(), scope.clone(), scope_id).unwrap();
                let left = &left[0];

                let right = self.right_child.as_ref().unwrap();
                let right = left_or_right(right, worker.clone(), scope, scope_id).unwrap();
                let right = &right[0];

                let wrk = worker.borrow();
                let instructions = wrk.add_infix(OpCode::OpLOR, left.to_vec(), right.to_vec());
                Some(vec![instructions])
            }
            IdentifierKind::VARIABLE => {
                let variable_name = self.variable_name.as_ref().unwrap();

                let variable_name_as_bytes = variable_name.as_bytes().to_vec();
                let wrk = worker.borrow();
                let instructions = wrk.generate_opcode_with_scope_and_vec_parameter(
                    OpCode::OpGetBinding,
                    scope,
                    scope_id,
                    variable_name_as_bytes,
                );
                Some(vec![instructions])
            }

            IdentifierKind::IF => {
                let condition_binding_key = random_string_id();
                let condition_binding_key_as_bytes = condition_binding_key.as_bytes().to_vec();

                let wrk = worker.borrow();
                let if_scope_id = wrk.generate_scope_id_from_scope(SymbolScope::Local);

                let mut condition_instructions = Vec::new();
                // get the condition first
                let condition = self.right_child.as_ref().unwrap();

                if let Some(instructions) =
                    left_or_right(condition, worker.clone(), scope.clone(), if_scope_id)
                {
                    let inst = instructions.get(0).unwrap();
                    condition_instructions.push(inst.clone());

                    condition_instructions.push(vec![OpCode::OpJumpTo as u8]);

                    let block_children = self.block_children.as_ref().unwrap();
                    for body_item in block_children.iter() {
                        let inst =
                            body_item.compile(worker.clone(), SymbolScope::Local, if_scope_id);
                        if let Some(all_insts) = inst {
                            for i in all_insts.iter() {
                                condition_instructions.push(i.clone())
                            }
                        }
                    }
                }

                // check if there is an alternate condition (ELSE)
                if let Some(alternate_condition) = &self.alternate {
                    if let Some(else_condition) =
                        alternate_condition.compile(worker.clone(), scope.clone(), if_scope_id)
                    {
                        // we add an alternate condition marker
                        condition_instructions.push(vec![OpCode::OpJumpToAlternate as u8]);

                        for i in else_condition.iter() {
                            condition_instructions.push(i.clone())
                        }
                    }
                }

                wrk.add_symbol(
                    condition_binding_key_as_bytes.clone(),
                    condition_instructions.clone(),
                );

                let insts = wrk.instructions_for_condition_statement(
                    OpCode::OpAddIfCondition,
                    scope,
                    if_scope_id,
                    condition_binding_key_as_bytes,
                );

                Some(vec![insts])
            }

            IdentifierKind::ELSE => {
                let wrk = worker.borrow();
                let else_scope_id = wrk.generate_scope_id_from_scope(SymbolScope::Local);

                let mut condition_instructions = Vec::new();

                let cond = self.right_child.as_ref().unwrap();

                if let Some(alternate_instructions) =
                    left_or_right(cond, worker.clone(), scope.clone(), else_scope_id)
                {
                    // add the alternate condition instructions
                    let alt_cond_instructions = alternate_instructions.get(0).unwrap();
                    condition_instructions.push(alt_cond_instructions.clone());
                }

                // add the else body.
                condition_instructions.push(vec![OpCode::OpJumpTo as u8]);

                // loop over the children and add their instructions
                let block_children = self.block_children.as_ref().unwrap();
                for body_item in block_children.iter() {
                    let inst = body_item.compile(worker.clone(), SymbolScope::Local, else_scope_id);
                    if let Some(all_insts) = inst {
                        for i in all_insts.iter() {
                            condition_instructions.push(i.clone())
                        }
                    }
                }

                if let Some(alternate) = &self.alternate {
                    if let Some(alt_insts) = alternate.compile(worker.clone(), scope, else_scope_id)
                    {
                        // add alternate marker
                        condition_instructions.push(vec![OpCode::OpJumpToAlternate as u8]);

                        for i in alt_insts.iter() {
                            condition_instructions.push(i.clone())
                        }
                    }
                }

                Some(condition_instructions)
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

                let insts = if let Some(instructions) =
                    left_or_right(rhs, worker.clone(), scope.clone(), scope_id)
                {
                    let wrk = worker.borrow();
                    wrk.add_symbol(binding_key_as_bytes.clone(), instructions.clone());

                    match rhs {
                        Left(_) => {
                            wrk.add_variable_binding(
                                scope,
                                scope_id,
                                BindingType::Literal,
                                binding_key_as_bytes,
                            );
                        }
                        Right(right) => {
                            let rght = right.as_ty_node().unwrap();
                            let kind = rght.identifier_kind.unwrap();
                            match kind {
                                IdentifierKind::INTLITERAL
                                | IdentifierKind::STRINGLITERAL
                                | IdentifierKind::BOOLEANLITERAL => {
                                    wrk.add_variable_binding(
                                        scope,
                                        scope_id,
                                        BindingType::Literal,
                                        binding_key_as_bytes,
                                    );
                                }

                                IdentifierKind::PLUS
                                | IdentifierKind::MINUS
                                | IdentifierKind::ASTERISK
                                | IdentifierKind::SLASH
                                | IdentifierKind::BANG
                                | IdentifierKind::AND
                                | IdentifierKind::LT
                                | IdentifierKind::GT
                                | IdentifierKind::EQ
                                | IdentifierKind::NOTEQ
                                | IdentifierKind::GTOREQ
                                | IdentifierKind::LTOREQ
                                | IdentifierKind::OR
                                | IdentifierKind::LAND
                                | IdentifierKind::LOR
                                | IdentifierKind::MODULUS => {
                                    wrk.add_variable_binding(
                                        scope,
                                        scope_id,
                                        BindingType::Expression,
                                        binding_key_as_bytes,
                                    );
                                }

                                IdentifierKind::CALLER => {
                                    wrk.add_variable_binding(
                                        scope,
                                        scope_id,
                                        BindingType::Caller,
                                        binding_key_as_bytes,
                                    );
                                }

                                // for function we don't attach it to a binding instruction. In the event the funtion is called
                                // at some point in the program, the caller binding will suffice. Otherwise it will be considered as
                                // unused function
                                IdentifierKind::FUNCTION => {}

                                _ => unreachable!(""),
                            };
                        }
                    };

                    Some(instructions)
                } else {
                    None
                };

                insts
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
                        SymbolScope::Global,
                        wrk.global_scope_id(),
                        OpCode::OpGetFunctionParameter,
                    ) {
                        function_instructions.push(instructions);
                    }
                }

                // add seperator terminal
                function_instructions.push(vec![OpCode::OpNull as u8]);

                let block_children = self.block_children.as_ref().unwrap();
                for body_item in block_children.iter() {
                    let inst = body_item.compile(worker.clone(), SymbolScope::Local, func_scope_id);
                    if let Some(all_insts) = inst {
                        for i in all_insts.iter() {
                            function_instructions.push(i.clone())
                        }
                    }
                }

                Some(function_instructions)
            }

            IdentifierKind::RETURN => {
                let right = self.right_child.as_ref().unwrap();
                let instructions =
                    left_or_right(right, worker.clone(), scope.clone(), scope_id).unwrap();

                let wrk = worker.borrow();
                let insts = wrk.instructions_for_return(scope, scope_id, instructions);

                Some(vec![insts])
            }

            IdentifierKind::CALLER => {
                // check the scope. if it's GLOBAL, exit the program because a function should only be called
                // inside a function block or main block, both of which are LOCAL

                if scope.is_global() {
                    eprintln!("Incorrect function call. A function should only be called inside a function block or main block");
                    process::exit(0x0100)
                }

                let func_name = self.variable_name.as_ref().unwrap();
                let func_name_as_bytes = func_name.as_bytes().to_vec();

                let wrk = worker.borrow();

                // we check that the function exists in global scope
                if let Some(_function_symbol) = wrk.get_symbol(func_name_as_bytes.clone()) {
                    let mut caller_instructions = Vec::new();

                    let wrk = worker.borrow();
                    let caller_scope_id = wrk.generate_scope_id_from_scope(SymbolScope::Local);

                    let caller_def = wrk.instructions_for_caller(
                        scope.clone(),
                        caller_scope_id,
                        func_name_as_bytes,
                    );

                    // the first set of instructions will point to the actual function that will be called
                    caller_instructions.push(caller_def);

                    // subsequent instructions will point to the call parameters if the function requires any.
                    if let Some(caller_params) = &self.call_params {
                        for param in caller_params.iter() {
                            call_parameter_map(
                                param,
                                worker.clone(),
                                &mut caller_instructions,
                                scope.clone(),
                                caller_scope_id,
                                OpCode::OpGetCallerParameter,
                            );
                        }
                    }

                    Some(caller_instructions)
                } else {
                    eprintln!("Function of name {:?} not found in global scope", func_name);
                    process::exit(0x0100)
                }
            }

            IdentifierKind::PRINT => {
                if let Some(caller_params) = &self.call_params {
                    let mut caller_instructions = Vec::new();

                    for param in caller_params.iter() {
                        call_parameter_map(
                            param,
                            worker.clone(),
                            &mut caller_instructions,
                            scope.clone(),
                            scope_id,
                            OpCode::OpPrint,
                        );
                    }

                    let binding_key = random_string_id();
                    let binding_key_as_bytes = binding_key.as_bytes().to_vec();

                    let wrk = worker.borrow();
                    wrk.add_symbol(binding_key_as_bytes.clone(), caller_instructions.clone());

                    let print_instructions =
                        wrk.instructions_for_builtin(scope, scope_id, binding_key_as_bytes);

                    Some(vec![print_instructions])
                } else {
                    None
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

fn call_parameter_map(
    param: &Either<LiteralObjects, Objects>,
    worker: Rc<RefCell<&CompileWorker>>,
    caller_instructions: &mut Vec<Vec<u8>>,
    scope: SymbolScope,
    scope_id: [u8; 2],
    command: OpCode,
) {
    match param {
        Left(left) => match left {
            LiteralObjects::ObjIntergerValue(int) => {
                let int_lit = int.value.unwrap();
                let obj = CompileObject::Interger(int_lit);
                let wrk = worker.borrow();
                let const_location = wrk.append_to_constant_pool(obj);

                let mut instructions = vec![command as u8, scope as u8];

                for id in scope_id {
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
            LiteralObjects::ObjBooleanValue(bool) => {
                let bool_lit = bool.value.unwrap();
                let obj = CompileObject::Boolean(bool_lit);
                let wrk = worker.borrow();
                let const_location = wrk.append_to_constant_pool(obj);

                let mut instructions = vec![command as u8, scope as u8];

                for id in scope_id {
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
            LiteralObjects::ObjStringValue(string) => {
                let string_lit = string.value.as_ref().unwrap();
                let obj = CompileObject::String(string_lit.to_string());
                let wrk = worker.borrow();

                let const_location = wrk.append_to_constant_pool(obj);

                let mut instructions = vec![command as u8, scope as u8];

                for id in scope_id {
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
            if let Some(insts) = function_or_caller_object_param_access_instructions(
                right,
                worker.clone(),
                scope.clone(),
                scope_id,
                OpCode::OpGetCallerParameter,
            ) {
                let mut instructions = vec![command as u8, scope as u8];

                for id in scope_id {
                    instructions.push(id);
                }

                let mut other = vec![
                    OpCode::OpTerminal as u8,
                    CallerParamType::Variable as u8,
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

fn random_string_id() -> String {
    let charset = "1234567890";
    generate(7, charset)
}
