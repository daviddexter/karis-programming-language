use std::{fs::File, io::Read, iter::zip};

use borsh::BorshDeserialize;
use itertools::Itertools;

use errors::errors::{KarisError, KarisErrorType};

use crate::{
    compile::ByteCode,
    defs::{BindingType, CallerParamType, OpCode},
    objects::CompileObject,
};

pub struct VM {
    byte_code: ByteCode,
    // stack: Rc<RefCell<Vec<u8>>>,
}

impl VM {
    pub fn from_executable_file(file_path: &str) -> Result<VM, KarisError> {
        let mut file = File::open(file_path).expect("unable to open file with error");
        let mut buffer = Vec::<u8>::new();
        file.read_to_end(&mut buffer)?;

        let byte_code = ByteCode::try_from_slice(&buffer).unwrap();
        Ok(Self {
            byte_code,
            // stack: Rc::new(RefCell::new(Vec::new())),
        })
    }

    pub fn from_raw_bytecode(byte_code: ByteCode) -> VM {
        Self {
            byte_code,
            // stack: Rc::new(RefCell::new(Vec::new())),
        }
    }

    pub fn execute(&self) {
        let instructions = &self.byte_code.instructions;

        for instruction in instructions.iter() {
            let _ = self.executor(instruction, None);
        }
    }

    fn executor(
        &self,
        instruction: &Vec<u8>,
        params: Option<Vec<(CompileObject, CompileObject)>>,
    ) -> Result<CompileObject, errors::errors::KarisError> {
        let command = instruction.first().unwrap();
        let command = *command;

        match command.try_into() {
            Ok(OpCode::OpTerminal) => Ok(CompileObject::Null),
            Ok(OpCode::OpAdd) => {
                let instructions = instruction.get(2..instruction.len()).unwrap();

                let separator = instructions
                    .iter()
                    .find_position(|i| **i == (OpCode::OpNull as u8))
                    .unwrap();
                let seperator_index = separator.0;

                let left = instructions.get(0..seperator_index).unwrap();
                let right = instructions
                    .get(seperator_index + 1..instructions.len())
                    .unwrap();

                if let Some(caller_params) = params {
                    let lhs_index = caller_params
                        .iter()
                        .position(|cp| {
                            let obj = &cp.0;
                            match obj {
                                CompileObject::Variable(var) => {
                                    let left_binding = left.get(5..left.len() - 1).unwrap();
                                    var == left_binding
                                }
                                CompileObject::Interger(_)
                                | CompileObject::String(_)
                                | CompileObject::Boolean(_)
                                | CompileObject::Null => false,
                            }
                        })
                        .unwrap();

                    let lhs_value = caller_params.get(lhs_index).unwrap();
                    let lhs_value = &lhs_value.1;
                    let lhs_value = lhs_value.as_interger().unwrap();

                    let rhs_index = caller_params
                        .iter()
                        .position(|cp| {
                            let obj = &cp.0;
                            match obj {
                                CompileObject::Variable(var) => {
                                    let right_binding = right.get(5..left.len() - 1).unwrap();
                                    var == right_binding
                                }
                                CompileObject::Interger(_)
                                | CompileObject::String(_)
                                | CompileObject::Boolean(_)
                                | CompileObject::Null => false,
                            }
                        })
                        .unwrap();

                    let rhs_value = caller_params.get(rhs_index).unwrap();
                    let rhs_value = &rhs_value.1;
                    let rhs_value = rhs_value.as_interger().unwrap();

                    let sum = lhs_value + rhs_value;

                    Ok(CompileObject::Interger(sum))
                } else {
                    Ok(CompileObject::Interger(0))
                }
            }
            Ok(OpCode::OpMinus) => todo!(),
            Ok(OpCode::OpMultiply) => todo!(),
            Ok(OpCode::OpDivide) => todo!(),
            Ok(OpCode::OpModulus) => todo!(),

            Ok(OpCode::OpSetBinding) => {
                let binding_type = instruction.get(5).unwrap();
                let binding_type = *binding_type;

                match binding_type.try_into() {
                    Ok(BindingType::Literal) => {
                        let binding_name = instruction.get(7..instruction.len() - 1).unwrap();

                        // get the literal from symbols table
                        let literal_symbol =
                            self.byte_code.symbols_table.0.get(binding_name).unwrap();
                        let literal_instructions = &literal_symbol.0;
                        let literal_instructions = literal_instructions.get(0).unwrap();

                        let constant_address = literal_instructions.get(5).unwrap();

                        let constant_address = *constant_address as usize;

                        let constant_object =
                            self.byte_code.constants.get(constant_address).unwrap();
                        Ok(constant_object.clone())
                    }

                    Ok(BindingType::Caller) => {
                        let binding_name = instruction.get(7..instruction.len() - 1).unwrap();

                        // get the function to execute from symbols table
                        let caller_symbol =
                            self.byte_code.symbols_table.0.get(binding_name).unwrap();
                        let caller_instructions = &caller_symbol.0;

                        let caller_function = caller_instructions.get(0).unwrap();

                        let caller_parameters =
                            caller_instructions.get(1..caller_instructions.len());
                        let caller_parameters = caller_parameters.unwrap();

                        let caller_function_name =
                            caller_function.get(5..caller_function.len() - 1).unwrap();

                        let function_definition = self
                            .byte_code
                            .symbols_table
                            .0
                            .get(caller_function_name)
                            .unwrap();
                        let function_definition_instructions = &function_definition.0;

                        // retrieve function parameters
                        let separator = function_definition_instructions
                            .iter()
                            .find_position(|v| v[0] == OpCode::OpNull as u8)
                            .unwrap();
                        let seperator_index = separator.0;

                        let empty_things = Vec::new();
                        let function_parameters =
                            match function_definition_instructions.get(0..seperator_index) {
                                Some(p) => p,
                                None => &empty_things,
                            };

                        let caller_parameters: Vec<CompileObject> = caller_parameters
                            .iter()
                            .map(|param| {
                                let param_type = param.get(5).unwrap();
                                let param_type = CallerParamType::from(*param_type);
                                match param_type {
                                    CallerParamType::Literal => {
                                        let param_location = param.get(7).unwrap();
                                        let param_location = *param_location as usize;
                                        let param_object_value =
                                            self.byte_code.constants.get(param_location).unwrap();
                                        param_object_value.clone()
                                    }
                                    CallerParamType::Variable => {
                                        let param_instructions =
                                            param.get(7..param.len() - 1).unwrap();
                                        let param_instructions = param_instructions.to_vec();
                                        if let Ok(obj) =
                                            self.executor(&param_instructions, params.clone())
                                        {
                                            obj
                                        } else {
                                            CompileObject::Null
                                        }
                                    }
                                }
                            })
                            .collect();

                        let function_def_parameters: Vec<CompileObject> = function_parameters
                            .iter()
                            .map(|param| {
                                let binding_name = param.get(5..param.len() - 1).unwrap();
                                CompileObject::Variable(binding_name.to_vec())
                            })
                            .collect();

                        let mut params = Vec::new();
                        for param in zip(function_def_parameters, caller_parameters) {
                            params.push(param);
                        }

                        // retrieve function block items
                        let block_items = match function_definition_instructions
                            .get(seperator_index..function_definition_instructions.len())
                        {
                            Some(p) => p,
                            None => &empty_things, // the likelihood of this is zero
                        };

                        let mut caller_result = Ok(CompileObject::Null);

                        for item in block_items.iter() {
                            let code = item.first().unwrap();

                            match OpCode::from(*code) {
                                OpCode::OpReturn => {
                                    caller_result = self.executor(item, Some(params.clone()));
                                    break;
                                }
                                _ => caller_result = self.executor(item, Some(params.clone())),
                            }
                        }

                        caller_result
                    }
                    Ok(BindingType::Expression) => todo!("binding for expression"),

                    Err(_) => Err(KarisError {
                        error_type: KarisErrorType::InvalidExecution,
                        message:
                            "Unexpected execution error encountered. There is no way to recover"
                                .to_string(),
                    }),
                }
            }

            Ok(OpCode::OpGetFunctionParameter) => {
                println!("OpGetFunctionParameter : {:?}", instruction);
                Ok(CompileObject::Null)
            }

            Ok(OpCode::OpReturn) => {
                let return_instructions = instruction.get(5..instruction.len() - 1).unwrap();
                let return_instructions = return_instructions.to_vec();
                self.executor(&return_instructions, params)
            }

            Ok(OpCode::OpGetCallerParameter) => {
                let binding_name = instruction.get(5..instruction.len() - 1).unwrap();

                // get the binding value from symbols table
                let binding_symbol = self.byte_code.symbols_table.0.get(binding_name).unwrap();
                let binding_instructions = &binding_symbol.0;
                let binding_instructions = binding_instructions.get(0).unwrap();
                self.executor(binding_instructions, params)
            }

            Ok(OpCode::OpConstant) => {
                let location = instruction.get(5).unwrap();
                let location = *location as usize;
                let obj = self.byte_code.constants.get(location).unwrap();
                let obj = obj.clone();
                Ok(obj)
            }

            Ok(OpCode::OpAddBuiltin) => {
                let symbol_key = instruction.get(5..instruction.len() - 1).unwrap();

                if let Some(symbol) = self.byte_code.symbols_table.0.get(symbol_key) {
                    let instructions = &symbol.0;
                    let instructions = instructions.get(0).unwrap();
                    return self.executor(instructions, params);
                }

                Ok(CompileObject::Null)
            }

            Ok(OpCode::OpPrint) => {
                println!("print {:?}", instruction);

                Ok(CompileObject::Null)
            }

            Ok(OpCode::OpNull)
            | Ok(OpCode::OpFunctionDef)
            | Ok(OpCode::OpCallerDef)
            | Ok(OpCode::OpGetBinding) => Ok(CompileObject::Null),
            Err(_) => unreachable!("Malformed program"),
        }
    }
}

#[cfg(test)]
mod vm_tests {
    use lexer::lexer::Lexer;
    use parser::parser::Parser;

    use crate::compile::CompileWorker;

    use super::VM;

    #[test]
    fn should_execute0() {
        let lx = Lexer::new(String::from(
            "
            let summation @int = fn(x @int, y @int) {
                return x + y;
            };

            @main fn(){
                let a @int = 10;
                let sum @int = summation(a,20);
            }@end;
        ",
        ));
        let mut parser = Parser::new(lx);
        let ast = parser.parse(Some("should_execute0.json")).unwrap();
        let worker = CompileWorker::new(ast);
        let byte_code = worker.compile();
        let vm = VM::from_raw_bytecode(byte_code);
        vm.execute();
    }

    #[test]
    fn should_execute1() {
        let lx = Lexer::new(String::from(
            "
            let summation @int = fn(x @int, y @int) {
                print(x);
                print(10);
                return x + y;
            };

            @main fn(){
                let a @int = 10;
                let sum @int = summation(a,20);
            }@end;
        ",
        ));
        let mut parser = Parser::new(lx);
        let ast = parser.parse(Some("should_execute1.json")).unwrap();
        let worker = CompileWorker::new(ast);
        let byte_code = worker.compile();
        let vm = VM::from_raw_bytecode(byte_code);
        vm.execute();
    }
}

// @main fn(){
//     let a @int = 10;
//     let sum @int = summation(a, 20);
// }@end;

// This example shows that Karis requires function calls to be bound to a variable
// for them to be executed
//
//
// let summation @unit = fn(x @int, y @int) {
//     let sum @int =  x + y;
//     print(sum);
// };

// @main fn(){
//     let _ @unit = summation(10,20);
// }@end;
//
//
