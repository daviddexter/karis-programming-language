use crate::{objects::ArrayObject, vm::VM};
use debug_print::debug_println;
use errors::errors::{KarisError, KarisErrorType};
use itertools::Itertools;
use std::iter::zip;

use crate::{
    defs::{CallerParamType, OpCode},
    objects::{CompileObject, BOOLEAN_OBJECT_TYPE, INTERGER_OBJECT_TYPE, STRING_OBJECT_TYPE},
};

impl VM {
    pub(crate) fn executor(
        &self,
        instruction: &Vec<Vec<u8>>,
        params: Option<Vec<(CompileObject, CompileObject)>>,
    ) -> Result<CompileObject, errors::errors::KarisError> {
        let command = instruction.first().unwrap();
        let command = command.first().unwrap();
        let command = *command;

        let command: OpCode = command.into();

        match command {
            OpCode::OpTerminal => Ok(CompileObject::Null),

            OpCode::OpAdd
            | OpCode::OpMinus
            | OpCode::OpMultiply
            | OpCode::OpDivide
            | OpCode::OpModulus => {
                let op_instruction = instruction.first().unwrap();
                let instructions = op_instruction.get(2..op_instruction.len()).unwrap();

                let separator = instructions
                    .iter()
                    .find_position(|i| **i == (OpCode::OpNull as u8))
                    .unwrap();
                let seperator_index = separator.0;

                let left = instructions.get(0..seperator_index).unwrap().to_vec();
                let left = vec![left];

                let right = instructions
                    .get(seperator_index + 1..instructions.len())
                    .unwrap()
                    .to_vec();
                let right = vec![right];

                debug_println!("Left: {:?}", left);
                debug_println!("Right: {:?}", right);

                let left_value = match self.executor(&left, params.clone()) {
                    Ok(obj) => match obj {
                        CompileObject::Interger(number) => number,
                        _ => 0_isize,
                    },
                    Err(_) => 0_isize,
                };

                let right_value = match self.executor(&right, params.clone()) {
                    Ok(obj) => match obj {
                        CompileObject::Interger(number) => number,
                        _ => 0_isize,
                    },
                    Err(_) => 0_isize,
                };

                let result = match command {
                    OpCode::OpAdd => left_value + right_value,
                    OpCode::OpMinus => left_value - right_value,
                    OpCode::OpMultiply => left_value * right_value,
                    OpCode::OpDivide => left_value / right_value,
                    OpCode::OpModulus => left_value % right_value,
                    _ => 0_isize,
                };

                Ok(CompileObject::Interger(result))
            }

            OpCode::OpGetBinding => {
                debug_println!("get binding {:?}", instruction);

                let op_instruction = instruction.first().unwrap();
                let binding_name = op_instruction
                    .get(5..op_instruction.len())
                    .unwrap()
                    .to_vec();

                let binding_name = binding_name
                    .iter()
                    .filter(|elem| **elem != OpCode::OpTerminal as u8)
                    .copied()
                    .collect::<Vec<u8>>();

                debug_println!("binding name {:?}", binding_name);

                let binding_value =
                    // fetch the binding from symbol table. If not present, fetch from params
                    if let Some(symbol) = self.byte_code.symbols_table.0.get(&binding_name) {
                        let symbol = &symbol.0;
                        if let Ok(obj) = self.executor(symbol, params) {
                            obj
                        } else {
                            CompileObject::Null
                        }
                    } else if let Some(parameters) = params   {
                            let parameter = parameters.iter().filter(|elem| {
                                    let name_binding = elem.0.as_variable().unwrap();
                                    let name_binding = name_binding.clone();
                                    name_binding == binding_name
                                }).collect::<Vec<&(CompileObject,CompileObject)>>();

                            let parameter = parameter.first().unwrap();
                            parameter.1.clone()
                    }else{
                            CompileObject::Null
                    };

                debug_println!("binding value {:?}", binding_value);
                Ok(binding_value)
            }

            OpCode::OpReturn => {
                let op_instruction = instruction.first().unwrap();
                let return_instructions = op_instruction.get(5..op_instruction.len() - 1).unwrap();
                let return_instructions = return_instructions.to_vec();
                let return_instructions = vec![return_instructions];
                self.executor(&return_instructions, params)
            }

            OpCode::OpGetCallerParameter => {
                let op_instruction = instruction.first().unwrap();
                let binding_name = op_instruction.get(5..op_instruction.len()).unwrap();

                // get the binding value from symbols table
                let binding_symbol = self.byte_code.symbols_table.0.get(binding_name).unwrap();
                let binding_instructions = &binding_symbol.0;
                let binding_instructions = binding_instructions.get(0).unwrap();
                let binding_instructions = binding_instructions.clone();
                let binding_instructions = vec![binding_instructions];
                self.executor(&binding_instructions, params)
            }

            OpCode::OpConstant => {
                let op_instruction = instruction.first().unwrap();
                let location = op_instruction.get(5).unwrap();
                let location = *location as usize;
                let obj = self.byte_code.constants.get(location).unwrap();
                let obj = obj.clone();
                Ok(obj)
            }

            OpCode::OpAddBuiltin => {
                let op_instruction = instruction.first().unwrap();

                let symbol_key = op_instruction.get(5..op_instruction.len() - 1).unwrap();

                if let Some(symbol) = self.byte_code.symbols_table.0.get(symbol_key) {
                    let instructions = &symbol.0;
                    let instructions = instructions.get(0).unwrap();
                    let instructions = instructions.clone();
                    let instructions = vec![instructions];
                    return self.executor(&instructions, params);
                }

                Ok(CompileObject::Null)
            }

            OpCode::OpArray => {
                let op_instruction = instruction.first().unwrap();
                let array_binding = op_instruction.get(5..op_instruction.len() - 1).unwrap();
                let symbol = self.byte_code.symbols_table.0.get(array_binding).unwrap();
                let symbol: &Vec<Vec<u8>> = &symbol.0;

                let mut int_items = Vec::new();
                let mut string_items = Vec::new();
                let mut bool_items = Vec::new();

                let mut is_int_item: bool = false;
                let mut is_string_item: bool = false;
                let mut is_bool_item: bool = false;

                for array_item_instructions in symbol.iter() {
                    let array_item_instructions = array_item_instructions.clone();
                    let array_item_instructions = vec![array_item_instructions];

                    if let Ok(item) = self.executor(&array_item_instructions, params.clone()) {
                        match item {
                            CompileObject::Interger(integer) => {
                                is_int_item = true;
                                int_items.push(integer);
                            }
                            CompileObject::String(string) => {
                                is_string_item = true;
                                string_items.push(string);
                            }
                            CompileObject::Boolean(boolean) => {
                                is_bool_item = true;
                                bool_items.push(boolean);
                            }
                            _ => {}
                        }
                    }
                }

                if is_int_item {
                    let obj = CompileObject::Array(ArrayObject::Interger(int_items));
                    return Ok(obj);
                }

                if is_string_item {
                    let obj = CompileObject::Array(ArrayObject::String(string_items));
                    return Ok(obj);
                }

                if is_bool_item {
                    let obj = CompileObject::Array(ArrayObject::Boolean(bool_items));
                    return Ok(obj);
                }

                Ok(CompileObject::Null)
            }

            OpCode::OpPrint => {
                let op_instruction = instruction.first().unwrap();
                let print_type = op_instruction.get(5).unwrap();
                let print_value: CallerParamType = CallerParamType::from(*print_type);

                match print_value {
                    CallerParamType::Literal => {
                        let loc = op_instruction.get(7).unwrap();
                        let loc = *loc as usize;
                        let value = self.byte_code.constants.get(loc).unwrap();

                        match value {
                            CompileObject::Interger(val) => println!("{:?}", val),
                            CompileObject::String(val) => println!("{:?}", val),
                            CompileObject::Boolean(val) => println!("{:?}", val),
                            _ => {}
                        }
                    }
                    CallerParamType::Variable => {
                        let variable_access_instructions =
                            op_instruction.get(7..op_instruction.len()).unwrap();

                        let access_command = variable_access_instructions.first().unwrap();
                        let access_command = *access_command;
                        let access_command: OpCode = access_command.into();

                        if access_command == OpCode::OpGetCallerParameter {
                            // get the access instructions
                            let access_key = variable_access_instructions
                                .get(5..variable_access_instructions.len() - 1)
                                .unwrap();

                            if let Some(symbol) = self.byte_code.symbols_table.0.get(access_key) {
                                let symbols: Vec<Vec<u8>> = symbol.0.clone();
                                if let Ok(response) = self.executor(&symbols, params) {
                                    println!("{:?}", response)
                                }
                            }
                        }
                    }
                };

                Ok(CompileObject::Null)
            }

            OpCode::OpAddIfCondition => {
                let op_instruction = instruction.first().unwrap();
                let mut result = Ok(CompileObject::Null);

                let condition_binding_name =
                    op_instruction.get(5..op_instruction.len() - 1).unwrap();

                // get the binding value from symbols table
                let condition_instructions = self
                    .byte_code
                    .symbols_table
                    .0
                    .get(condition_binding_name)
                    .unwrap();
                let condition_instructions = &condition_instructions.0;

                let mut marker = 0;
                while marker < condition_instructions.len() {
                    let instructions: Vec<u8> =
                        condition_instructions.get(marker).unwrap().to_vec();

                    let command = instructions.first().unwrap();
                    let command = *command;
                    let command: OpCode = command.into();

                    match command {
                        OpCode::OpGreaterThan
                        | OpCode::OpGreaterThanOrEqual
                        | OpCode::OpLessThan
                        | OpCode::OpLessThanOrEqual
                        | OpCode::OpEqualTo
                        | OpCode::OpNotEqualTo
                        | OpCode::OpAND
                        | OpCode::OpOR
                        | OpCode::OpLAND
                        | OpCode::OpLOR => {
                            let instructions = instructions.clone();
                            let instructions = vec![instructions];
                            match self.executor(&instructions, params.clone()) {
                                Ok(val) => {
                                    let verdict = val.as_boolean().unwrap();
                                    if *verdict {
                                        marker += 1;
                                        continue;
                                    } else {
                                        // move two places
                                        marker += 2;
                                        continue;
                                    }
                                }
                                Err(err) => {
                                    result = Err(err);
                                    break;
                                }
                            }
                        }

                        OpCode::OpJumpTo | OpCode::OpJumpToAlternate => {
                            marker += 1;
                            continue;
                        }
                        OpCode::OpReturn => {
                            let instructions = instructions.clone();
                            let instructions = vec![instructions];
                            result = self.executor(&instructions, params);
                            break;
                        }
                        _ => {}
                    }
                }

                result
            }

            OpCode::OpGreaterThan
            | OpCode::OpGreaterThanOrEqual
            | OpCode::OpLessThan
            | OpCode::OpLessThanOrEqual
            | OpCode::OpEqualTo
            | OpCode::OpNotEqualTo
            | OpCode::OpAND
            | OpCode::OpOR
            | OpCode::OpLAND
            | OpCode::OpLOR => {
                let op_instruction = instruction.first().unwrap();
                let instructions = op_instruction.get(2..op_instruction.len()).unwrap();

                let separator = instructions
                    .iter()
                    .find_position(|i| **i == (OpCode::OpNull as u8))
                    .unwrap();
                let seperator_index = separator.0;

                let left = instructions.get(0..seperator_index).unwrap();
                let right = instructions
                    .get(seperator_index + 1..instructions.len())
                    .unwrap();

                let variable_or_literal_func =
                    |side_instructions: Vec<u8>,
                     operation_params: Option<Vec<(CompileObject, CompileObject)>>|
                     -> CompileObject {
                        let instructions = vec![side_instructions];
                        if let Ok(obj) = self.executor(&instructions, operation_params) {
                            obj
                        } else {
                            CompileObject::Null
                        }
                    };

                let lhs = variable_or_literal_func(left.to_vec(), params.clone());
                let rhs = variable_or_literal_func(right.to_vec(), params);

                if lhs.object_type() != rhs.object_type() {
                    return Err(KarisError {
                        error_type: KarisErrorType::InvalidExecution,
                        message: "Comparison type mismatch".to_string(),
                    });
                }

                match command {
                    OpCode::OpGreaterThan => {
                        let obj_type = lhs.object_type();
                        let result = match obj_type {
                            INTERGER_OBJECT_TYPE => {
                                let lhs_value = lhs.as_interger().unwrap();
                                let rhs_value = rhs.as_interger().unwrap();
                                lhs_value > rhs_value
                            }
                            STRING_OBJECT_TYPE => {
                                let lhs_value = lhs.as_string().unwrap();
                                let rhs_value = rhs.as_string().unwrap();
                                lhs_value.len() > rhs_value.len()
                            }
                            BOOLEAN_OBJECT_TYPE => {
                                let lhs_value = lhs.as_boolean().unwrap();
                                let rhs_value = rhs.as_boolean().unwrap();
                                lhs_value > rhs_value
                            }
                            _ => false,
                        };
                        Ok(CompileObject::Boolean(result))
                    }

                    OpCode::OpGreaterThanOrEqual => {
                        let obj_type = lhs.object_type();
                        let result = match obj_type {
                            INTERGER_OBJECT_TYPE => {
                                let lhs_value = lhs.as_interger().unwrap();
                                let rhs_value = rhs.as_interger().unwrap();
                                lhs_value >= rhs_value
                            }
                            STRING_OBJECT_TYPE => {
                                let lhs_value = lhs.as_string().unwrap();
                                let rhs_value = rhs.as_string().unwrap();
                                lhs_value.len() >= rhs_value.len()
                            }
                            BOOLEAN_OBJECT_TYPE => {
                                let lhs_value = lhs.as_boolean().unwrap();
                                let rhs_value = rhs.as_boolean().unwrap();
                                lhs_value >= rhs_value
                            }
                            _ => false,
                        };
                        Ok(CompileObject::Boolean(result))
                    }

                    OpCode::OpLessThan => {
                        let obj_type = lhs.object_type();
                        let result = match obj_type {
                            INTERGER_OBJECT_TYPE => {
                                let lhs_value = lhs.as_interger().unwrap();
                                let rhs_value = rhs.as_interger().unwrap();
                                lhs_value < rhs_value
                            }
                            STRING_OBJECT_TYPE => {
                                let lhs_value = lhs.as_string().unwrap();
                                let rhs_value = rhs.as_string().unwrap();
                                lhs_value.len() < rhs_value.len()
                            }
                            BOOLEAN_OBJECT_TYPE => {
                                let lhs_value = lhs.as_boolean().unwrap();
                                let rhs_value = rhs.as_boolean().unwrap();
                                lhs_value < rhs_value
                            }
                            _ => false,
                        };
                        Ok(CompileObject::Boolean(result))
                    }
                    OpCode::OpLessThanOrEqual => {
                        let obj_type = lhs.object_type();
                        let result = match obj_type {
                            INTERGER_OBJECT_TYPE => {
                                let lhs_value = lhs.as_interger().unwrap();
                                let rhs_value = rhs.as_interger().unwrap();
                                lhs_value <= rhs_value
                            }
                            STRING_OBJECT_TYPE => {
                                let lhs_value = lhs.as_string().unwrap();
                                let rhs_value = rhs.as_string().unwrap();
                                lhs_value.len() <= rhs_value.len()
                            }
                            BOOLEAN_OBJECT_TYPE => {
                                let lhs_value = lhs.as_boolean().unwrap();
                                let rhs_value = rhs.as_boolean().unwrap();
                                lhs_value <= rhs_value
                            }
                            _ => false,
                        };
                        Ok(CompileObject::Boolean(result))
                    }
                    OpCode::OpEqualTo => {
                        let obj_type = lhs.object_type();
                        let result = match obj_type {
                            INTERGER_OBJECT_TYPE => {
                                let lhs_value = lhs.as_interger().unwrap();
                                let rhs_value = rhs.as_interger().unwrap();
                                lhs_value == rhs_value
                            }
                            STRING_OBJECT_TYPE => {
                                let lhs_value = lhs.as_string().unwrap();
                                let rhs_value = rhs.as_string().unwrap();
                                lhs_value.len() == rhs_value.len()
                            }
                            BOOLEAN_OBJECT_TYPE => {
                                let lhs_value = lhs.as_boolean().unwrap();
                                let rhs_value = rhs.as_boolean().unwrap();
                                lhs_value == rhs_value
                            }
                            _ => false,
                        };
                        Ok(CompileObject::Boolean(result))
                    }
                    OpCode::OpNotEqualTo => {
                        let obj_type = lhs.object_type();
                        let result = match obj_type {
                            INTERGER_OBJECT_TYPE => {
                                let lhs_value = lhs.as_interger().unwrap();
                                let rhs_value = rhs.as_interger().unwrap();
                                lhs_value != rhs_value
                            }
                            STRING_OBJECT_TYPE => {
                                let lhs_value = lhs.as_string().unwrap();
                                let rhs_value = rhs.as_string().unwrap();
                                lhs_value.len() != rhs_value.len()
                            }
                            BOOLEAN_OBJECT_TYPE => {
                                let lhs_value = lhs.as_boolean().unwrap();
                                let rhs_value = rhs.as_boolean().unwrap();
                                lhs_value != rhs_value
                            }
                            _ => false,
                        };
                        Ok(CompileObject::Boolean(result))
                    }
                    OpCode::OpAND => {
                        let obj_type = lhs.object_type();
                        let result = match obj_type {
                            // conditional AND operation on integers return the most significant interger
                            INTERGER_OBJECT_TYPE => {
                                let lhs_value = lhs.as_interger().unwrap();
                                let rhs_value = rhs.as_interger().unwrap();
                                lhs_value > rhs_value
                            }

                            // conditional AND operation on string return the most significant string length
                            STRING_OBJECT_TYPE => {
                                let lhs_value = lhs.as_string().unwrap();
                                let rhs_value = rhs.as_string().unwrap();
                                lhs_value.len() > rhs_value.len()
                            }
                            BOOLEAN_OBJECT_TYPE => {
                                let lhs_value = lhs.as_boolean().unwrap();
                                let rhs_value = rhs.as_boolean().unwrap();
                                *lhs_value && *rhs_value
                            }
                            _ => false,
                        };
                        Ok(CompileObject::Boolean(result))
                    }
                    OpCode::OpOR => {
                        let obj_type = lhs.object_type();
                        let result = match obj_type {
                            // conditional OR operation on integers return the least significant interger
                            INTERGER_OBJECT_TYPE => {
                                let lhs_value = lhs.as_interger().unwrap();
                                let rhs_value = rhs.as_interger().unwrap();
                                lhs_value < rhs_value
                            }
                            // conditional AND operation on string return the most significant string length
                            STRING_OBJECT_TYPE => {
                                let lhs_value = lhs.as_string().unwrap();
                                let rhs_value = rhs.as_string().unwrap();
                                lhs_value.len() < rhs_value.len()
                            }
                            BOOLEAN_OBJECT_TYPE => {
                                let lhs_value = lhs.as_boolean().unwrap();
                                let rhs_value = rhs.as_boolean().unwrap();
                                *lhs_value || *rhs_value
                            }
                            _ => false,
                        };
                        Ok(CompileObject::Boolean(result))
                    }
                    OpCode::OpLAND => {
                        let obj_type = lhs.object_type();
                        let result = match obj_type {
                            // Bitwise OR operation on integers return the least significant bit
                            INTERGER_OBJECT_TYPE => {
                                let lhs_value = lhs.as_interger().unwrap();
                                let rhs_value = rhs.as_interger().unwrap();
                                lhs_value < rhs_value
                            }
                            // Bitwise OR operation on string return the least significant bit on the string length
                            STRING_OBJECT_TYPE => {
                                let lhs_value = lhs.as_string().unwrap();
                                let rhs_value = rhs.as_string().unwrap();
                                lhs_value.len() < rhs_value.len()
                            }
                            BOOLEAN_OBJECT_TYPE => {
                                let lhs_value = lhs.as_boolean().unwrap();
                                let rhs_value = rhs.as_boolean().unwrap();
                                lhs_value & rhs_value
                            }
                            _ => false,
                        };
                        Ok(CompileObject::Boolean(result))
                    }
                    OpCode::OpLOR => {
                        let obj_type = lhs.object_type();
                        let result = match obj_type {
                            // Bitwise OR operation on integers return the most significant bit
                            INTERGER_OBJECT_TYPE => {
                                let lhs_value = lhs.as_interger().unwrap();
                                let rhs_value = rhs.as_interger().unwrap();
                                lhs_value > rhs_value
                            }
                            // Bitwise OR operation on string return the most significant bit on the string length
                            STRING_OBJECT_TYPE => {
                                let lhs_value = lhs.as_string().unwrap();
                                let rhs_value = rhs.as_string().unwrap();
                                lhs_value.len() > rhs_value.len()
                            }
                            BOOLEAN_OBJECT_TYPE => {
                                let lhs_value = lhs.as_boolean().unwrap();
                                let rhs_value = rhs.as_boolean().unwrap();
                                lhs_value | rhs_value
                            }
                            _ => false,
                        };
                        Ok(CompileObject::Boolean(result))
                    }
                    OpCode::OpBang => {
                        let obj_type = lhs.object_type();
                        let result = match obj_type {
                            INTERGER_OBJECT_TYPE => false,
                            STRING_OBJECT_TYPE => false,
                            BOOLEAN_OBJECT_TYPE => {
                                let lhs_value = lhs.as_boolean().unwrap();
                                !lhs_value
                            }
                            _ => false,
                        };
                        Ok(CompileObject::Boolean(result))
                    }
                    _ => Ok(CompileObject::Boolean(false)),
                }
            }

            OpCode::OpFunctionCaller => {
                let op_instruction = instruction.first().unwrap();
                let binding_name = op_instruction.get(5..op_instruction.len() - 1).unwrap();

                let caller_parameters = instruction.get(1..instruction.len());

                #[allow(clippy::useless_vec)]
                let caller_parameters = caller_parameters
                    .unwrap_or(&vec![vec![OpCode::OpNull as u8]])
                    .to_vec();

                // get the function to execute from symbols table
                let function_definition = self.byte_code.symbols_table.0.get(binding_name).unwrap();
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
                                let param_instructions = param.get(7..param.len() - 1).unwrap();
                                let param_instructions = param_instructions.to_vec();
                                let param_instructions = vec![param_instructions];
                                if let Ok(obj) = self.executor(&param_instructions, params.clone())
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

                debug_println!("caller parameters: {:?}", params);

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
                            let item = item.clone();
                            let item = vec![item];
                            caller_result = self.executor(&item, Some(params.clone()));
                            break;
                        }
                        OpCode::OpNull => {}
                        _ => {
                            let item = item.clone();
                            let item = vec![item];
                            caller_result = self.executor(&item, Some(params.clone()));
                        }
                    }
                }

                caller_result
            }

            // FIXME:
            OpCode::OpBang => panic!(""),

            OpCode::OpNull
            | OpCode::OpMain
            | OpCode::OpFunctionDef
            | OpCode::OpGetFunctionParameter
            | OpCode::OpJumpTo
            | OpCode::OpJumpToAlternate => Ok(CompileObject::Null),
        }
    }
}
