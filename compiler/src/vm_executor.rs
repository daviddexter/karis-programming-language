use crate::vm::VM;
use errors::errors::{KarisError, KarisErrorType};
use itertools::Itertools;
use std::iter::zip;

use crate::{
    defs::{BindingType, CallerParamType, OpCode},
    objects::{CompileObject, BOOLEAN_OBJECT_TYPE, INTERGER_OBJECT_TYPE, STRING_OBJECT_TYPE},
};

impl VM {
    pub(crate) fn executor(
        &self,
        instruction: &Vec<u8>,
        params: Option<Vec<(CompileObject, CompileObject)>>,
    ) -> Result<CompileObject, errors::errors::KarisError> {
        let command = instruction.first().unwrap();
        let command = *command;

        let command: OpCode = command.into();

        match command {
            OpCode::OpTerminal => Ok(CompileObject::Null),

            OpCode::OpAdd
            | OpCode::OpMinus
            | OpCode::OpMultiply
            | OpCode::OpDivide
            | OpCode::OpModulus => {
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
                                | CompileObject::Array(_)
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
                                | CompileObject::Array(_)
                                | CompileObject::Null => false,
                            }
                        })
                        .unwrap();

                    let rhs_value = caller_params.get(rhs_index).unwrap();
                    let rhs_value = &rhs_value.1;
                    let rhs_value = rhs_value.as_interger().unwrap();

                    let result = match command {
                        OpCode::OpAdd => lhs_value + rhs_value,
                        OpCode::OpMinus => lhs_value - rhs_value,
                        OpCode::OpMultiply => lhs_value * rhs_value,
                        OpCode::OpDivide => lhs_value / rhs_value,
                        OpCode::OpModulus => lhs_value % rhs_value,
                        _ => 0_isize,
                    };

                    Ok(CompileObject::Interger(result))
                } else {
                    Ok(CompileObject::Interger(0))
                }
            }

            OpCode::OpGetBinding => {
                let binding_type = instruction.get(5).unwrap();
                let binding_value = *binding_type;
                let binding_type: BindingType = binding_value.into();

                match binding_type {
                    BindingType::Literal => {
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

                    BindingType::Caller => {
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
                                OpCode::OpNull => {}
                                _ => {
                                    caller_result = self.executor(item, Some(params.clone()));
                                }
                            }
                        }

                        caller_result
                    }

                    BindingType::Expression => {
                        let binding_name = instruction.get(7..instruction.len() - 1).unwrap();

                        // get the function to execute from symbols table
                        let expression_symbol =
                            self.byte_code.symbols_table.0.get(binding_name).unwrap();
                        let expression_instructions = &expression_symbol.0;

                        let mut expression_result = Ok(CompileObject::Null);
                        for item in expression_instructions.iter() {
                            expression_result = self.executor(item, params.clone());
                        }

                        expression_result
                    }

                    BindingType::Array => {
                        let binding_name = instruction.get(7..instruction.len() - 1).unwrap();

                        // get the array to execute from symbols table
                        let array_symbol =
                            self.byte_code.symbols_table.0.get(binding_name).unwrap();
                        let array_instructions = &array_symbol.0;

                        Ok(CompileObject::Array(array_instructions.clone()))
                    }
                }
            }

            OpCode::OpReturn => {
                let return_instructions = instruction.get(5..instruction.len() - 1).unwrap();
                let return_instructions = return_instructions.to_vec();
                self.executor(&return_instructions, params)
            }

            OpCode::OpGetCallerParameter => {
                let binding_name = instruction.get(5..instruction.len()).unwrap();

                // get the binding value from symbols table
                let binding_symbol = self.byte_code.symbols_table.0.get(binding_name).unwrap();
                let binding_instructions = &binding_symbol.0;
                let binding_instructions = binding_instructions.get(0).unwrap();
                self.executor(binding_instructions, params)
            }

            OpCode::OpConstant => {
                let location = instruction.get(5).unwrap();
                let location = *location as usize;
                let obj = self.byte_code.constants.get(location).unwrap();
                let obj = obj.clone();
                Ok(obj)
            }

            OpCode::OpAddBuiltin => {
                println!("Add builtin {:?}", instruction);

                let symbol_key = instruction.get(5..instruction.len() - 1).unwrap();

                if let Some(symbol) = self.byte_code.symbols_table.0.get(symbol_key) {
                    let instructions = &symbol.0;
                    let instructions = instructions.get(0).unwrap();
                    return self.executor(instructions, params);
                }

                Ok(CompileObject::Null)
            }

            OpCode::OpPrint => {
                let print_type = instruction.get(5).unwrap();
                let print_value: CallerParamType = CallerParamType::from(*print_type);

                match print_value {
                    CallerParamType::Literal => {
                        let loc = instruction.get(7).unwrap();
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
                        // TODO: revisit this

                        let binding_name = instruction.get(7..instruction.len()).unwrap();
                        if let Some(binding_name) = self.byte_code.symbols_table.0.get(binding_name)
                        {
                            let instructions = &binding_name.0;
                            let instructions = instructions.get(0).unwrap();
                            println!("{:?}", instructions);
                        }
                    }
                };

                Ok(CompileObject::Null)
            }

            OpCode::OpAddIfCondition => {
                let mut result = Ok(CompileObject::Null);

                let condition_binding_name = instruction.get(5..instruction.len() - 1).unwrap();

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
                    let instructions = condition_instructions.get(marker).unwrap();
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
                        | OpCode::OpLOR => match self.executor(instructions, params.clone()) {
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
                        },
                        OpCode::OpJumpTo | OpCode::OpJumpToAlternate => {
                            marker += 1;
                            continue;
                        }
                        OpCode::OpReturn => {
                            result = self.executor(instructions, params);
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

                // we check if the left hand side is argument is a variable or a literal.
                // if a variable, we retrieve it from caller params
                // we then return the a CompileObject

                let variable_or_literal_func = |side_instructions: Vec<u8>,
                                                is_left: bool,
                                                operation_params: Option<
                    Vec<(CompileObject, CompileObject)>,
                >|
                 -> CompileObject {
                    let command = side_instructions.first().unwrap();
                    let command = *command;
                    let command: OpCode = command.into();

                    if command == OpCode::OpGetBinding || command == OpCode::OpGetCallerParameter {
                        if let Some(caller_params) = operation_params {
                            let side_index = caller_params
                                .iter()
                                .position(|cp| {
                                    let obj = &cp.0;
                                    match obj {
                                        CompileObject::Variable(var) => {
                                            if is_left {
                                                let left_binding =
                                                    left.get(5..left.len() - 1).unwrap();
                                                var == left_binding
                                            } else {
                                                let right_binding =
                                                    right.get(5..left.len() - 1).unwrap();
                                                var == right_binding
                                            }
                                        }
                                        CompileObject::Interger(_)
                                        | CompileObject::String(_)
                                        | CompileObject::Boolean(_)
                                        | CompileObject::Array(_)
                                        | CompileObject::Null => false,
                                    }
                                })
                                .unwrap();

                            let side_value = caller_params.get(side_index).unwrap();
                            let side_value = &side_value.1;
                            let side_value = side_value.clone();
                            return side_value;
                        }
                    }

                    if command == OpCode::OpConstant {
                        let constant_index = side_instructions.get(5).unwrap();
                        let side_value = self
                            .byte_code
                            .constants
                            .get(*constant_index as usize)
                            .unwrap();
                        let side_value = side_value.clone();
                        return side_value;
                    }

                    CompileObject::Null
                };

                let lhs = variable_or_literal_func(left.to_vec(), true, params.clone());
                let rhs = variable_or_literal_func(right.to_vec(), false, params.clone());

                if lhs.object_type() != rhs.object_type() {
                    return Err(KarisError {
                        error_type: KarisErrorType::InvalidExecution,
                        message: "Comparison type mismatch".to_string(),
                    });
                }

                let result = match command {
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
                        result
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
                        result
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
                        result
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
                        result
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
                        result
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
                        result
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
                        result
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
                        result
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
                        result
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
                        result
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
                        result
                    }
                    _ => false,
                };

                Ok(CompileObject::Boolean(result))
            }

            // FIXME:
            OpCode::OpBang => panic!(""),

            OpCode::OpNull
            | OpCode::OpMain
            | OpCode::OpFunctionDef
            | OpCode::OpCallerDef
            | OpCode::OpGetFunctionParameter
            | OpCode::OpJumpTo
            | OpCode::OpJumpToAlternate => Ok(CompileObject::Null),
        }
    }
}
