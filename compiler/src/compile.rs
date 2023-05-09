use borsh::{BorshDeserialize, BorshSerialize};
use byteorder::{ByteOrder, LittleEndian};
use hashbrown::HashMap;

use crate::{
    compiler_impls::Compiler,
    defs::{
        BindingType, OpCode, SymbolScope, SymbolStore, SymbolStoreValue, SymbolsTableTyping,
        DEFAULT_SCOPE_ID,
    },
    objects::CompileObject,
};
use parser::objects::Objects;
use std::{cell::RefCell, fs::File, io::Write, os::unix::prelude::PermissionsExt, rc::Rc};

#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]

pub struct ByteCode {
    pub instructions: Vec<Vec<u8>>,

    pub constants: Vec<CompileObject>,

    pub symbols_table: SymbolStore<Vec<u8>, SymbolStoreValue<u8>>,

    pub global_scope_id: [u8; 2],
}

pub struct CompileWorker {
    program_ast: Objects,

    // these are the instructions that the VM will execute
    pub instructions: Rc<RefCell<Vec<Vec<u8>>>>,

    // these are values that don't change. E.g literals
    pub constants: Rc<RefCell<Vec<CompileObject>>>,

    // this hold the the scoped symbols that will be resolved by the VM.
    // this symbol_table will by default be the GLOBAL scope. In some cases
    // however, it will point to a local scope. Such cases are in `@main` block
    // and function blocks
    pub symbols_table: SymbolsTableTyping,

    pub scope_id_counter: Rc<RefCell<usize>>,
}

impl CompileWorker {
    pub fn new(ast: Objects) -> CompileWorker {
        CompileWorker {
            program_ast: ast,
            instructions: Rc::new(RefCell::new(Vec::new())),
            constants: Rc::new(RefCell::new(Vec::new())),
            symbols_table: Rc::new(RefCell::new(SymbolStore(HashMap::new()))),
            scope_id_counter: Rc::new(RefCell::new(DEFAULT_SCOPE_ID)),
        }
    }

    pub fn compile(&self) -> ByteCode {
        let worker = Rc::new(RefCell::new(self));

        let global_scope_id = self.global_scope_id();

        self.program_ast
            .compile(worker.clone(), SymbolScope::Global, global_scope_id);

        let instructions = self.instructions.clone();
        let instructions = instructions.as_ref().clone();
        let instructions = instructions.into_inner();

        // check that there is a main instruction. If not, end the program with an error
        let mut main_count = 0;
        instructions.iter().for_each(|item| {
            item.iter().for_each(|val| {
                let val = *val;
                let op: OpCode = val.into();
                if op == OpCode::OpMain {
                    main_count += 1;
                }
            })
        });

        if main_count == 1 {
            let constants = self.constants.clone();
            let constants = constants.as_ref().clone();
            let constants = constants.into_inner();

            let symbols_table = self.symbols_table.clone();
            let symbols_table = symbols_table.as_ref().clone();
            let symbols_table = symbols_table.into_inner();

            ByteCode {
                instructions,
                constants,
                symbols_table,
                global_scope_id,
            }
        } else {
            panic!("Program main function not found in scope")
        }
    }

    pub fn global_scope_id(&self) -> [u8; 2] {
        let mut global_scope_id = [0; 2];
        LittleEndian::write_u16(&mut global_scope_id, DEFAULT_SCOPE_ID.try_into().unwrap());
        global_scope_id
    }

    pub fn write_as_executable(
        &self,
        output_name: &str,
        byte_code: ByteCode,
    ) -> std::io::Result<()> {
        let encoded = byte_code.try_to_vec().unwrap();
        let file_name = format!("{}.krbin", output_name);
        let mut file = File::create(file_name)?;

        let mut perms = file.metadata()?.permissions();
        perms.set_mode(0o700);
        file.set_permissions(perms)?;

        file.write_all(&encoded)?;

        Ok(())
    }

    fn emit_opcode_with_parameter(
        &self,
        code: OpCode,
        scope: SymbolScope,
        scope_id: [u8; 2],
        operand: u8,
    ) -> Vec<u8> {
        // [opcode scope scopeid terminal operand terminal ]
        let mut instructions = vec![code as u8, scope as u8];

        // add scope id
        for id in scope_id.iter() {
            instructions.push(*id);
        }

        // add terminal
        instructions.push(OpCode::OpTerminal as u8);

        // add operand
        instructions.push(operand);

        // add tail terminal
        instructions.push(OpCode::OpTerminal as u8);

        instructions
    }

    fn emit_opcode_with_vec_parameter(
        &self,
        code: OpCode,
        scope: SymbolScope,
        scope_id: [u8; 2],
        param: Vec<u8>,
    ) -> Vec<u8> {
        // [opcode scope scopeid terminal params terminal]
        let mut instructions = vec![code as u8, scope as u8];

        // add scope id
        for id in scope_id.iter() {
            instructions.push(*id);
        }

        // add terminal
        instructions.push(OpCode::OpTerminal as u8);

        // add parameters
        for u in param.iter() {
            instructions.push(*u);
        }

        // add tail terminal
        instructions.push(OpCode::OpTerminal as u8);

        instructions
    }

    fn emit_opcode_for_binding(
        &self,
        code: OpCode,
        scope: SymbolScope,
        scope_id: [u8; 2],
        binding_type: u8,
        binding_name: Vec<u8>,
    ) -> Vec<u8> {
        // [opcode scope scopeid terminal binding_type terminal binding_name terminal]
        let mut instructions = vec![code as u8, scope as u8];

        // add scope id
        for id in scope_id.iter() {
            instructions.push(*id);
        }

        // add terminal
        instructions.push(OpCode::OpTerminal as u8);

        // add binding_type
        instructions.push(binding_type);

        // add terminal
        instructions.push(OpCode::OpTerminal as u8);

        // add binding_name
        for u in binding_name.iter() {
            instructions.push(*u);
        }

        // add tail terminal
        instructions.push(OpCode::OpTerminal as u8);

        instructions
    }

    fn emit_opcode_for_builtin(
        &self,
        code: OpCode,
        scope: SymbolScope,
        scope_id: [u8; 2],
        binding_name: Vec<u8>,
    ) -> Vec<u8> {
        // [opcode scope scopeid terminal binding_name terminal]
        let mut instructions = vec![code as u8, scope as u8];

        // add scope id
        for id in scope_id.iter() {
            instructions.push(*id);
        }

        // add terminal
        instructions.push(OpCode::OpTerminal as u8);

        // add binding_name
        for u in binding_name.iter() {
            instructions.push(*u);
        }

        // add tail terminal
        instructions.push(OpCode::OpTerminal as u8);

        instructions
    }

    fn emit_opcode_for_caller(
        &self,
        code: OpCode,
        scope: SymbolScope,
        scope_id: [u8; 2],
        param_type: u8,
        values: Vec<u8>,
    ) -> Vec<u8> {
        // [opcode scope scopeid terminal param_type terminal values terminal ]
        let mut instructions = vec![code as u8, scope as u8];

        // add scope id
        for id in scope_id.iter() {
            instructions.push(*id);
        }

        // add terminal
        instructions.push(OpCode::OpTerminal as u8);

        // add param type
        instructions.push(param_type);

        // add terminal
        instructions.push(OpCode::OpTerminal as u8);

        // add parameters
        for u in values.iter() {
            instructions.push(*u);
        }

        // add tail terminal
        instructions.push(OpCode::OpTerminal as u8);

        instructions
    }

    fn emit_opcode_with_scopeid(
        &self,
        code: OpCode,
        scope: SymbolScope,
        scope_id: [u8; 2],
    ) -> Vec<u8> {
        // [opcode scope scopeid terminal]
        let mut instructions = vec![code as u8, scope as u8];

        // add scope id
        for id in scope_id.iter() {
            instructions.push(*id);
        }

        // add tail terminal
        instructions.push(OpCode::OpTerminal as u8);

        instructions
    }

    fn emit_opcode(&self, code: OpCode) -> Vec<u8> {
        // [opcode terminal]
        let mut instructions = vec![code as u8];

        // add tail terminal
        instructions.push(OpCode::OpTerminal as u8);

        instructions
    }

    fn add_instruction(&self, instructions0: Option<Vec<u8>>, instructions1: Option<Vec<Vec<u8>>>) {
        if let Some(inst) = instructions0 {
            let mut instructions = self.instructions.borrow_mut();
            instructions.push(inst);
            return;
        }

        // for return operations
        if let Some(inst) = instructions1 {
            let mut instructions = self.instructions.borrow_mut();

            for i in inst.iter() {
                instructions.push(i.to_vec());
            }
        }
    }
}

// these are public methods add instructions and symbols to the worker
impl CompileWorker {
    pub fn add_main(&self) {
        let mut instructions = self.instructions.borrow_mut();
        instructions.push(vec![OpCode::OpMain as u8]);
    }

    pub fn append_to_constant_pool(&self, obj: CompileObject) -> u8 {
        let mut consts = self.constants.borrow_mut();
        consts.push(obj);

        let idx = consts.len() - 1;
        idx as u8
    }

    pub fn add_infix(&self, code: OpCode, left: Vec<u8>, right: Vec<u8>) -> Vec<u8> {
        // [opcode terminal left terminal right terminal]
        let mut instructions = self.emit_opcode(code);

        // add the left
        for l in left.iter() {
            instructions.push(*l)
        }

        // add separator
        instructions.push(OpCode::OpNull as u8);

        // add the right
        for r in right.iter() {
            instructions.push(*r)
        }

        // we don't add the instructions into the CompileWorker instructions vector.
        // Typically these instructions are RETURNED or PRINTED, hence they will be consumed by
        // those OpCodes
        instructions
    }

    pub fn add_constant(
        &self,
        scope: SymbolScope,
        scope_id: [u8; 2],
        obj: CompileObject,
    ) -> Vec<u8> {
        let operand = self.append_to_constant_pool(obj);
        // constant instructions should not be added to the instructions vector. The binding to this constant will set the appropriate
        // instruction to access the constant literal
        self.emit_opcode_with_parameter(OpCode::OpConstant, scope, scope_id, operand)
    }

    // we pass the instruction `OpGetBinding`. Look at it from the VM's perspective. When a variable or an expression is needed it,
    // must fetch it from somewhere, either the constant pool or symboltable
    pub fn add_variable_binding(
        &self,
        scope: SymbolScope,
        scope_id: [u8; 2],
        binding_type: BindingType,
        binding_name: Vec<u8>,
    ) -> Vec<u8> {
        let insts = self.emit_opcode_for_binding(
            OpCode::OpGetBinding,
            scope,
            scope_id,
            binding_type as u8,
            binding_name,
        );

        self.add_instruction(Some(insts.clone()), None);
        insts
    }

    pub fn add_builtin_instructions(&self, insts: Vec<u8>) {
        self.add_instruction(Some(insts), None);
    }

    pub fn instructions_for_condition_statement(
        &self,
        op: OpCode,
        scope: SymbolScope,
        scope_id: [u8; 2],
        binding_name: Vec<u8>,
    ) -> Vec<u8> {
        self.emit_opcode_for_builtin(op, scope, scope_id, binding_name)
    }

    pub fn instructions_for_builtin(
        &self,
        scope: SymbolScope,
        scope_id: [u8; 2],
        binding_name: Vec<u8>,
    ) -> Vec<u8> {
        self.emit_opcode_for_builtin(OpCode::OpAddBuiltin, scope, scope_id, binding_name)
    }

    pub fn add_symbol(&self, binding_key: Vec<u8>, symbol: Vec<Vec<u8>>) {
        let mut symbols_table = self.symbols_table.borrow_mut();
        symbols_table
            .0
            .insert(binding_key, SymbolStoreValue(symbol));
    }

    pub fn get_symbol(&self, binding_key: Vec<u8>) -> Option<SymbolStoreValue<u8>> {
        let symbols_table = self.symbols_table.borrow();
        if let Some(sy) = symbols_table.0.get(&binding_key) {
            let symbol = sy.clone();
            Some(symbol)
        } else {
            None
        }
    }

    pub fn instructions_for_caller(
        &self,
        scope: SymbolScope,
        scope_id: [u8; 2],
        function_name: Vec<u8>,
    ) -> Vec<u8> {
        // [opcode scope scopeid terminal func_name]
        let mut instructions = self.emit_opcode_with_scopeid(OpCode::OpCallerDef, scope, scope_id);

        for f in function_name.iter() {
            instructions.push(*f);
        }

        // add tail terminal
        instructions.push(OpCode::OpTerminal as u8);

        instructions
    }

    pub fn instructions_for_return(
        &self,
        scope: SymbolScope,
        scope_id: [u8; 2],
        operations_insts: Vec<Vec<u8>>,
    ) -> Vec<u8> {
        // [opcode scope scopeid terminal return_insts terminal ]
        let operations_insts = operations_insts.get(0).unwrap();

        let mut instructions = vec![OpCode::OpReturn as u8, scope as u8];
        for i in scope_id {
            instructions.push(i);
        }
        instructions.push(OpCode::OpTerminal as u8);

        for i in operations_insts {
            instructions.push(*i);
        }

        instructions
    }
}

// these methods generate things that the CompilerWorker needs to do its work well
impl CompileWorker {
    pub fn generate_scope_counter(&self) -> usize {
        let scope_counter = self.scope_id_counter.clone();
        let mut scope_counter = scope_counter.borrow_mut();
        let new_scope_counter = *scope_counter + 1;
        *scope_counter = new_scope_counter;
        new_scope_counter
    }

    pub fn generate_scope_id(&self) -> [u8; 2] {
        let scope_counter = self.generate_scope_counter();
        let mut scope_id = [0; 2];
        LittleEndian::write_u16(&mut scope_id, scope_counter.try_into().unwrap());
        scope_id
    }

    pub fn generate_scope_id_from_scope(&self, scope: SymbolScope) -> [u8; 2] {
        match scope {
            SymbolScope::Global => {
                let mut scope_id = [0; 2];
                LittleEndian::write_u16(&mut scope_id, DEFAULT_SCOPE_ID.try_into().unwrap());
                scope_id
            }
            SymbolScope::Local | SymbolScope::Main => self.generate_scope_id(),
        }
    }

    pub fn generate_opcode(&self, opcode: OpCode) -> Vec<u8> {
        self.emit_opcode(opcode)
    }

    pub fn generate_opcode_with_scope_and_vec_parameter(
        &self,
        opcode: OpCode,
        scope: SymbolScope,
        scope_id: [u8; 2],
        parameter: Vec<u8>,
    ) -> Vec<u8> {
        self.emit_opcode_with_vec_parameter(opcode, scope, scope_id, parameter)
    }

    pub fn generate_opcode_for_caller(
        &self,
        opcode: OpCode,
        scope: SymbolScope,
        scope_id: [u8; 2],
        param_type: u8,
        values: Vec<u8>,
    ) -> Vec<u8> {
        self.emit_opcode_for_caller(opcode, scope, scope_id, param_type, values)
    }
}

#[cfg(test)]
mod compile_tests {
    use byteorder::{ByteOrder, LittleEndian};
    use lexer::lexer::Lexer;
    use parser::{objects::Objects, parser::Parser};

    use crate::{
        compile::DEFAULT_SCOPE_ID,
        defs::{OpCode, SymbolScope},
    };

    use super::CompileWorker;

    #[test]
    fn should_increment_counter() {
        let worker = CompileWorker::new(Objects::TyUnknown);
        let counter = worker.generate_scope_counter();
        assert!(counter.eq(&1001));

        let counter = worker.generate_scope_counter();
        assert!(counter.eq(&1002));

        let counter = worker.generate_scope_counter();
        assert!(counter.eq(&1003));
    }

    #[test]
    fn should_emit_opcode_with_parameter() {
        let worker = CompileWorker::new(Objects::TyUnknown);
        let mut scope_id = [0; 2];
        LittleEndian::write_u16(&mut scope_id, DEFAULT_SCOPE_ID.try_into().unwrap());

        let instruction = worker.emit_opcode_with_parameter(
            OpCode::OpConstant,
            SymbolScope::Global,
            scope_id,
            100,
        );
        assert_eq!(instruction.len(), 7);
    }

    #[test]
    fn should_compile1() {
        let lx = Lexer::new(String::from(
            "
            @main fn(){
                let num @int = 1;
            }@end;
        ",
        ));
        let mut parser = Parser::new(lx);
        let ast = parser.parse(Some("should_compile1.json")).unwrap();
        let worker = CompileWorker::new(ast);
        let byte_code = worker.compile();

        assert_eq!(byte_code.instructions.len(), 2);
        assert_eq!(byte_code.constants.len(), 1);
        let st = byte_code.symbols_table;

        assert_eq!(st.0.len(), 1);
    }

    // this test shows that a literal must be first assigned to a variable before returning it.
    // this is a language design decision to ensure that a value has occupied some space in memory before returning it
    #[test]
    fn should_compile2() {
        let lx = Lexer::new(String::from(
            "
            let num @int = fn() {
                let one @int = 1;
                return one;
            };

            @main fn (){
                let num0 @int = num();
            }@end;

        ",
        ));
        let mut parser = Parser::new(lx);
        let ast = parser.parse(Some("should_compile2.json")).unwrap();
        let worker = CompileWorker::new(ast);
        let byte_code = worker.compile();

        assert_eq!(byte_code.instructions.len(), 3);
        assert_eq!(byte_code.constants.len(), 1);
        let st = byte_code.symbols_table;
        assert_eq!(st.0.len(), 3);
    }

    #[test]
    fn should_compile3() {
        let lx = Lexer::new(String::from(
            "
            let summation @int = fn(x @int, y @int) {
                return x + y;
            };

            @main fn(){
                let a @int = 10;
                let sum @int = summation(a, 20);
            }@end;
        ",
        ));
        let mut parser = Parser::new(lx);
        let ast = parser.parse(Some("should_compile3.json")).unwrap();
        let worker = CompileWorker::new(ast);
        let byte_code = worker.compile();

        assert_eq!(byte_code.instructions.len(), 3);
        assert_eq!(byte_code.constants.len(), 2);
        let st = byte_code.symbols_table;
        assert_eq!(st.0.len(), 3);
    }

    #[test]
    fn should_compile4() {
        let lx = Lexer::new(String::from(
            "
            let num @int = fn() {
                let ten @int = 10;
                print(ten);
                print(1);
                return ten;
            };

            @main fn(){
                num();
            }@end;
        ",
        ));
        let mut parser = Parser::new(lx);
        let ast = parser.parse(Some("should_compile4.json")).unwrap();
        let worker = CompileWorker::new(ast);
        let byte_code = worker.compile();

        assert_eq!(byte_code.instructions.len(), 4);
        assert_eq!(byte_code.constants.len(), 2);
        let st = byte_code.symbols_table;
        assert_eq!(st.0.len(), 4);
    }

    #[test]
    fn should_compile4a() {
        let lx = Lexer::new(String::from(
            "
            @main fn(){
                let val @bool = !!true;
            }@end;
        ",
        ));
        let mut parser = Parser::new(lx);
        let ast = parser.parse(Some("should_compile4a.json")).unwrap();
        let worker = CompileWorker::new(ast);
        let byte_code = worker.compile();

        assert_eq!(byte_code.instructions.len(), 2);
        assert_eq!(byte_code.constants.len(), 1);
        let st = byte_code.symbols_table;
        assert_eq!(st.0.len(), 1);
    }

    #[test]
    fn should_compile4b() {
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

            @main fn(){
                minmax_or_product();
            }@end;
        ",
        ));
        let mut parser = Parser::new(lx);
        let ast = parser.parse(Some("should_compile4b.json")).unwrap();
        let worker = CompileWorker::new(ast);
        let byte_code = worker.compile();

        assert_eq!(byte_code.instructions.len(), 1);
        assert_eq!(byte_code.constants.len(), 0);
        let st = byte_code.symbols_table;
        assert_eq!(st.0.len(), 2);
    }

    #[test]
    fn should_compile5() {
        let lx = Lexer::new(String::from(
            "
            let minmax_or_product @int = fn(x @int, y @int){
                if x < y{
                   return x + y;
                }else x == y{
                    return x - y;
                };

                return x * y;
            };

            @main fn(){
                let minmax @int = minmax_or_product(10, 20);
            }@end;
        ",
        ));
        let mut parser = Parser::new(lx);
        let ast = parser.parse(Some("should_compile5.json")).unwrap();
        let worker = CompileWorker::new(ast);
        let byte_code = worker.compile();

        assert_eq!(byte_code.instructions.len(), 2);
        assert_eq!(byte_code.constants.len(), 2);
        let st = byte_code.symbols_table;
        assert_eq!(st.0.len(), 3);
    }

    #[test]
    fn should_compile6() {
        let lx = Lexer::new(String::from(
            "
            let multi_conditions @int = fn(x @int, y @int){
                if x < 3 {
                    let x1 @int = x + 5;
                    let y @int = y + 10;
                    return x1 + y;
                }else x > y {
                    return x - y;
                } else {
                    return x * y;
                };
            };

            @main fn(){
                let val @int = multi_conditions(10, 20);
                print(val);
            }@end;
        ",
        ));
        let mut parser = Parser::new(lx);
        let ast = parser.parse(Some("should_compile6.json")).unwrap();
        let worker = CompileWorker::new(ast);
        let byte_code = worker.compile();

        assert_eq!(byte_code.instructions.len(), 5);
        assert_eq!(byte_code.constants.len(), 5);
        let st = byte_code.symbols_table;
        assert_eq!(st.0.len(), 6);
    }

    #[test]
    fn should_compile9() {
        let lx = Lexer::new(String::from(
            "
            let multi_conditions @int = fn(x @int, y @int){
                if x < 3 {
                    let x1 @int = x + 5;
                    let y @int = y + 10;
                    return x1 + y;
                }else x > y {
                    return x - y;
                } else {
                    return x * y;
                };
            };

            @main fn(){
                let val @int = multi_conditions(10, 20);
                print(val);
            }@end;
        ",
        ));
        let mut parser = Parser::new(lx);
        let ast = parser.parse(Some("should_compile9.json")).unwrap();
        let worker = CompileWorker::new(ast);
        let byte_code = worker.compile();

        assert_eq!(byte_code.instructions.len(), 5);
        assert_eq!(byte_code.constants.len(), 5);
        let st = byte_code.symbols_table;
        assert_eq!(st.0.len(), 6);
    }

    #[test]
    fn should_compile10() {
        let lx = Lexer::new(String::from(
            "
            let downer @int = fn(n @int){
                if n == 0 {
                    return 0;
                };
                let n0 @int = n - 1;
                return downer(n0);
            };

            @main fn(){
                let result @int = downer(3);
            }@end;
        ",
        ));
        let mut parser = Parser::new(lx);
        let ast = parser.parse(Some("should_compile10.json")).unwrap();
        let worker = CompileWorker::new(ast);
        let byte_code = worker.compile();

        assert_eq!(byte_code.instructions.len(), 3);
        assert_eq!(byte_code.constants.len(), 4);
        let st = byte_code.symbols_table;
        assert_eq!(st.0.len(), 4);
    }

    #[test]
    fn should_compile11() {
        let lx = Lexer::new(String::from(
            "
            @main fn(){
                let items [ @int ] = [ 1, 2, 3 ];
                print(items);
            }@end;
        ",
        ));
        let mut parser = Parser::new(lx);
        let ast = parser.parse(Some("should_compile11.json")).unwrap();
        let worker = CompileWorker::new(ast);
        let byte_code = worker.compile();

        assert_eq!(byte_code.instructions.len(), 3);
        assert_eq!(byte_code.constants.len(), 3);
        let st = byte_code.symbols_table;
        assert_eq!(st.0.len(), 2);
    }
}
