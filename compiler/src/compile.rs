use borsh::{BorshDeserialize, BorshSerialize};
use byteorder::{ByteOrder, LittleEndian};
use hashbrown::HashMap;

use crate::{
    compiler_impls::Compiler,
    defs::{
        OpCode, SymbolScope, SymbolStore, SymbolStoreValue, SymbolsTableTyping, DEFAULT_SCOPE_ID,
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

        let mut global_scope_id = [0; 2];
        LittleEndian::write_u16(&mut global_scope_id, DEFAULT_SCOPE_ID.try_into().unwrap());

        self.program_ast
            .compile(worker.clone(), SymbolScope::Global, global_scope_id);

        let instructions = self.instructions.clone();
        let instructions = instructions.as_ref().clone();
        let instructions = instructions.into_inner();

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
        }
    }

    pub fn write_as_executable(
        &self,
        output_name: &str,
        byte_code: ByteCode,
    ) -> std::io::Result<()> {
        let encoded = byte_code.try_to_vec().unwrap();
        let file_name = format!("{}.bin", output_name);
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

        // add terminal
        instructions.push(OpCode::OpTerminal as u8);

        // add the right
        for r in right.iter() {
            instructions.push(*r)
        }

        // add tail terminal
        instructions.push(OpCode::OpTerminal as u8);

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
        let instructions =
            self.emit_opcode_with_parameter(OpCode::OpConstant, scope, scope_id, operand);
        self.add_instruction(Some(instructions.clone()), None);
        instructions
    }

    pub fn add_variable(&self, scope: SymbolScope, scope_id: [u8; 2], param: Vec<u8>) {
        let insts =
            self.emit_opcode_with_vec_parameter(OpCode::OpSetVariable, scope, scope_id, param);
        self.add_instruction(Some(insts), None);
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

    pub fn add_function(&self, scope_id: [u8; 2]) {
        // [opcode scope scopeid terminal ]
        let instructions =
            self.emit_opcode_with_scopeid(OpCode::OpFunctionDef, SymbolScope::Global, scope_id);

        self.add_instruction(Some(instructions), None)
    }

    pub fn add_caller(&self, scope: SymbolScope, scope_id: [u8; 2], function: Vec<u8>) {
        // [opcode scope scopeid terminal ]
        let mut instructions = self.emit_opcode_with_scopeid(OpCode::OpCallerDef, scope, scope_id);

        for f in function.iter() {
            instructions.push(*f);
        }

        // add tail terminal
        instructions.push(OpCode::OpTerminal as u8);

        self.add_instruction(Some(instructions), None)
    }

    pub fn add_return(&self, scope: SymbolScope, scope_id: [u8; 2], return_insts: Vec<Vec<u8>>) {
        // [opcode scope scopeid terminal return_insts terminal ]
        let return_instructions = return_insts.get(0).unwrap();

        let mut instructions = vec![OpCode::OpReturn as u8, scope as u8];
        for i in scope_id {
            instructions.push(i);
        }
        instructions.push(OpCode::OpTerminal as u8);

        for i in return_instructions {
            instructions.push(*i);
        }

        self.add_instruction(Some(instructions), None)
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
            SymbolScope::Local => self.generate_scope_id(),
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
    fn should_compile0() {
        let lx = Lexer::new(String::from("1"));
        let mut parser = Parser::new(lx);
        let ast = parser.parse(Some("should_compile0.json")).unwrap();
        let worker = CompileWorker::new(ast);
        let byte_code = worker.compile();
        assert_eq!(byte_code.instructions.len(), 1);
    }

    #[test]
    fn should_compile1() {
        let lx = Lexer::new(String::from(
            "
            let num @int = 1;
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

    #[test]
    fn should_compile2() {
        let lx = Lexer::new(String::from(
            "
            let num @int = fn() {
                return 1;
            };
        ",
        ));
        let mut parser = Parser::new(lx);
        let ast = parser.parse(Some("should_compile2.json")).unwrap();
        let worker = CompileWorker::new(ast);
        let byte_code = worker.compile();

        assert_eq!(byte_code.instructions.len(), 4);
        assert_eq!(byte_code.constants.len(), 1);
        let st = byte_code.symbols_table;
        assert_eq!(st.0.len(), 1);
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

        assert_eq!(byte_code.instructions.len(), 7);
        assert_eq!(byte_code.constants.len(), 2);
        let st = byte_code.symbols_table;
        assert_eq!(st.0.len(), 3);
    }
}
