use borsh::{BorshDeserialize, BorshSerialize};
use hashbrown::HashMap;

use std::{cell::RefCell, fs::File, io::Write, os::unix::prelude::PermissionsExt, rc::Rc};

use parser::objects::Objects;

use crate::{
    compiler_impls::Compiler,
    defs::{OpCode, Symbol, SymbolResolver, SymbolScope, SymbolStore},
    objects::CompileObject,
};

#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]

pub struct ByteCode {
    pub instructions: Vec<Vec<u8>>,

    pub constants: Vec<CompileObject>,

    pub symbols_table: SymbolResolver,
}

pub struct CompileWorker {
    program_ast: Objects,

    // these are the instructions that the VM will execute
    pub instructions: Rc<RefCell<Vec<Vec<u8>>>>,

    // these are values that don't change. E.g literals
    pub constants: Rc<RefCell<Vec<CompileObject>>>,

    // this hold the the symbols that will be resolved by the VM
    pub symbols_table: Rc<RefCell<SymbolResolver>>,
}

impl CompileWorker {
    pub fn new(ast: Objects) -> CompileWorker {
        CompileWorker {
            program_ast: ast,
            instructions: Rc::new(RefCell::new(Vec::new())),
            constants: Rc::new(RefCell::new(Vec::new())),
            symbols_table: Rc::new(RefCell::new(SymbolResolver::Resolver(SymbolStore(
                HashMap::new(),
            )))),
        }
    }

    pub fn compile(&self) -> ByteCode {
        let worker = Rc::new(RefCell::new(self));
        self.program_ast
            .compile(worker.clone(), SymbolScope::Global);

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

    pub fn emit_opcode_with_parameter(&self, code: OpCode, operand_location: u8) -> Vec<u8> {
        let instructions = vec![code as u8, operand_location];
        instructions
    }

    pub fn emit_opcode_with_vec_parameter(
        &self,
        code: OpCode,
        operand_location: Vec<u8>,
    ) -> Vec<u8> {
        let mut instructions = vec![code as u8];
        for u in operand_location.iter() {
            instructions.push(*u);
        }
        instructions
    }

    pub fn emit_opcode(&self, code: OpCode) -> Vec<u8> {
        // [opcode]
        let instructions = vec![code as u8];
        instructions
    }

    pub fn add_infix(&self, code: OpCode) -> Vec<u8> {
        let instructions = self.emit_opcode(code);
        self.add_instruction(instructions.clone());
        instructions
    }

    pub fn add_constant(&self, obj: CompileObject) -> Vec<u8> {
        let mut consts = self.constants.borrow_mut();
        consts.push(obj);

        let idx = consts.len() - 1;
        let operand_location = idx as u8;
        let instructions = self.emit_opcode_with_parameter(OpCode::OpConstant, operand_location);
        self.add_instruction(instructions.clone());
        instructions
    }

    pub fn add_instruction(&self, instruction: Vec<u8>) {
        let mut instructions = self.instructions.borrow_mut();
        instructions.push(instruction);
    }

    pub fn add_symbol(&self, binding_key_and_scope: Vec<u8>, symbol: Symbol) {
        let mut symbols_table = self.symbols_table.borrow_mut();
        let symbols_table = symbols_table.as_resolver_mut().unwrap();
        symbols_table.0.insert(binding_key_and_scope, symbol);
    }
}

#[cfg(test)]
mod compile_tests {
    use lexer::lexer::Lexer;
    use parser::{objects::Objects, parser::Parser};

    use crate::defs::OpCode;

    use super::CompileWorker;

    #[test]
    fn should_emit_opcode_with_parameter() {
        let worker = CompileWorker::new(Objects::TyUnknown);
        let instruction = worker.emit_opcode_with_parameter(OpCode::OpConstant, 100);
        println!("{:?}", instruction);
        assert_eq!(instruction.len(), 2);
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
        let st = byte_code.symbols_table.as_resolver().unwrap();
        assert_eq!(st.0.len(), 1);
    }
}
