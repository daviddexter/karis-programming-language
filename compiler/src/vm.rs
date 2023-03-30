use std::{cell::RefCell, fs::File, io::Read, rc::Rc};

use borsh::BorshDeserialize;
use errors::errors::KarisError;

use crate::compile::ByteCode;

pub struct VM {
    byte_code: ByteCode,
    pub stack: Rc<RefCell<Vec<u8>>>,
}

impl VM {
    pub fn from_executable_file(file_path: &str) -> Result<VM, KarisError> {
        let mut file = File::open(file_path).expect("unable to open file with error");
        let mut buffer = Vec::<u8>::new();
        file.read_to_end(&mut buffer)?;

        let byte_code = ByteCode::try_from_slice(&buffer).unwrap();
        Ok(Self {
            byte_code,
            stack: Rc::new(RefCell::new(Vec::new())),
        })
    }

    pub fn from_raw_bytecode(byte_code: ByteCode) -> VM {
        Self {
            byte_code,
            stack: Rc::new(RefCell::new(Vec::new())),
        }
    }

    pub fn execute(&self) {
        let instructions = &self.byte_code.instructions;

        for instruction in instructions.iter() {
            println!("{:?}", instruction);
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


        ",
        ));
        let mut parser = Parser::new(lx);
        let ast = parser.parse(Some("should_execute0.json")).unwrap();
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
