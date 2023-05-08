use crate::compile::ByteCode;
use borsh::BorshDeserialize;
use errors::errors::KarisError;
use std::{fs::File, io::Read};

pub struct VM {
    pub(crate) byte_code: ByteCode,
}

impl VM {
    pub fn from_executable_file(file_path: &str) -> Result<VM, KarisError> {
        let mut file = File::open(file_path).expect("unable to open file with error");
        let mut buffer = Vec::<u8>::new();
        file.read_to_end(&mut buffer)?;

        let byte_code = ByteCode::try_from_slice(&buffer).unwrap();
        Ok(Self { byte_code })
    }

    pub fn from_raw_bytecode(byte_code: ByteCode) -> VM {
        Self { byte_code }
    }

    pub fn execute(&self) -> bool {
        let mut result = true;

        let instructions = &self.byte_code.instructions;

        for instruction in instructions.iter() {
            match self.executor(instruction, None) {
                Ok(_) => continue,
                Err(err) => {
                    eprintln!("{}", err);
                    result = false;
                    break;
                }
            }
        }

        result
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
                let ten @int = 10;
                let sum @int = summation(ten,20);
            }@end;
        ",
        ));
        let mut parser = Parser::new(lx);
        let ast = parser.parse(Some("should_execute0.json")).unwrap();
        let worker = CompileWorker::new(ast);
        let byte_code = worker.compile();
        let vm = VM::from_raw_bytecode(byte_code);
        assert!(vm.execute());
    }

    #[test]
    fn should_execute0b() {
        let lx = Lexer::new(String::from(
            "
            @main fn(){
                let sum @int = 10 + 20;
            }@end;
        ",
        ));
        let mut parser = Parser::new(lx);
        let ast = parser.parse(Some("should_execute0b.json")).unwrap();
        let worker = CompileWorker::new(ast);
        let byte_code = worker.compile();
        let vm = VM::from_raw_bytecode(byte_code);
        assert!(vm.execute());
    }

    #[test]
    fn should_execute1() {
        let lx = Lexer::new(String::from(
            "
            let summation @int = fn(x @int, y @int) {
                print(x);
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
        assert!(vm.execute());
    }

    #[test]
    fn should_execute2() {
        let lx = Lexer::new(String::from(
            "
            let minus @int = fn(x @int, y @int) {
                print(x);
                return x - y;
            };

            @main fn(){
                let a @int = 10;
                let sub @int = minus(a,20);
            }@end;
        ",
        ));
        let mut parser = Parser::new(lx);
        let ast = parser.parse(Some("should_execute2.json")).unwrap();
        let worker = CompileWorker::new(ast);
        let byte_code = worker.compile();
        let vm = VM::from_raw_bytecode(byte_code);
        assert!(vm.execute());
    }

    #[test]
    fn should_execute3() {
        let lx = Lexer::new(String::from(
            "
            let multiply @int = fn(x @int, y @int) {
                print(x);
                return x * y;
            };

            @main fn(){
                let a @int = 10;
                let mul @int = multiply(a,20);
            }@end;
        ",
        ));
        let mut parser = Parser::new(lx);
        let ast = parser.parse(Some("should_execute3.json")).unwrap();
        let worker = CompileWorker::new(ast);
        let byte_code = worker.compile();
        let vm = VM::from_raw_bytecode(byte_code);
        assert!(vm.execute());
    }

    #[test]
    fn should_execute4() {
        let lx = Lexer::new(String::from(
            "
            let divide @int = fn(x @int, y @int) {
                print(x);
                return x / y;
            };

            @main fn(){
                let a @int = 10;
                let div @int = divide(a,20);
            }@end;
        ",
        ));
        let mut parser = Parser::new(lx);
        let ast = parser.parse(Some("should_execute4.json")).unwrap();
        let worker = CompileWorker::new(ast);
        let byte_code = worker.compile();
        let vm = VM::from_raw_bytecode(byte_code);
        assert!(vm.execute());
    }

    #[test]
    fn should_execute5() {
        let lx = Lexer::new(String::from(
            "
            let modulus @int = fn(x @int, y @int) {
                print(x);
                return x / y;
            };

            @main fn(){
                let a @int = 10;
                let mod @int = modulus(a,20);
            }@end;
        ",
        ));
        let mut parser = Parser::new(lx);
        let ast = parser.parse(Some("should_execute5.json")).unwrap();
        let worker = CompileWorker::new(ast);
        let byte_code = worker.compile();
        let vm = VM::from_raw_bytecode(byte_code);
        assert!(vm.execute());
    }

    #[test]
    fn should_execute6() {
        let lx = Lexer::new(String::from(
            "
            let multi_conditions @int = fn(x @int, y @int){
                if 3 > x {
                    return x + y;
                }else x > y {
                    return x - y;
                } else {
                    return x * y;
                };
            };

            @main fn(){
                let result1 @int = multi_conditions(10, 20);
                print(result1);
            }@end;
        ",
        ));
        let mut parser = Parser::new(lx);
        let ast = parser.parse(Some("should_execute6.json")).unwrap();
        let worker = CompileWorker::new(ast);
        let byte_code = worker.compile();
        let vm = VM::from_raw_bytecode(byte_code);
        assert!(vm.execute());
    }

    #[test]
    #[should_panic]
    fn should_not_execute7() {
        let lx = Lexer::new(String::from(
            "
            @main fn(){
                let val @bool = !!true;
                print(val);
            }@end;
        ",
        ));
        let mut parser = Parser::new(lx);
        let ast = parser.parse(Some("should_execute7.json")).unwrap();
        let worker = CompileWorker::new(ast);
        let byte_code = worker.compile();
        let vm = VM::from_raw_bytecode(byte_code);
        vm.execute();
    }

    // FIXME: recursive function calls
    #[test]
    #[should_panic]
    fn should_execute8() {
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
        let ast = parser.parse(Some("should_execute8.json")).unwrap();
        let worker = CompileWorker::new(ast);
        let byte_code = worker.compile();
        let vm = VM::from_raw_bytecode(byte_code);
        assert!(vm.execute());
    }

    #[test]
    fn should_execute9() {
        let lx = Lexer::new(String::from(
            "
            @main fn(){
                let num @int = 1;
                print(items);
            }@end;
        ",
        ));
        let mut parser = Parser::new(lx);
        let ast = parser.parse(Some("should_execute9.json")).unwrap();
        let worker = CompileWorker::new(ast);
        let byte_code = worker.compile();
        let vm = VM::from_raw_bytecode(byte_code);
        assert!(vm.execute());
    }
}
