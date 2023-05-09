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
            let instruction = instruction.clone();
            let instruction = vec![instruction];
            match self.executor(&instruction, None) {
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
                print(sum);
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
    fn should_execute_logical1() {
        let lx = Lexer::new(String::from(
            "
            let greater @bool = fn(x @int, y @int){
                return x > y;
            };

            @main fn(){
                let a @int = 10;
                let verdict @bool = greater(20,10);
                print(verdict);
            }@end;
        ",
        ));
        let mut parser = Parser::new(lx);
        let ast = parser.parse(Some("should_execute_logical1.json")).unwrap();
        let worker = CompileWorker::new(ast);
        let byte_code = worker.compile();
        let vm = VM::from_raw_bytecode(byte_code);
        assert!(vm.execute());
    }

    #[test]
    fn should_execute_logical2() {
        let lx = Lexer::new(String::from(
            "
            let less @bool = fn(x @int, y @int){
                return x < y;
            };

            @main fn(){
                let a @int = 10;
                let verdict @bool = less(a,20);
                print(verdict);
            }@end;
        ",
        ));
        let mut parser = Parser::new(lx);
        let ast = parser.parse(Some("should_execute_logical2.json")).unwrap();
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
                let x @int = 10;
                let y @int = 20;
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
            let factorial @int = fn(n @int){
                if n == 1 {
                    return 1;
                };
                return n * factorial(n - 1);
            };

            @main fn(){
                let result @int = factorial(3);
                print(result);
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
                let list [ @int ] = [ 1, 2 , 3, 100, 1001 ];
                print(list);
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

    #[test]
    fn should_execute11() {
        let lx = Lexer::new(String::from(
            "
            @main fn(){
                let list [ @string ] = [ \"ka\", \"ris\" , \"karis\" ];
                print(list);
            }@end;
        ",
        ));
        let mut parser = Parser::new(lx);
        let ast = parser.parse(Some("should_execute11.json")).unwrap();
        let worker = CompileWorker::new(ast);
        let byte_code = worker.compile();
        let vm = VM::from_raw_bytecode(byte_code);
        assert!(vm.execute());
    }

    #[test]
    fn should_execute12() {
        let lx = Lexer::new(String::from(
            "
            @main fn(){
                let list [ @bool ] = [ true, false , false, true ];
                print(list);
            }@end;
        ",
        ));
        let mut parser = Parser::new(lx);
        let ast = parser.parse(Some("should_execute12.json")).unwrap();
        let worker = CompileWorker::new(ast);
        let byte_code = worker.compile();
        let vm = VM::from_raw_bytecode(byte_code);
        assert!(vm.execute());
    }

    #[test]
    fn should_execute13() {
        let lx = Lexer::new(String::from(
            "
            @main fn(){
                let sum @int = 10 + 20;
                print(sum);
            }@end;
        ",
        ));
        let mut parser = Parser::new(lx);
        let ast = parser.parse(Some("should_execute13.json")).unwrap();
        let worker = CompileWorker::new(ast);
        let byte_code = worker.compile();
        let vm = VM::from_raw_bytecode(byte_code);
        assert!(vm.execute());
    }

    #[test]
    fn should_execute_full_program() {
        let lx = Lexer::new(String::from(
            "
        let add @int = fn(x @int, y @int){
            return x + y;
        };

        let sub @int = fn(x @int, y @int){
            return x - y;
        };

        let mul @int = fn(x @int, y @int){
            return x * y;
        };

        let div @int = fn(x @int, y @int){
            return x * y;
        };

        let mod @int = fn(x @int, y @int){
            return x % y;
        };

        let greater @bool = fn(x @int, y @int){
            return x > y;
        };

        let greater_or_eq @bool = fn(x @int, y @int){
            return x >= y;
        };

        let less @bool = fn(x @int, y @int){
            return x > y;
        };

        let less_or_eq @bool = fn(x @int, y @int){
            return x <= y;
        };

        let or @bool = fn(x @int, y @int){
            return x || y;
        };

        let and @bool = fn(x @int, y @int){
            return x && y;
        };


        @main fn(){
            let x @int = 5;
            let y @int = 7;
            let name @string = \"Karis\";

            let list1 [ @string ] = [ \"ka\", \"ris\" , \"karis\" ];
            let list2 [ @int ] = [ 1, 2 , 3, 100, 1001 ];
            let list3 [ @bool ] = [ true, false , false, true ];

            let result0 @int = add(x,y);
            let result1 @int = sub(x,y);
            let result2 @int = mul(x,y);
            let result3 @int = div(x,y);
            let result4 @int = mod(x,y);
            let result5 @int = greater(x,y);
            let result6 @int = greater_or_eq(x,y);
            let result7 @int = less(x,y);
            let result8 @int = less_or_eq(x,y);
            let result9 @int = or(x,y);
            let result10 @int = and(x,y);

            print(result0);
            print(result1);
            print(result2);
            print(result3);
            print(result4);
            print(result5);
            print(result6);
            print(result7);
            print(result8);
            print(result9);
            print(result10);
        }@end;
        ",
        ));
        let mut parser = Parser::new(lx);
        let ast = parser
            .parse(Some("should_execute_full_program.json"))
            .unwrap();
        let worker = CompileWorker::new(ast);
        let byte_code = worker.compile();
        let vm = VM::from_raw_bytecode(byte_code);
        assert!(vm.execute());
    }
}
