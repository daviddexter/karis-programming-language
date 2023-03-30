use std::{cell::RefCell, rc::Rc};

use either::Either::{self, Left, Right};
use lexer::tokens::IdentifierKind;
use parser::objects::{LiteralObjects, Node, Objects, Program};

use crate::{
    compile::CompileWorker,
    defs::{OpCode, Symbol, SymbolScope},
    objects::CompileObject,
};

pub trait Compiler {
    fn compile(&self, worker: Rc<RefCell<&CompileWorker>>, scope: SymbolScope) -> Option<Vec<u8>>;
}

impl Compiler for Objects {
    fn compile(&self, worker: Rc<RefCell<&CompileWorker>>, scope: SymbolScope) -> Option<Vec<u8>> {
        match self {
            Self::TyProgram(program) => program.compile(worker.clone(), scope),
            Self::TyNode(node) => node.compile(worker.clone(), scope),
            _ => unreachable!("cannot compile UNKNOWN or CONSUMABLE"),
        }
    }
}

impl Compiler for Program {
    fn compile(&self, worker: Rc<RefCell<&CompileWorker>>, scope: SymbolScope) -> Option<Vec<u8>> {
        for item in self.body.iter() {
            item.compile(worker.clone(), scope.clone());
        }

        None
    }
}

impl Compiler for Node {
    fn compile(&self, worker: Rc<RefCell<&CompileWorker>>, scope: SymbolScope) -> Option<Vec<u8>> {
        let kind = self.identifier_kind.unwrap();

        match kind {
            IdentifierKind::INTLITERAL => {
                let lit = self.left_child.as_ref().unwrap().as_ref().left().unwrap();
                let int_value = lit.as_obj_interger_value().unwrap();
                let int_lit = int_value.value.unwrap();

                let obj = CompileObject::Interger(int_lit);
                let wrk = worker.borrow_mut();
                let instructions = wrk.add_constant(obj);
                Some(instructions)
            }

            IdentifierKind::STRINGLITERAL => {
                let lit = self.left_child.as_ref().unwrap().as_ref().left().unwrap();
                let string_value = lit.as_obj_string_value().unwrap();
                let string_lit = string_value.value.as_ref().unwrap();

                let obj = CompileObject::String(string_lit.to_string());
                let wrk = worker.borrow_mut();
                let instructions = wrk.add_constant(obj);
                Some(instructions)
            }

            IdentifierKind::BOOLEANLITERAL => {
                let lit = self.left_child.as_ref().unwrap().as_ref().left().unwrap();
                let bool_value = lit.as_obj_boolean_value().unwrap();
                let bool_lit = bool_value.value.unwrap();

                let obj = CompileObject::Boolean(bool_lit);
                let wrk = worker.borrow_mut();
                let instructions = wrk.add_constant(obj);
                Some(instructions)
            }

            IdentifierKind::PLUS => {
                let left = self.left_child.as_ref().unwrap();
                left_or_right(left, worker.clone(), scope.clone());

                let right = self.right_child.as_ref().unwrap();
                left_or_right(right, worker.clone(), scope);

                let wrk = worker.borrow_mut();
                let instructions = wrk.add_infix(OpCode::OpAdd);
                Some(instructions)
            }

            IdentifierKind::MINUS => {
                let left = self.left_child.as_ref().unwrap();
                left_or_right(left, worker.clone(), scope.clone());

                let right = self.right_child.as_ref().unwrap();
                left_or_right(right, worker.clone(), scope);

                let wrk = worker.borrow_mut();
                let instructions = wrk.add_infix(OpCode::OpMinus);
                Some(instructions)
            }

            IdentifierKind::ASTERISK => {
                let left = self.left_child.as_ref().unwrap();
                left_or_right(left, worker.clone(), scope.clone());

                let right = self.right_child.as_ref().unwrap();
                left_or_right(right, worker.clone(), scope);

                let wrk = worker.borrow_mut();
                let instructions = wrk.add_infix(OpCode::OpMultiply);
                Some(instructions)
            }

            IdentifierKind::SLASH => {
                let left = self.left_child.as_ref().unwrap();
                left_or_right(left, worker.clone(), scope.clone());

                let right = self.right_child.as_ref().unwrap();
                left_or_right(right, worker.clone(), scope);

                let wrk = worker.borrow_mut();
                let instructions = wrk.add_infix(OpCode::OpDivide);
                Some(instructions)
            }

            IdentifierKind::MODULUS => {
                let left = self.left_child.as_ref().unwrap();
                left_or_right(left, worker.clone(), scope.clone());

                let right = self.right_child.as_ref().unwrap();
                left_or_right(right, worker.clone(), scope);

                let wrk = worker.borrow_mut();
                let instructions = wrk.add_infix(OpCode::OpModulus);
                Some(instructions)
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
                if let Some(instructions) = left_or_right(rhs, worker.clone(), scope.clone()) {
                    let wrk = worker.borrow_mut();

                    wrk.add_symbol(
                        binding_key_as_bytes.clone(),
                        Symbol {
                            scope,
                            binding_key: binding_key.clone(),
                            instruction: instructions,
                        },
                    );

                    let insts = wrk.emit_opcode_with_vec_parameter(
                        OpCode::OpSetVariable,
                        binding_key_as_bytes,
                    );

                    wrk.add_instruction(insts);
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
) -> Option<Vec<u8>> {
    match object {
        Left(left) => match left {
            LiteralObjects::ObjIntergerValue(int) => {
                let int_lit = int.value.unwrap();
                let obj = CompileObject::Interger(int_lit);
                let wrk = worker.borrow_mut();
                let instructions = wrk.add_constant(obj);
                Some(instructions)
            }
            LiteralObjects::ObjBooleanValue(bool) => {
                let bool_lit = bool.value.unwrap();
                let obj = CompileObject::Boolean(bool_lit);
                let wrk = worker.borrow_mut();
                let instructions = wrk.add_constant(obj);
                Some(instructions)
            }
            LiteralObjects::ObjStringValue(string) => {
                let string_lit = string.value.as_ref().unwrap();
                let obj = CompileObject::String(string_lit.to_string());
                let wrk = worker.borrow_mut();
                let instructions = wrk.add_constant(obj);
                Some(instructions)
            }
        },
        Right(right) => right.compile(worker, scope),
    }
}
