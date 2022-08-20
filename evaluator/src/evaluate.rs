use colored::*;
use Either::Left;

use either::Either;
use lexer::tokens::IdentifierKind;
use parser::objects::{LiteralObjects, Node, Objects, Program};
use parser::parser::Parser;

type Cache = hashbrown::HashMap<String, Either<LiteralObjects, Objects>>;

pub trait Evaluate {
    fn eval(&self, cache: &mut Cache);
}

pub struct Evaluator {
    parser: Parser,
    cache: Cache,
}

impl Evaluator {
    pub fn new(parser: Parser) -> Self {
        Self {
            parser,
            cache: hashbrown::HashMap::new(),
        }
    }

    pub fn repl_evaluate_program(&mut self) {
        match self.parser.parse(Some("repl_evaluate_program.json")) {
            Ok(program) => {
                program.eval(&mut self.cache);
            }
            Err(err) => println!("{}", err.to_string().red()),
        }
    }
}

impl Evaluate for Objects {
    fn eval(&self, cache: &mut Cache) {
        match self {
            Self::TyProgram(program) => program.eval(cache),
            Self::TyNode(node) => node.eval(cache),
            _ => unreachable!("cannot evaluate UNKNOWN or CONSUMABLE"),
        }
    }
}

impl Evaluate for Program {
    fn eval(&self, _cache: &mut Cache) {}
}

impl Evaluate for Node {
    fn eval(&self, cache: &mut Cache) {
        let kind = self.identifier_kind.unwrap();
        match kind {
            IdentifierKind::INTLITERAL
            | IdentifierKind::STRINGLITERAL
            | IdentifierKind::BOOLEANLITERAL => {
                let lit = self.left_child.as_ref().unwrap().as_ref().left().unwrap();
                let hash = self.hash();
                cache.insert(hash, Left(lit.clone()));
            }

            IdentifierKind::VARIABLE => todo!(),
            IdentifierKind::HASH => todo!(),
            IdentifierKind::ASSIGN => {}
            IdentifierKind::PLUS => todo!(),
            IdentifierKind::MINUS => todo!(),
            IdentifierKind::BANG => todo!(),
            IdentifierKind::ASTERISK => todo!(),
            IdentifierKind::SLASH => todo!(),
            IdentifierKind::LT => todo!(),
            IdentifierKind::GT => todo!(),
            IdentifierKind::EQ => todo!(),
            IdentifierKind::NOTEQ => todo!(),
            IdentifierKind::GTOREQ => todo!(),
            IdentifierKind::LTOREQ => todo!(),
            IdentifierKind::AND => todo!(),
            IdentifierKind::OR => todo!(),
            IdentifierKind::LAND => todo!(),
            IdentifierKind::LOR => todo!(),
            IdentifierKind::MODULUS => todo!(),
            IdentifierKind::FUNCTION => todo!(),
            IdentifierKind::LET => todo!(),
            IdentifierKind::IF => todo!(),
            IdentifierKind::ELSE => todo!(),
            IdentifierKind::RETURN => todo!(),
            IdentifierKind::FORMAT => todo!(),
            IdentifierKind::PRINT => todo!(),
            IdentifierKind::MAIN => todo!(),
            IdentifierKind::CALLER => todo!(),
            IdentifierKind::BLOCK => todo!(),
            IdentifierKind::GROUPING => todo!(),
            IdentifierKind::ARRAY => todo!(),

            _ => unreachable!(),
        }
    }
}
