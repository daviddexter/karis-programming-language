use crate::declarations;
use crate::identifier_declaration::IdentifierDeclaration;
use crate::literal_declaration::LiteralDeclaration;
use crate::objects::Objects;
use crate::program_declaration::ProgramDeclaration;
use crate::variable_declaration::VariableDeclaration;
use crate::variable_declarator::VariableDeclarator;
use errors::errors;
use lexer::lexer::Lexer;
use lexer::tokens::Token;

pub struct Parser {
    pub lexer: Lexer,
    bucket: Vec<Token>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        Self {
            lexer,
            bucket: Vec::new(),
        }
    }

    pub fn parse(&mut self) -> Result<Objects, errors::ParserError> {
        let mut lx = Lexer::new(String::from("let sum @int = 1;"));
        match lx.generate() {
            Ok(t) => {
                self.bucket.push(t);
            }
            Err(err) => println!("{}", err),
        }

        let lit = LiteralDeclaration::new(String::from("1"));
        let id = IdentifierDeclaration::new(String::from("sum"), declarations::TypingKind::Int);
        let vdtor = VariableDeclarator::new(Objects::LiteralDeclaration(lit), id);
        let vd = VariableDeclaration::new(
            declarations::VariableKind::Let,
            Some(vec![Objects::VariableDeclarator(vdtor)]),
        );
        let pg = ProgramDeclaration::new(Some(vec![Objects::VariableDeclaration(vd)]));
        Ok(Objects::ProgramDeclaration(pg))
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn should_parse_let_binding() {
        let lx = Lexer::new(String::from("let sum @int = 1 + 2;"));
        let mut parser = Parser::new(lx);
        let res = parser.parse();
        if let Ok(Objects::ProgramDeclaration(x)) = res {
            assert_eq!(x.count(), 1);
        }
    }
}
