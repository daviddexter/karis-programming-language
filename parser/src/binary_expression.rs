use crate::declarations::{Declaration, DeclarationType};
use crate::objects::Objects;

#[derive(Debug, Clone)]
pub struct BinaryExpression {
    pub operator: BinaryOperator,
    pub rhs: Objects,
    pub lhs: Objects,
}

impl Declaration for BinaryExpression {
    fn which(&self) -> DeclarationType {
        DeclarationType::BinaryExpression
    }
}

impl BinaryExpression {
    pub fn new(operator: BinaryOperator, rhs: Objects, lhs: Objects) -> Self {
        Self { operator, rhs, lhs }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Equals,
}
