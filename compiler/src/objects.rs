use borsh::{BorshDeserialize, BorshSerialize};
use enum_as_inner::EnumAsInner;
use serde::{Deserialize, Serialize};

pub const INTERGER_OBJECT_TYPE: &str = "I";
pub const STRING_OBJECT_TYPE: &str = "S";
pub const BOOLEAN_OBJECT_TYPE: &str = "B";
pub const VARIABLE_OBJECT_TYPE: &str = "V";
pub const NULL_OBJECT_TYPE: &str = "N";

// BorshDeserialize, BorshSerialize,
#[derive(Debug, Clone, Serialize, Deserialize, EnumAsInner, BorshDeserialize, BorshSerialize)]
pub enum CompileObject {
    Interger(isize),
    String(String),
    Boolean(bool),
    Variable(Vec<u8>),
    Array(Vec<Vec<u8>>),
    Null,
}

impl CompileObject {
    pub fn object_type(&self) -> &'static str {
        match self {
            CompileObject::Interger(_) => INTERGER_OBJECT_TYPE,
            CompileObject::String(_) => STRING_OBJECT_TYPE,
            CompileObject::Boolean(_) => BOOLEAN_OBJECT_TYPE,
            CompileObject::Variable(_) => VARIABLE_OBJECT_TYPE,
            CompileObject::Null => "N",
            CompileObject::Array(_) => todo!(),
        }
    }
}
