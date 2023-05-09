use std::fmt;

use borsh::{BorshDeserialize, BorshSerialize};
use enum_as_inner::EnumAsInner;
use serde::{Deserialize, Serialize};

pub const INTERGER_OBJECT_TYPE: &str = "I";
pub const STRING_OBJECT_TYPE: &str = "S";
pub const BOOLEAN_OBJECT_TYPE: &str = "B";
pub const VARIABLE_OBJECT_TYPE: &str = "V";
pub const NULL_OBJECT_TYPE: &str = "N";
pub const ARRAY_OBJECT_TYPE: &str = "N";

#[derive(Debug, Clone, Serialize, Deserialize, EnumAsInner, BorshDeserialize, BorshSerialize)]
pub enum CompileObject {
    Interger(isize),
    String(String),
    Boolean(bool),
    Variable(Vec<u8>),
    Array(ArrayObject),
    Null,
}

impl CompileObject {
    pub fn object_type(&self) -> &'static str {
        match self {
            CompileObject::Interger(_) => INTERGER_OBJECT_TYPE,
            CompileObject::String(_) => STRING_OBJECT_TYPE,
            CompileObject::Boolean(_) => BOOLEAN_OBJECT_TYPE,
            CompileObject::Variable(_) => VARIABLE_OBJECT_TYPE,
            CompileObject::Null => NULL_OBJECT_TYPE,
            CompileObject::Array(_) => ARRAY_OBJECT_TYPE,
        }
    }
}

#[derive(Clone, Serialize, Deserialize, EnumAsInner, BorshDeserialize, BorshSerialize)]
pub enum ArrayObject {
    Interger(Vec<isize>),
    String(Vec<String>),
    Boolean(Vec<bool>),
}

impl fmt::Debug for ArrayObject {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ArrayObject::Interger(integer) => write!(f, "{:?}", integer),
            ArrayObject::String(string) => write!(f, "{:?}", string),
            ArrayObject::Boolean(bools) => write!(f, "{:?}", bools),
        }
    }
}
