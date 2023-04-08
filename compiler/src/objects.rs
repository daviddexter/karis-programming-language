use borsh::{BorshDeserialize, BorshSerialize};
use enum_as_inner::EnumAsInner;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize, BorshDeserialize, BorshSerialize, EnumAsInner)]
pub enum CompileObject {
    Interger(isize),
    String(String),
    Boolean(bool),
    Variable(Vec<u8>),
    Null,
}
