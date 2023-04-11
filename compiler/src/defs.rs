use std::cell::RefCell;
use std::hash::Hash;
use std::rc::Rc;

use borsh::{BorshDeserialize, BorshSerialize};
use enum_as_inner::EnumAsInner;
use hashbrown::HashMap;

pub const DEFAULT_SCOPE_ID: usize = 1000;

#[derive(Default, Clone, EnumAsInner)]
pub enum SymbolScope {
    #[default]
    Global = 0x00000000,

    Local,
}

#[derive(Default, Clone)]
pub enum CallerParamType {
    #[default]
    Literal = 0x003,

    Variable,
}

impl From<u8> for CallerParamType {
    fn from(value: u8) -> Self {
        if value == CallerParamType::Variable as u8 {
            CallerParamType::Variable
        } else {
            CallerParamType::Literal
        }
    }
}

#[derive(Clone)]
pub enum BindingType {
    Literal = 0x004,

    Caller,

    Expression,
}

impl From<u8> for BindingType {
    fn from(value: u8) -> Self {
        if value == BindingType::Caller as u8 {
            BindingType::Caller
        } else if value == BindingType::Expression as u8 {
            BindingType::Expression
        } else {
            BindingType::Literal
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum OpCode {
    OpTerminal = -0x08,
    // acts as a seperator in some cases
    OpNull = -0x09,

    OpConstant = 0x09,
    OpAdd,
    OpMinus,
    OpMultiply,
    OpDivide,
    OpModulus,

    OpSetBinding,
    OpGetBinding,
    OpFunctionDef,
    OpGetFunctionParameter,
    OpReturn,
    OpCallerDef,
    OpGetCallerParameter,
    OpAddBuiltin,
    OpPrint,
    OpAddIfCondition,
    OpGreaterThan,
    OpGreaterThanOrEqual,
    OpLessThan,
    OpLessThanOrEqual,
    OpEqualTo,
    OpNotEqualTo,
    OpAND,
    OpOR,
    OpLAND,
    OpLOR,
    OpBang,
    OpJumpTo,
    OpJumpToAlternate,
}

impl From<u8> for OpCode {
    fn from(value: u8) -> Self {
        if value == OpCode::OpTerminal as u8 {
            OpCode::OpTerminal
        } else if value == OpCode::OpConstant as u8 {
            OpCode::OpConstant
        } else if value == OpCode::OpAdd as u8 {
            OpCode::OpAdd
        } else if value == OpCode::OpMinus as u8 {
            OpCode::OpMinus
        } else if value == OpCode::OpMultiply as u8 {
            OpCode::OpMultiply
        } else if value == OpCode::OpDivide as u8 {
            OpCode::OpDivide
        } else if value == OpCode::OpModulus as u8 {
            OpCode::OpModulus
        } else if value == OpCode::OpGetBinding as u8 {
            OpCode::OpGetBinding
        } else if value == OpCode::OpSetBinding as u8 {
            OpCode::OpSetBinding
        } else if value == OpCode::OpFunctionDef as u8 {
            OpCode::OpFunctionDef
        } else if value == OpCode::OpGetFunctionParameter as u8 {
            OpCode::OpGetFunctionParameter
        } else if value == OpCode::OpReturn as u8 {
            OpCode::OpReturn
        } else if value == OpCode::OpCallerDef as u8 {
            OpCode::OpCallerDef
        } else if value == OpCode::OpGetCallerParameter as u8 {
            OpCode::OpGetCallerParameter
        } else if value == OpCode::OpAddBuiltin as u8 {
            OpCode::OpAddBuiltin
        } else if value == OpCode::OpPrint as u8 {
            OpCode::OpPrint
        } else if value == OpCode::OpAddIfCondition as u8 {
            OpCode::OpAddIfCondition
        } else if value == OpCode::OpGreaterThan as u8 {
            OpCode::OpGreaterThan
        } else if value == OpCode::OpGreaterThanOrEqual as u8 {
            OpCode::OpGreaterThanOrEqual
        } else if value == OpCode::OpLessThan as u8 {
            OpCode::OpLessThan
        } else if value == OpCode::OpLessThanOrEqual as u8 {
            OpCode::OpLessThanOrEqual
        } else if value == OpCode::OpEqualTo as u8 {
            OpCode::OpEqualTo
        } else if value == OpCode::OpNotEqualTo as u8 {
            OpCode::OpNotEqualTo
        } else if value == OpCode::OpAND as u8 {
            OpCode::OpAND
        } else if value == OpCode::OpOR as u8 {
            OpCode::OpOR
        } else if value == OpCode::OpLAND as u8 {
            OpCode::OpLAND
        } else if value == OpCode::OpLOR as u8 {
            OpCode::OpLOR
        } else if value == OpCode::OpBang as u8 {
            OpCode::OpBang
        } else if value == OpCode::OpJumpTo as u8 {
            OpCode::OpJumpTo
        } else if value == OpCode::OpJumpToAlternate as u8 {
            OpCode::OpJumpToAlternate
        } else {
            OpCode::OpNull
        }
    }
}

#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct SymbolStoreValue<T: borsh::BorshSerialize + borsh::BorshDeserialize>(pub Vec<Vec<T>>);

pub type SymbolsTableTyping = Rc<RefCell<SymbolStore<Vec<u8>, SymbolStoreValue<u8>>>>;

// we use new pattern to implement foreign traits -> BorshSerialize and BorshDeserialize
#[derive(Debug, Clone)]
pub struct SymbolStore<K, V>(pub HashMap<K, V>);

impl<K, V> BorshSerialize for SymbolStore<K, V>
where
    K: BorshSerialize + PartialOrd,
    V: BorshSerialize,
{
    fn serialize<W: std::io::Write>(&self, writer: &mut W) -> std::io::Result<()> {
        let mut vec = self.0.iter().collect::<Vec<_>>();

        vec.sort_by(|(a, _), (b, _)| a.partial_cmp(b).unwrap());
        u32::try_from(vec.len())
            .map_err(|_| std::io::ErrorKind::InvalidInput)?
            .serialize(writer)?;
        for (key, value) in vec {
            key.serialize(writer)?;
            value.serialize(writer)?;
        }
        Ok(())
    }
}

impl<K, V> BorshDeserialize for SymbolStore<K, V>
where
    K: BorshDeserialize + Eq + Hash,
    V: BorshDeserialize,
{
    fn deserialize_reader<R: std::io::Read>(reader: &mut R) -> std::io::Result<Self> {
        let len = u32::deserialize_reader(reader)?;
        let mut result = HashMap::new();
        for _ in 0..len {
            let key = K::deserialize_reader(reader)?;
            let value = V::deserialize_reader(reader)?;
            result.insert(key, value);
        }

        Ok(SymbolStore(result))
    }

    fn deserialize(buf: &mut &[u8]) -> std::io::Result<Self> {
        Self::deserialize_reader(&mut *buf)
    }

    fn try_from_slice(v: &[u8]) -> std::io::Result<Self> {
        let mut v_mut = v;
        let result = Self::deserialize(&mut v_mut)?;
        if !v_mut.is_empty() {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "Not all bytes were read",
            ));
        }
        Ok(result)
    }

    fn try_from_reader<R: std::io::Read>(reader: &mut R) -> std::io::Result<Self> {
        let result = Self::deserialize_reader(reader)?;
        let mut buf = [0u8; 1];
        match reader.read_exact(&mut buf) {
            Err(f) if f.kind() == std::io::ErrorKind::UnexpectedEof => Ok(result),
            _ => Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "Not all bytes were read",
            )),
        }
    }

    fn vec_from_reader<R: std::io::Read>(
        len: u32,
        reader: &mut R,
    ) -> std::io::Result<Option<Vec<Self>>> {
        let _ = len;
        let _ = reader;
        Ok(None)
    }

    fn array_from_reader<R: std::io::Read, const N: usize>(
        reader: &mut R,
    ) -> std::io::Result<Option<[Self; N]>> {
        let _ = reader;
        Ok(None)
    }
}
