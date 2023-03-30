use std::cell::RefCell;
use std::hash::Hash;
use std::rc::Rc;

use borsh::{BorshDeserialize, BorshSerialize};
use hashbrown::HashMap;

pub const DEFAULT_SCOPE_ID: usize = 1000;

#[derive(Default, Clone)]
pub enum SymbolScope {
    #[default]
    Global = 0x00000000,

    Local,
}

#[derive(Default, Clone)]
pub enum CallerParamType {
    #[default]
    Literal = 0x003,

    Object,
}

pub enum OpCode {
    OpTerminal = -0x005,
    OpConstant = 0x005,
    OpAdd,
    OpMinus,
    OpMultiply,
    OpDivide,
    OpModulus,
    OpSetVariable,
    OpGetVariable,
    OpFunctionDef,
    OpGetFunctionParameter,
    OpReturn,
    OpCallerDef,
    OpSetCallerParameter,
    OpGetCallerParameter,
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
