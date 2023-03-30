use std::{fs::File, io::Read};

use borsh::BorshDeserialize;
use errors::errors::KarisError;

use crate::compile::ByteCode;

pub struct VM {
    byte_code: ByteCode,
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

    pub fn execute(&self) {
        println!("{:?}", self.byte_code)
    }
}
