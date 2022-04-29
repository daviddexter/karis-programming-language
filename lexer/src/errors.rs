use std::fmt;
use std::io;

#[derive(Debug, Copy, Clone)]
pub enum LexerErrorType {
    UnknownToken,
    Internal,
}

#[derive(Debug, Clone)]
pub struct LexerError {
    pub error_type: LexerErrorType,
    pub message: String,
}

impl fmt::Display for LexerError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let err_msg = format!(
            "An error occured while processing. Details \n \t\t\t Type : {:?} \n \t\t\t Message : {:?}",
            self.error_type, self.message
        );
        write!(fmt, "{}", err_msg)
    }
}

impl From<LexerError> for io::Error {
    fn from(err: LexerError) -> Self {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "\n [Error type] : {:?} \n [Message] {} \n",
                err.error_type, err.message
            ),
        )
    }
}
