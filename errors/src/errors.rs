use std::fmt;
use std::io;

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum KarisErrorType {
    UnknownToken,
    MissingLetBinding,
    MissingConditionalIndentifier,
    MissingEntryPoint,
    MissingVariableName,
    MissingTypeInfo,
    InvalidSyntax,
    MalformedProgram,
    UnableToConvert,
    PreconditionFailure,
}

#[derive(Debug, Clone)]
pub struct KarisError {
    pub error_type: KarisErrorType,
    pub message: String,
}

impl fmt::Display for KarisError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let err_msg = format!(
            "An error occured while processing. Details \n \t\t\t Type : {:?} \n \t\t\t Message : {:?}",
            self.error_type, self.message
        );
        write!(fmt, "{}", err_msg)
    }
}

impl From<KarisError> for io::Error {
    fn from(err: KarisError) -> Self {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "\n [Error type] : {:?} \n [Message] {} \n",
                err.error_type, err.message
            ),
        )
    }
}
