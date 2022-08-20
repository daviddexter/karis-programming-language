use std::fmt;
use std::io;

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum KarisErrorType {
    Escape,
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
    IO,
    ReadlineError,
    PyO3,
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
        write!(fmt, "{err_msg}")
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

impl From<io::Error> for KarisError {
    fn from(err: io::Error) -> Self {
        KarisError {
            error_type: KarisErrorType::IO,
            message: err.to_string(),
        }
    }
}

impl From<rustyline::error::ReadlineError> for KarisError {
    fn from(err: rustyline::error::ReadlineError) -> Self {
        KarisError {
            error_type: KarisErrorType::ReadlineError,
            message: err.to_string(),
        }
    }
}

impl From<pyo3::PyErr> for KarisError {
    fn from(err: pyo3::PyErr) -> Self {
        KarisError {
            error_type: KarisErrorType::PyO3,
            message: err.to_string(),
        }
    }
}
