use super::Builtin;
use crate::expr::SmolStr;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, thiserror::Error, Clone)]
pub enum Error {
    #[error("Aborted: {}", .reason)]
    Abort { reason: SmolStr },
    #[error("Assertion failed")]
    AssertionFailed,
    #[error("Type error: cannot {} {} and {}", .operation, .lhs, .rhs)]
    BinOpTypeError {
        operation: &'static str,
        lhs: &'static str,
        rhs: &'static str,
    },
    #[error("Builtin not implemented: {:?}", .builtin.name())]
    BuiltinNotImplemented { builtin: Builtin },
    #[error("Builtin call error: {:?}", .reason)]
    BuiltinError { reason: SmolStr },
    #[error("Type error: cannot coerce {} to string", .actual)]
    CannotCoerceToString { actual: &'static str },
    #[error("Division by zero")]
    DivisionByZero,
    #[error("Division overflow")]
    DivisionOverflow,
    #[error("Infinite recursion")]
    InfiniteRecursion,
    #[error("Missing required argument {:?}", .name)]
    MissingArgument { name: SmolStr },
    #[error("Missing attribute {:?}", .name)]
    MissingAttribute { name: SmolStr },
    #[error("Thrown error: {}", .reason)]
    Throw { reason: SmolStr },
    #[error("Type error: expecting {}, found {}", .expect, .actual)]
    TypeError {
        expect: &'static str,
        actual: &'static str,
    },
    #[error("Unexpected argument {:?}", .name)]
    UnexpectedArgument { name: SmolStr },
}

impl Error {
    /// Check if it is a soft error, which can be caught by `tryEval`.
    pub fn is_soft_error(&self) -> bool {
        match self {
            Self::AssertionFailed | Self::Throw { .. } => true,
            _ => false,
        }
    }
}
