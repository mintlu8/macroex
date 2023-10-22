use std::fmt::{Display, Debug};

use proc_macro2::{TokenTree, Span};

#[derive(Debug, thiserror::Error)]
/// Types of errors in [`macroex`].
pub(crate) enum ErrorType {
    #[error("Too many items in TokenStream, expected one.")]
    ExpectOne,
    #[error("Expected more items, found one.")]
    ExpectMany,
    #[error("Expected end of stream, found items.")]
    ExpectEnd,
    #[error("Failed to match `{0}`.")]
    FailedToMatch(String),
    #[error("Expected integer {0}, found {1}.")]
    ExpectInt(usize, TokenTree),
    #[error("Expected ident {0}, found {1}.")]
    ExpectIdent(&'static str, TokenTree),
    #[error("Expected {0} items, found {1}.")]
    ExpectCount(usize, usize),
    #[error("Expected {0}, {1} found.")]
    ExpectTokenTree(&'static str, TokenTree),
    #[error("Expected punct {0}, {1} found.")]
    ExpectPunct(char, TokenTree),
    #[error("None delimiter encountered in from_one.")]
    EncounteredNoneDelim,
    #[error("Expected '=', '(' or end of stream.")]
    ExpectValidMeta,
    #[error("{0}")]
    LitrsInvalidToken(#[from] litrs::InvalidToken),
    #[error("Integer Overflow: {} is not a valid {}", 1, 0)]
    IntegerOverflow(&'static str, TokenTree),
    #[error("{0}")]
    ParseFloatError(#[from] std::num::ParseFloatError),
    #[error("{0}")]
    LexError(#[from] proc_macro2::LexError),
    #[error("Matching {} failed.", 0)]
    ExpectedOneMatch(TokenTree),
    #[error("Number of items is different in the input vector.")]
    LengthMismatch,
    #[error("Expected one of {}, found {}.", 0, 1)]
    ExpectedOneOf(&'static [&'static str], String),
    #[error("Expected tuple of length {}, found {}.", 0, 1)]
    ExpectedTupleOfLength(usize, usize),
    #[error("Unreachable, this is probably a bug.")]
    Unreachable,
    #[error("Is a specific punct, used for control flow.")]
    IsPunct,
    #[error("{0}")]
    CustomError(String)
}

/// A spanned error. Useful in proc macro error handling.
pub struct Error {
    /// The none case represents end of stream, 
    /// which is optimized, and used in control flow.
    pub(crate) error: Option<Box<ErrorType>>,
    pub span: Span,
}

impl Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.error {
            Some(e) => Display::fmt(&e, f),
            None => f.write_str("Reached end of TokenStream, expected more items.")
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.error {
            Some(e) => Display::fmt(&e, f),
            None => f.write_str("Reached end of TokenStream, expected more items.")
        }
    }
}

impl Error {

    /// Create a `EndOfStream` error
    pub fn end_of_stream(span: Span) -> Self{
        Self {
            error: None,
            span
        }
    }

    /// Check if error is `EndOfStream`.
    pub fn is_end_of_stream(&self) -> bool{
        self.error.is_none()
    }

    /// Create a custom error.
    pub fn custom(span: Span, val: impl Display, ) -> Self {
        Self { 
            error: Some(Box::new(ErrorType::CustomError(val.to_string()))), 
            span 
        }
    }

    /// A non-panicking version of `unreachable!`.
    pub fn unreachable() -> Self {
        Self { 
            error: Some(Box::new(ErrorType::Unreachable)), 
            span: Span::call_site() 
        }
    }
}

impl std::error::Error for Error {}


pub(crate) mod error_handling {
    #[doc(hidden)]
    #[macro_export]
    /// Parse a tt into a litrs Literal.
    macro_rules! litrs_get {
        ($name: ident, $value: expr) => {
            match ::litrs::$name::try_from(&$value) {
                Ok(ok) => ok,
                Err(e) => return Err(crate::Error {
                    error: Some(Box::new(e.into())),
                    span: $value.span(),
                })
            }
        };
    }

    #[doc(hidden)]
    #[macro_export]
    // Abort by calling into.
    macro_rules! abort_into {
        (call_site, $err: expr) => {
            return Err(
                crate::Error {
                    error: Some(Box::new($err.into())),
                    span: ::proc_macro2::Span::call_site(), 
                }
            )
        };
        ($span: expr, $err: ident) => {
            return Err(
                crate::Error {
                    error: Some(Box::new($err.into())),
                    span: $span, 
                }
            )
        };
    }

    #[doc(hidden)]
    #[macro_export]
    // Internal error throwing
    macro_rules! abort {
        (call_site, $err: ident) => {
            return Err(
                crate::Error {
                    span: ::proc_macro2::Span::call_site(), 
                    error: Some(Box::new(crate::ErrorType::$err)),
                }
            )
        };
        (call_site, $err: ident ($($tt: expr),* $(,)?)) => {
            return Err(
                crate::Error {
                    span: ::proc_macro2::Span::call_site(), 
                    error: Some(Box::new(crate::ErrorType::$err($($tt),*))),
                }
            )
        };
        ($span: expr, $err: ident) => {
            return Err(
                crate::Error {
                    span: $span, 
                    error: Some(Box::new(crate::ErrorType::$err)),
                }
            )
        };
        ($span: expr, $err: ident ($($tt: expr),* $(,)?)) => {
            return Err(
                crate::Error {
                    span: $span, 
                    error: Some(Box::new(crate::ErrorType::$err($($tt),*))),
                }
            )
        };
        ($span: expr => $err: ident) => {
            return Err(
                crate::Error {
                    span: $span.into_iter().next().map(|x|x.span()).unwrap_or(
                        ::proc_macro2::Span::call_site()
                    ), 
                    error: Some(Box::new(crate::ErrorType::$err)),
                }
            )
        };
        ($span: expr => $err: ident ($($tt: expr),* $(,)?)) => {
            return Err(
                crate::Error {
                    span: $span.into_iter().next().map(|x|x.span()).unwrap_or(
                        ::proc_macro2::Span::call_site()
                    ), 
                    error: Some(Box::new(crate::ErrorType::$err($($tt),*))),
                }
            )
        };
    }
}

/// Creates and returns a formatted custom [`macroex::Error`](Error), similar to `anyhow::bail!`.
#[macro_export]
macro_rules! bail {
    (call_site, $expr: expr) => {
        return Err(::macroex::Error::custom(
            ::macroex::proc_macro2::Span::call_site(),
            $expr,
        ));
    };
    (call_site, $string: literal $(,$expr: expr)* $(,)?) => {
        return Err(::macroex::Error::custom(
            ::macroex::proc_macro2::Span::call_site(),
            format!($string, $($expr),*),
        ));
    };
    ($span: expr, $expr: expr) => {
        return Err(::macroex::Error::custom(
            $span,
            $expr,
        ));
    };
    ($span: expr, $string: literal $(,$expr: expr)* $(,)?) => {
        return Err(::macroex::Error::custom(
            $span,
            format!($string, $($expr),*),
        ));
    };
}

/// Use [`proc_macro_error::abort!`](https://docs.rs/proc-macro-error/latest/proc_macro_error/macro.abort.html)
/// with our [`Error`]. 
/// 
/// Requires `proc_macro_error` being properly 
/// [set up](https://docs.rs/proc-macro-error/latest/proc_macro_error/#proc_macro_error-attribute).
#[macro_export]
macro_rules! abort_this {
    ($expr: expr) => {
        ::proc_macro_error::abort!($expr.span.clone(), "{}", $expr);
    };
}
