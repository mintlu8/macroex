//! See the documentation of [`macroex`](https://docs.rs/macroex/latest/macroex/)

use proc_macro_error::proc_macro_error;
mod macros;
mod metalist;

/// Generates a `FromMacro` implementation
/// 
/// * struct `from_one` parses a `{field: value, ..}`, 
/// * struct `from_many` parses `TypeName {field: value, ..}`
/// * tuple struct `from_one` parses a `(value, ..)`, 
/// * tuple struct `from_many` parses `TypeName (value, ..)`
/// * enum `from_one` parses a `Discriminant` on fieldless variants, 
/// * enum `from_many` parses a `Discriminant (value, ..)` on tuple variants,
/// * enum `from_many` parses a `Discriminant {field: value, ..}` on named variants,
/// 
/// Attributes:
/// * `#[macroex(rename_all="snake_case")]`: 
/// 
/// Rename all field names. 
/// 
/// Allowed on named structs, enums and named enum variants.
/// * `#[macroex(rename="something_else")]`: 
/// 
/// Rename the struct or field name.
/// * #[macroex(required)]`: 
/// 
/// We supply default values to fields by default, unlike serde.
/// This attribute opts out of that. Required on types not implementing
/// `Default`.
/// * `#[macroex(default="4")]`: 
/// 
/// Use this expression as the default value.
/// * `#[macroex(repeat)]`: 
/// 
/// Collect duplicate keys into a [`FromIterator`](std::iter::FromIterator).
#[proc_macro_error]
#[proc_macro_derive(FromMacro, attributes(macroex))]
pub fn from_macro(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    macros::from_macro(tokens.into()).into()
}

/// Generates a simple `FromMacro` implementation for macro attributes.
/// 
/// This macro is only allowed on named structs and supports 3 basic syntax:
/// 
/// * `.., name, ..` parses to `name: true`, which matches a boolean value.
/// * `.., name = expr, ..` parses to `name: expr`
/// * `.., name(..), ..` parses to `name: T{ .. }`
/// 
/// Other types like fieldless enums can potentially use [`FromMacro`] 
/// to generated compatible [`FromMacro`] implementations to use with this macro.
/// 
/// Unlike [`FromMacro`], we provide `from_many` with no delimiters since that's
/// how `syn` produces its `TokenStreams`.
/// 
#[proc_macro_error]
#[proc_macro_derive(FromAttrs, attributes(macroex))]
pub fn from_attrs(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    metalist::from_meta(tokens.into()).into()
}