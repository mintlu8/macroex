use proc_macro2::{TokenTree, TokenStream};

use crate::{FromMacro, abort, Error};


/// Matches one of two [`FromMacro`] implementors sequentially.
/// 
/// Note the input will be cloned for each branch, which might not be optimal.
pub enum Either<A: FromMacro, B: FromMacro>{
    A(A),
    B(B),
}

/// Matches one of two [`FromMacro`] implementors by count.
///
/// * Uses [`One`](OneOrMany::One) if input is a [`TokenTree`]
/// * Uses [`Many`](OneOrMany::Many) if input is a [`TokenStream`]
#[derive(Debug)]
pub enum OneOrMany<TOne, TMany>{
    One(TOne),
    Many(TMany),
}

impl<TOne, TMany> FromMacro for OneOrMany<TOne, TMany> where TOne: FromMacro, TMany: FromMacro{
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        Ok(Self::One(TOne::from_one(tt)?))
    }

    fn from_many(tokens: TokenStream) -> Result<Self, Error> {
        Ok(Self::Many(TMany::from_many(tokens)?))
    }
}

/// Either a [`TokenTree`] or a [`TokenStream`]
pub type EitherStream = OneOrMany<TokenTree, TokenStream>;


/// Matches one of three [`FromMacro`] implementors sequentially.
/// 
/// Note the input will be cloned for each branch, which might not be optimal.
pub enum Either3<A: FromMacro, B: FromMacro, C: FromMacro>{
    A(A),
    B(B),
    C(C),
}

/// Matches one of four [`FromMacro`] implementors sequentially.
/// 
/// Note the input will be cloned for each branch, which might not be optimal.
pub enum Either4<A: FromMacro, B: FromMacro, C: FromMacro, D: FromMacro>{
    A(A),
    B(B),
    C(C),
    D(D),
}


macro_rules! impl_choice {
    ($name: ident {$($fields: ident),*}) => {
        
impl<$($fields),*> FromMacro for $name<$($fields),*> where $($fields: FromMacro),* {
    fn from_one(tt: proc_macro2::TokenTree) -> Result<Self, crate::Error> {
        let span = tt.span();
        $(if let Ok(x) = $fields::from_one(tt.clone()) {
            return Ok(Self::$fields(x));
        })*
        abort!(span, FailedToMatch(tt.to_string()))
    }

    fn from_many(tokens: proc_macro2::TokenStream) -> Result<Self, crate::Error> {
        $(if let Ok(x) = $fields::from_many(tokens.clone()) {
            return Ok(Self::$fields(x));
        })*
        abort!(call_site, FailedToMatch(tokens.to_string()))
    }
}
    };
}

impl_choice!(Either {A, B});
impl_choice!(Either3 {A, B, C});
impl_choice!(Either4 {A, B, C, D});