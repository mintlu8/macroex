use proc_macro2::{TokenTree, TokenStream};

use crate::{FromMacro, abort, Error};

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



macro_rules! impl_choice {
    ($name: ident {$($fields: ident),*}) => {

/// Matches one of many [`FromMacro`] implementors sequentially.
/// 
/// Note the input will be cloned for each branch, which might not be optimal.
#[derive(Debug)]
pub enum $name<$($fields: FromMacro),*>{
    $($fields($fields)),*
}
        
impl<$($fields),*> FromMacro for $name<$($fields),*> where $($fields: FromMacro),* {
    fn from_one(tt: proc_macro2::TokenTree) -> Result<Self, crate::Error> {
        let span = tt.span();
        $(
            if $fields::peek(&tt) {
                if let Ok(x) = $fields::from_one(tt.clone()) {
                    return Ok(Self::$fields(x));
                }
            }
        )*
        abort!(span, FailedToMatch(tt.to_string()))
    }

    fn from_many(tokens: proc_macro2::TokenStream) -> Result<Self, crate::Error> {
        $(if let Ok(x) = $fields::from_many(tokens.clone()) {
            return Ok(Self::$fields(x));
        })*
        abort!(call_site, FailedToMatch(tokens.to_string()))
    }
}

impl<$($fields),*> Default for $name<$($fields),*> where A: Default, $($fields: FromMacro),*{
    fn default() -> Self {
        Self::A(A::default())
    }
}


#[cfg(feature="quote")]
impl<$($fields),*> quote::ToTokens for $name<$($fields),*>where $($fields: FromMacro + quote::ToTokens),*{
    #[allow(nonstandard_style)]
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            $($name::$fields($fields) => $fields.to_tokens(tokens)),*
        }
    }
}
    };
}

impl_choice!(Either {A, B});
impl_choice!(Either3 {A, B, C});
impl_choice!(Either4 {A, B, C, D});
impl_choice!(Either5 {A, B, C, D, E});
impl_choice!(Either6 {A, B, C, D, E, F});
impl_choice!(Either7 {A, B, C, D, E, F, G});
impl_choice!(Either8 {A, B, C, D, E, F, G, H});
impl_choice!(Either9 {A, B, C, D, E, F, G, H, I});

