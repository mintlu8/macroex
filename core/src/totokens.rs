//! This adapts our extractors with the quote crate

use std::str::FromStr;

use proc_macro2::{Literal, TokenStream};
use quote::{ToTokens, quote};

use crate::*;

impl quote::ToTokens for Byte {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        Literal::u8_unsuffixed(self.0).to_tokens(tokens)
    }
}
impl quote::ToTokens for Bytes {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        Literal::byte_string(self.0.as_slice()).to_tokens(tokens)
    }
}

impl<A, B> quote::ToTokens for Either<A, B> where A: FromMacro + ToTokens, B: FromMacro + ToTokens{
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Either::A(a) => a.to_tokens(tokens),
            Either::B(b) => b.to_tokens(tokens),
        }
    }
}

impl<A, B, C> quote::ToTokens for Either3<A, B, C> where 
            A: FromMacro + ToTokens, 
            B: FromMacro + ToTokens, 
            C: FromMacro + ToTokens,{
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Either3::A(a) => a.to_tokens(tokens),
            Either3::B(b) => b.to_tokens(tokens),
            Either3::C(c) => c.to_tokens(tokens),
        }
    }
}

impl<A, B, C, D> quote::ToTokens for Either4<A, B, C, D> where 
            A: FromMacro + ToTokens, 
            B: FromMacro + ToTokens, 
            C: FromMacro + ToTokens, 
            D: FromMacro + ToTokens,{
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Either4::A(a) => a.to_tokens(tokens),
            Either4::B(b) => b.to_tokens(tokens),
            Either4::C(c) => c.to_tokens(tokens),
            Either4::D(d) => d.to_tokens(tokens),
        }
    }
}

/// Format a fixed sized array into a function call.
/// 
/// `format_str` syntax: `path::to::fn($)` or `fn(1,2,$,3)`
pub fn format_call<T: ToTokens>(format_str: &str, array: &[T]) -> Result<TokenStream, Error>{
    let inner = quote::quote!(#(#array),*).to_string();
    match TokenStream::from_str(&format_str.replace("$", &inner)) {
        Ok(stream) => Ok(stream),
        Err(e) => abort_into!(call_site, e),
    }
}

/// Format a fixed sized array into a function call.
/// 
/// `format_str` syntax: `path::to::Struct{$}` or `Struct{name: "ferris", $}`
pub fn format_comstructor<T: ToTokens>(format_str: &str, fields: &[&str], array: &[T]) -> Result<TokenStream, Error>{
    let inner = quote::quote!(#(#fields: #array),*).to_string();
    match TokenStream::from_str(&format_str.replace("$", &inner)) {
        Ok(stream) => Ok(stream),
        Err(e) => abort_into!(call_site, e),
    }
}

/// Converts an array into an [array, ..] in `quote!`.
#[derive(Debug, Default, PartialEq, Eq, Clone, Copy)]
struct ArraySyntax<T>(pub T);

impl<T: IntoIterator + Clone> quote::ToTokens for ArraySyntax<T> where T::Item: ToTokens{
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let items = self.0.clone().into_iter();
        tokens.extend(quote!([#(#items),*]))
    }
}

/// Converts an array into a function call in `quote!`.
/// ```
/// # use macroex::*;
/// call_syntax!("::glam::Vec2($)", pub Vec2(pub [f32; 2]));
/// ```
/// This is a dummy extractor that formats an array extractor
/// into a function call.
#[macro_export]
macro_rules! call_syntax {
    ($fmt: literal, $(#[$($attr: tt)*])* $vis: vis $name: ident ($vis2: vis $ty: ty) ) => {
        $(#[$($attr)*])* 
        $vis struct $name($vis2 $ty);

        const _: () = {
            use ::macroex::FromMacro;
            impl ::macroex::FromMacro for $name {
                fn from_one(tt: ::macroex::proc_macro2::TokenTree) -> Result<Self, ::macroex::Error> {
                    Ok(Self(<$ty>::from_one(tt)?))
                }
                fn from_many(tokens: ::macroex::proc_macro2::TokenStream) -> Result<Self, ::macroex::Error> {
                    Ok(Self(<$ty>::from_many(tokens)?))
                }
            }
            impl ::macroex::quote::ToTokens for $name {
                fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
                    tokens.extend(::macroex::format_call($fmt, &self.0).unwrap());
                }
            }
        };
    };
}


/// Converts array into a struct constructor in `quote!`.
/// ```
/// # use macroex::*;
/// construct_syntax!("::glam::Vec2{$}", pub Vec2(pub [f32; 2]) => {x, y});
/// ```
/// This is a dummy extractor that formats an array extractor
/// into a constructor.
#[macro_export]
macro_rules! construct_syntax {
    ($fmt: literal, $(#[$($attr: tt)*])* $vis: vis $name: ident ($vis2: vis $ty: ty) => {$($field: ident),* $(,)?}) => {
        $(#[$($attr)*])* 
        $vis struct $name($vis2 $ty);

        const _: () = {
            use ::macroex::FromMacro;
            impl ::macroex::FromMacro for $name {
                fn from_one(tt: ::macroex::proc_macro2::TokenTree) -> Result<Self, ::macroex::Error> {
                    Ok(Self(<$ty>::from_one(tt)?))
                }
                fn from_many(tokens: ::macroex::proc_macro2::TokenStream) -> Result<Self, ::macroex::Error> {
                    Ok(Self(<$ty>::from_many(tokens)?))
                }
            }
            impl ::macroex::quote::ToTokens for $name {
                fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
                    tokens.extend(::macroex::format_comstructor($fmt, &[$(stringify!($field),)*], &self.0).unwrap());
                }
            }
        };
    };
}