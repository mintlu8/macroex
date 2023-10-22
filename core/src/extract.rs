
use std::mem;

use proc_macro2::{TokenTree, Spacing, Span};
use crate::{Error, FromMacro, NotPunct, abort, StreamExtract};

/// Allow extracting from a TokenStream Iterator.
/// 
/// All `FromMacro` implementors use `from_one` as an extractor.
/// 
/// Combine with an extractor like `Deliminated` to use `from_multi`.
pub trait Extractor: Sized {
    fn extract(iter: &mut impl Iterator<Item=TokenTree>) -> Result<Self, Error>;
}

impl<T> Extractor for T where T: FromMacro {
    fn extract(iter: &mut impl Iterator<Item=TokenTree>) -> Result<Self, Error> {
        match iter.next(){
            Some(tt) => T::from_one(tt),
            None => return Err(Error::end_of_stream(Span::call_site())),
        }
    }
}

/// Special extractor that validates the end of TokenStream.
/// # Example:
/// ```
/// # use macroex::*;
/// # use quote::quote;
/// # fn main() -> Result<(), Error> {
/// // Extractor syntax
/// let EndOfStream = quote!().into_iter().extract()?;
/// 
/// // Tests
/// assert!(quote!().into_iter().extract::<EndOfStream>().is_ok());
/// assert!(quote!(,).into_iter().extract::<EndOfStream>().is_err());
/// # Ok(())}
/// ```
#[derive(Debug, Default)]
pub struct EndOfStream;

/// Extracts an [`Option<T>`] that may be [`None`] if at end of stream.
/// 
/// # Example:
/// ```
/// # use macroex::*;
/// # use quote::quote;
/// # fn main() -> Result<(), Error> {
/// let OrEndOfStream(comma) = quote!(,).into_iter().extract()?;
/// # let x: &Option<proc_macro2::Punct> = &comma;
/// assert!(comma.unwrap().as_char() == ',');
/// 
/// let OrEndOfStream(comma) = quote!().into_iter().extract()?;
/// # let x: &Option<proc_macro2::Punct> = &comma;
/// assert!(comma.is_none());
/// # Ok(())}
/// ```
#[derive(Debug, Default)]
pub struct OrEndOfStream<T: FromMacro>(pub Option<T>);


impl Extractor for EndOfStream {
    fn extract(iter: &mut impl Iterator<Item=TokenTree>) -> Result<Self, Error> {
        match iter.next(){
            Some(tt) => abort!(tt.span(), ExpectEnd),
            None => Ok(Self)
        }
    }
}

impl<T> Extractor for OrEndOfStream<T> where T: FromMacro {
    fn extract(iter: &mut impl Iterator<Item=TokenTree>) -> Result<Self, Error> {
        match iter.next(){
            Some(tt) => Ok(Self(Some(T::from_one(tt)?))),
            None => Ok(Self(None))
        }
    }
}


/// Extractor that matches everything in the stream and dipatches `from_one` and `from_many` accordingly
/// 
/// # Example:
/// ```
/// # use macroex::*;
/// # use quote::quote;
/// # fn main() -> Result<(), Error> {
/// let Stringify(string) = quote!(Ferris the Rustacean.).into_iter().extract()?;
/// assert_eq!(string, "Ferris");
/// 
/// let All(Stringify(string)) = quote!(Ferris the Rustacean.).into_iter().extract()?;
/// assert_eq!(string, "Ferris the Rustacean .");
/// # Ok(())}
/// ```
#[derive(Debug, Default)]
pub struct All<T: FromMacro>(pub T);

/// Extractor that matches until a punctuation mark is found.
/// 
/// This is useful for potentially calling the `from_many` method 
/// if more than one TokenTree is extracted. 
/// # Example:
/// ```
/// # use macroex::*;
/// # use quote::quote;
/// # fn main() -> Result<(), Error> {
/// let mut iter = quote!(ferris, crates.io, cargo.toml).into_iter();
/// let Punctuated::<_, ','>(Stringify(next)) = iter.extract()?;
/// assert_eq!(next, "ferris");
/// let Punctuated::<_, ','>(Stringify(next)) = iter.extract()?;
/// assert_eq!(next, "crates . io");
/// let Punctuated::<_, '.'>(Stringify(next)) = iter.extract()?;
/// assert_eq!(next, "cargo");
/// let Punctuated::<_, '@'>(Stringify(next)) = iter.extract()?;
/// assert_eq!(next, "toml");
/// let EndOfStream = iter.extract()?;
/// # Ok(())}
/// ```
#[derive(Debug, Default)]
pub struct Punctuated<T: FromMacro, const P: char>(pub T);


impl<T> Extractor for All<T> where T: FromMacro{
    fn extract(iter: &mut impl Iterator<Item=proc_macro2::TokenTree>) -> Result<Self, crate::Error> {
        let (tokens, len) = crate::sanitize(iter);
        if T::PREFER_MANY {
            return Ok(Self(T::from_many(tokens)?));
        }
        match len {
            0 => Err(Error::end_of_stream(Span::call_site())),
            1 => Ok(Self(T::from_one(tokens.into_iter().next().unwrap())?)),
            _ => Ok(Self(T::from_many(tokens)?)),
        }
    }
}


impl<T, const P: char> Extractor for Punctuated<T, P> where T: FromMacro{
    fn extract(iter: &mut impl Iterator<Item=proc_macro2::TokenTree>) -> Result<Self, crate::Error> {
        let mut result = Vec::new();
        loop {
            match NotPunct::<P>::extract(iter) {
                Ok(NotPunct(Some(tt))) => result.push(tt),
                Ok(NotPunct(None)) => {
                    let (tokens, len) = crate::sanitize(result);
                    if T::PREFER_MANY {
                        return Ok(Self(T::from_many(tokens)?));
                    }
                    return match len {
                        0 => Err(Error::end_of_stream(Span::call_site())),
                        1 => Ok(Self(T::from_one(tokens.into_iter().next().unwrap())?)),
                        _ => Ok(Self(T::from_many(tokens)?)),
                    }
                },
                Err(e) if e.is_end_of_stream() => {
                    let (tokens, len) = crate::sanitize(result);
                    if T::PREFER_MANY {
                        return Ok(Self(T::from_many(tokens)?));
                    }
                    return match len {
                        0 => Err(Error::end_of_stream(Span::call_site())),
                        1 => Ok(Self(T::from_one(tokens.into_iter().next().unwrap())?)),
                        _ => Ok(Self(T::from_many(tokens)?)),
                    }
                },
                Err(e) => return Err(e)
            }
        };

    }
}

/// Extractor that matches until a comma is found.
///
/// This is equivalent to [`Punctuated<T, ','>`]. 
/// 
/// Since type aliases cannot be extractors as of now,
/// we define a new struct for ergonomics.
#[derive(Debug, Default)]
pub struct CommaExtractor<T>(pub T);

/// Extractor that matches until a colon is found.
///
/// This is equivalent to [`Punctuated <T, ':'>`]. 
/// 
/// Since type aliases cannot be extractors as of now,
/// we define a new struct for ergonomics.
#[derive(Debug, Default)]
pub struct ColonExtractor<T>(pub T);

/// Extractor that matches until a semicolon is found.
///
/// This is equivalent to [`Punctuated<T, ';'>`]
/// 
/// Since type aliases cannot be extractors as of now,
/// we define a new struct for ergonomics.
#[derive(Debug, Default)]
pub struct SemiColonExtractor<T>(pub T);

macro_rules! impl_punct_extract {
    ($($name: ident, $punct: literal);*) => {
        $(impl<T> Extractor for $name<T> where T: FromMacro{
            fn extract(iter: &mut impl Iterator<Item=proc_macro2::TokenTree>) -> Result<Self, crate::Error> {
                let Punctuated(item) = Punctuated::<T, $punct>::extract(iter)?;
                Ok(Self(item))
            }
        })*
    };
}

impl_punct_extract!(CommaExtractor, ','; ColonExtractor, ':'; SemiColonExtractor, ';');

/// Separator of expressions.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExprSep {
    Comma,
    SemiColon,
    FatArrow,
    EndOfStream,
}

/// Extracts an expression according to Rust's rule.
/// Ends when encountering `,`, `;`, `=>` or end of stream.
/// # Example:
/// ```
/// # use macroex::*;
/// # use quote::quote;
/// # fn main() -> Result<(), Error> {
/// let mut iter = quote!(Red=Green.Blue,Yellow=>brown==orange;purple).into_iter();
/// let RustExpr(Stringify(next), _) = iter.extract()?;
/// assert_eq!(next, "Red = Green . Blue");
/// let RustExpr(Stringify(next), _) = iter.extract()?;
/// assert_eq!(next, "Yellow");
/// let RustExpr(Stringify(next), _) = iter.extract()?;
/// assert_eq!(next, "brown = =orange");
/// let RustExpr(Stringify(next), _) = iter.extract()?;
/// assert_eq!(next, "purple");
/// let EndOfStream = iter.extract()?;
/// # Ok(())}
/// ```
pub struct RustExpr<T: FromMacro>(pub T, pub ExprSep);

impl<T: FromMacro> Extractor for RustExpr<T> {
    fn extract(iter: &mut impl Iterator<Item=TokenTree>) -> Result<Self, Error> {
        let mut collected = Vec::new();
        let mut last_is_eq = None;
        loop {
            match iter.next() {
                Some(TokenTree::Punct(p)) if p.as_char() == ',' => {
                    let All(item) = collected.into_iter().extract()?;
                    return Ok(Self(item, ExprSep::Comma));
                },
                Some(TokenTree::Punct(p)) if p.as_char() == ';' => {
                    let All(item) = collected.into_iter().extract()?;
                    return Ok(Self(item, ExprSep::SemiColon));
                },
                Some(TokenTree::Punct(p)) if p.as_char() == '=' && p.spacing() == Spacing::Joint => {
                    last_is_eq = Some(TokenTree::Punct(p));
                    continue;
                },
                Some(TokenTree::Punct(p)) if p.as_char() == '>' && last_is_eq.is_some() => {
                    let All(item) = collected.into_iter().extract()?;
                    return Ok(Self(item, ExprSep::FatArrow));
                },
                Some(tt) => collected.push(tt),
                None =>  {
                    let All(item) = collected.into_iter().extract()?;
                    return Ok(Self(item, ExprSep::EndOfStream));
                },
            };
            if let Some(tt) = mem::take(&mut last_is_eq) {
                collected.push(tt)
            }
        };
    }
}
