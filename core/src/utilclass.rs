use std::{borrow::Borrow, str::FromStr};

use proc_macro2::{TokenTree, Ident, Span, Group, TokenStream, Delimiter};

use crate::{FromMacro, Error, StreamExtract, EndOfStream, CommaExtractor, PunctOf, abort, All, Either, EitherStream};

/// Records the span from the incoming TokenTree
#[derive(Debug)]
pub struct Spanned<T: FromMacro>(pub Span, pub T);

impl<T> Default for Spanned<T> where T: FromMacro + Default{
    fn default() -> Self {
        Spanned(Span::call_site(), T::default())
    }
}

impl<T> FromMacro for Spanned<T> where T: FromMacro{
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        Ok(Self(tt.span(), T::from_one(tt)?))
    }

    fn from_many(tokens: proc_macro2::TokenStream) -> Result<Self, Error> {
        Ok(Self(Span::call_site(), T::from_many(tokens)?))
    }
}

/// Extacts a [`Group`] enclosed in parenthesis `()`.
pub struct ParenthesizedGroup(pub Group);
/// Extacts a [`Group`] enclosed in brackets `[]`.
pub struct BracketedGroup(pub Group);
/// Extacts a [`Group`] enclosed in curly braces `{}`.
pub struct CurlyBracedGroup(pub Group);

macro_rules! impl_groups {
    ($($name: ident, $delim: ident, $lit: literal);*) => {
        $(impl FromMacro for $name {
            fn from_one(tt: TokenTree) -> Result<Self, Error> {
                match tt {
                    TokenTree::Group(group) => {
                        if group.delimiter() == proc_macro2::Delimiter::$delim {
                            Ok(Self(group))
                        } else {
                            abort!(group.span(), ExpectTokenTree($lit, TokenTree::Group(group)))
                        }
                    },
                    tt => abort!(tt.span(), ExpectTokenTree("TokenTree::Group", tt))
                }
            }

            fn peek(tt: &TokenTree) -> bool {
                match tt {
                    TokenTree::Group(group) if group.delimiter() ==proc_macro2::Delimiter::$delim => {
                        true
                    },
                    _ => false,
                }
            }
        })*
    };
}

impl_groups!(
    ParenthesizedGroup, Parenthesis, "()"; 
    BracketedGroup, Bracket, "[]"; 
    CurlyBracedGroup, Brace, "{}"
);


/// Extracts a [`TokenStream`] enclosed in parenthesis `()`.
pub struct Parenthesized<T>(pub T);
/// Extracts a [`TokenStream`] enclosed in brackets `[]`.
pub struct Bracketed<T>(pub T);
/// Extracts a [`TokenStream`] enclosed in curly braces `{}`.
pub struct CurlyBraced<T>(pub T);

macro_rules! impl_group_extract {
    ($($name: ident, $delim: ident, $lit: literal);*) => {
        $(impl<T: FromMacro> FromMacro for $name<T> {
            fn from_one(tt: TokenTree) -> Result<Self, Error> {
                match tt {
                    TokenTree::Group(group) => {
                        if group.delimiter() == proc_macro2::Delimiter::$delim {
                            let All(item) = group.stream().into_iter().extract()?;
                            Ok(Self(item))
                        } else {
                            abort!(group.span(), ExpectTokenTree($lit, TokenTree::Group(group)))
                        }
                    },
                    tt => abort!(tt.span(), ExpectTokenTree("TokenTree::Group", tt))
                }
            }

            fn peek(tt: &TokenTree) -> bool {
                match tt {
                    TokenTree::Group(group) if group.delimiter() ==proc_macro2::Delimiter::$delim => {
                        true
                    },
                    _ => false,
                }
            }
        })*
    };
}

impl_group_extract!(
    Parenthesized, Parenthesis, "(..)"; 
    Bracketed, Bracket, "[..]"; 
    CurlyBraced, Brace, "{..}"
);


/// Extacts anything that's not in a [`Group`] enclosed in parenthesis `()`.
/// 
/// Useful for syntax consistancy when using with [`Either`]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NotParenthesized<T>(pub T);
/// Extacts anything that's not in a [`Group`] enclosed in brackets `[]`.
/// 
/// Useful for syntax consistancy when using with [`Either`]
pub struct NotBracketed<T>(pub T);
/// Extacts anything that's not in a [`Group`] enclosed in curly braces `{}`.
/// 
/// Useful for syntax consistancy when using with [`Either`]
pub struct NotCurlyBraced<T>(pub T);

macro_rules! impl_not_group_extract {
    ($($name: ident, $delim: ident, $lit: literal);*) => {
        $(impl<T: FromMacro> FromMacro for $name<T> {
            fn from_one(tt: TokenTree) -> Result<Self, Error> {
                match tt {
                    TokenTree::Group(group)
                        if group.delimiter() == proc_macro2::Delimiter::$delim => {
                        abort!(group.span(), ExpectTokenTree($lit, TokenTree::Group(group)))
                    },
                    tt => Ok(Self(T::from_one(tt)?))
                }
            }

            fn peek(tt: &TokenTree) -> bool {
                match tt {
                    TokenTree::Group(group) if group.delimiter() == proc_macro2::Delimiter::$delim => {
                        false
                    },
                    _ => true,
                }
            }
        })*
    };
}

impl_not_group_extract!(
    NotParenthesized, Parenthesis, "anything but (..)"; 
    NotBracketed, Bracket, "anything but [..]"; 
    NotCurlyBraced, Brace, "anything but {..}"
);


/// Extracts a string from an ident.
/// # Example
/// ```
/// # use macroex::*;
/// # use quote::quote;
/// # pub fn main() -> Result<(), Error> {
/// let IdentString(string) = quote!(rust).into_iter().extract()?;
/// assert_eq!(string, "rust");
/// # Ok(()) }
/// ```
#[derive(Debug, Default, Hash, PartialEq, Eq)]
pub struct IdentString(pub String);

impl From<Ident> for IdentString {
    fn from(ident: Ident) -> Self {
        Self(ident.to_string())
    }
}

impl Borrow<str> for IdentString {
    fn borrow(&self) -> &str {
        self.0.as_str()
    }
}

impl FromMacro for IdentString {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        match tt {
            TokenTree::Ident(ident) => {
                Ok(Self(ident.to_string()))
            },
            tt => abort!(tt.span(), ExpectTokenTree("TokenTree::Ident", tt))
        }
    }
}


/// Parse a string as [`TokenStream`] and extract into T.
/// # Example
/// ```
/// # use macroex::*;
/// # use quote::quote;
/// # pub fn main() -> Result<(), Error> {
/// let StringTokens(Iter(mut iter)) = quote!("1 2 3").into_iter().extract()?;
/// let a: i32 = iter.extract()?;
/// let b: i32 = iter.extract()?;
/// let c: i32 = iter.extract()?;
/// assert_eq!((a, b, c), (1, 2, 3));
/// # Ok(()) }
/// ```
#[derive(Debug, Default, Hash, PartialEq, Eq)]
pub struct StringTokens<T: FromMacro>(pub T);

impl<T: FromMacro> FromMacro for StringTokens<T> {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        let span = tt.span();
        let s = String::from_one(tt)?;
        let stream = match TokenStream::from_str(&s) {
            Ok(ts) => ts,
            Err(e) => abort!(span, LexError(e))
        };
        let All(result) = stream.into_iter().extract()?;
        Ok(Self(result))
    }
}

/// Convert input tokens to a String using the `to_string` method.
/// # Example
/// ```
/// # use macroex::*;
/// # use quote::quote;
/// # pub fn main() -> Result<(), Error> {
/// let All(Stringify(string)) = quote!(hello+world).into_iter().extract()?;
/// assert_eq!(string, "hello + world");
/// # Ok(()) }
/// ```
#[derive(Debug, Default, Hash, PartialEq, Eq)]
pub struct Stringify(pub String);

impl FromMacro for Stringify {
    const PREFER_MANY: bool = true;
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        Ok(Self(tt.to_string()))
    }

    fn from_many(tokens: TokenStream) -> Result<Self, Error> {
        Ok(Self(tokens.to_string()))
    }
}


/// Extracts something that's not a specific punct.
pub(crate) struct NotPunct<const P: char>(pub Option<proc_macro2::TokenTree>);

impl<const P: char> FromMacro for NotPunct<P> {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        match &tt {
            TokenTree::Punct(p) if p.as_char() == P => {
                Ok(Self(None))
            }
            _ => Ok(Self(Some(tt)))
        }
    }
}

/// Extracts an expression like `Name (..)`
pub struct TupleStructExtractor(pub Ident, pub Group);

impl FromMacro for TupleStructExtractor {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        abort!(tt.span(), ExpectMany)
    }

    fn from_many(tokens: proc_macro2::TokenStream) -> Result<Self, Error> {
        let mut iter = tokens.into_iter();
        let ident = iter.extract()?;
        let ParenthesizedGroup(group) = iter.extract()?;
        iter.extract::<EndOfStream>()?;
        Ok(TupleStructExtractor(ident, group))
    }

}

/// Extracts an expression like `Name {..}`
pub struct NamedStructExtractor(pub Ident, pub Group);

impl FromMacro for NamedStructExtractor {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        abort!(tt.span(), ExpectMany)
    }

    fn from_many(tokens: proc_macro2::TokenStream) -> Result<Self, Error> {
        let mut iter = tokens.into_iter();
        let ident = iter.extract()?;
        let CurlyBracedGroup(group) = iter.extract()?;
        iter.extract::<EndOfStream>()?;
        Ok(NamedStructExtractor(ident, group))
    }
}


/// Extracts an expression like `Name {..}` or `Name (..)`
pub struct StructExtractor(pub Ident, pub Group);

impl FromMacro for StructExtractor {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        abort!(tt.span(), ExpectMany)
    }

    fn from_many(tokens: proc_macro2::TokenStream) -> Result<Self, Error> {
        let mut iter = tokens.into_iter();
        let ident = iter.extract()?;
        let group = match iter.extract()? {
            Either::A(ParenthesizedGroup(g)) => g,
            Either::B(CurlyBracedGroup(g)) => g,
        };
        iter.extract::<EndOfStream>()?;
        Ok(StructExtractor(ident, group))
    }
}

/// Extracts an extression like `x: 10`, `y: -4.0 * f32::PI`, `z: Vec2 {x: 1, y: 2}`
#[derive(Debug, Default)]
pub struct Field<A: FromMacro, B: FromMacro>(pub A, pub B);

impl<A: FromMacro, B: FromMacro> FromMacro for Field<A, B> {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        abort!(tt.span(), ExpectMany)
    }
    fn from_many(tokens: TokenStream) -> Result<Self, Error> {
        let mut iter = tokens.into_iter();
        let name = iter.extract()?;        
        iter.extract::<PunctOf<':'>>()?;
        let CommaExtractor(rem) = iter.extract()?; 
        Ok(Self(name, rem))
    }
}

/// Matches parsable metalist segment for [`FromAttr`](::macroex_derive::FromAttrs).
/// 
/// * `name`
/// * `name(value)`
/// * `name = value`
/// 
pub struct Meta(pub Ident, pub EitherStream);

impl FromMacro for Meta {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        let ident = Ident::from_one(tt)?;
        Ok(Self(
            ident,
            EitherStream::One(TokenTree::Ident(Ident::new("true", Span::call_site())))
        ))
    }
    fn from_many(tokens: TokenStream) -> Result<Self, Error> {
        let mut iter = tokens.into_iter();
        let name: Ident = iter.extract()?;
        match iter.next() {
            Some(TokenTree::Punct(p)) if p.as_char() == '=' => {
                let tt = iter.extract()?;
                let EndOfStream = iter.extract()?;
                Ok(Self(name, EitherStream::One(tt)))
            },
            Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Parenthesis => {
                Ok(Self(name, EitherStream::Many(g.stream())))
            }
            None => Ok(Self(
                name,
                // we always parse unit to true
                EitherStream::One(TokenTree::Ident(Ident::new("true", Span::call_site())))
            )),
            Some(tt) => {
                abort!(tt.span(), ExpectValidMeta)
            }
        }
    }
}