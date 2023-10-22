use proc_macro2::{TokenTree, TokenStream};
use crate::{FromMacro, StreamExtract, Error, EndOfStream, PunctOf, abort, litrs_get, abort_into};

type Neg = PunctOf<'-'>;

macro_rules! get_uint {
    ($($name: ident),*) => {
        $(impl FromMacro for $name {
            fn from_one(tt: TokenTree) -> Result<Self, Error> {
                let lit = litrs_get!(IntegerLit, tt);
                match lit.value() {
                    Some(x) => Ok(x),
                    None => abort!(tt.span(), IntegerOverflow(stringify!($name), tt))
                }
            }
        })*
    };
}

macro_rules! get_int {
    ($($name: ident),*) => {
        $(impl FromMacro for $name {
            fn from_one(tt: TokenTree) -> Result<Self, Error> {
                let lit = litrs_get!(IntegerLit, tt);
                match lit.value() {
                    Some(x) => Ok(x),
                    None => abort!(tt.span(), IntegerOverflow(stringify!($name), tt))
                }
            }
            fn from_many(tokens: TokenStream) -> Result<Self, Error> {
                let mut iter = tokens.into_iter();
                iter.extract::<Neg>()?;
                let result = iter.extract::<Self>();
                iter.extract::<EndOfStream>()?;
                result.map(|x| -x)
            }
        })*
    };
}

macro_rules! get_float {
    ($($name: ident),*) => {
        $(impl FromMacro for $name {
            fn from_one(tt: TokenTree) -> Result<Self, Error> {
                let lit = litrs_get!(FloatLit, tt);
                let float: $name = match lit.number_part().parse() {
                    Ok(x) => x,
                    Err(e) => abort_into!(tt.span(), e),
                };
                Ok(float)
            }
            fn from_many(tokens: TokenStream) -> Result<Self, Error> {
                let mut iter = tokens.into_iter();
                iter.extract::<Neg>()?;
                let result = iter.extract::<Self>();
                iter.extract::<EndOfStream>()?;
                result.map(|x| -x)
            }
        })*
    };
}

impl FromMacro for bool {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        let lit = litrs_get!(BoolLit, tt);
        Ok(lit.value())
    }
}

impl FromMacro for char {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        let lit = litrs_get!(CharLit, tt);
        Ok(lit.value())
    }
}

impl FromMacro for String {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        let lit = litrs_get!(StringLit, tt);
        Ok(lit.into_value().into_owned())
    }
}

/// Specialized extractor for [`u8`] that additionally accepts a byte char literal.
/// # Examples
/// ```
/// # use macroex::*;
/// # use quote::quote;
/// # fn main() -> Result<(), Error> {
/// let mut value = 0u8;
/// Byte(value) = quote!(42).into_iter().extract()?;
/// assert_eq!(value, 42);
/// Byte(value) = quote!(b'@').into_iter().extract()?;
/// assert_eq!(value, b'@');
/// # Ok(())}
#[derive(Debug, Default)]
pub struct Byte(pub u8);

impl FromMacro for Byte {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        if let Ok(lit) = litrs::ByteLit::try_from(&tt) {
            return Ok(Self(lit.value()))
        };
        let lit = litrs_get!(IntegerLit, tt);
        match lit.value() {
            Some(val) => Ok(Self(val)),
            None => abort!(tt.span(), IntegerOverflow("u8", tt)),
        }
    }
}


/// Specialized extractor for [`Vec<u8>`] that additionally accepts a byte string literal.
/// # Examples
/// ```
/// # use macroex::*;
/// # use quote::quote;
/// # fn main() -> Result<(), Error> {
/// let mut value: Vec<u8>;
/// Bytes(value) = quote!([49, 50, 51]).into_iter().extract()?;
/// assert_eq!(value, b"123");
/// Bytes(value) = quote!(b"456").into_iter().extract()?;
/// assert_eq!(value, b"456");
/// # Ok(())}
#[derive(Debug, Default)]
pub struct Bytes(pub Vec<u8>);

impl FromMacro for Bytes {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        match tt {
            TokenTree::Group(_) => {
                Ok(Self(Vec::from_one(tt)?))
            },
            TokenTree::Literal(_) => {
                let lit = litrs_get!(ByteStringLit, tt);
                Ok(Self(lit.into_value().into_owned()))
            },
            tt => abort!(tt.span(), ExpectTokenTree("array or byte string literal", tt))
        }
    }
}

get_uint!(u8, u16, u32, u64, u128, usize);
get_int!(i8, i16, i32, i64, i128, isize);
get_float!(f32, f64);

/// Specialized extractor for [`f32`] and [`f64`] that additionally accepts integers.
/// # Examples
/// ```
/// # use macroex::*;
/// # use quote::quote;
/// # fn main() -> Result<(), Error> {
/// let mut value = 0.0f32;
/// Number(value) = quote!(1).into_iter().extract()?;
/// assert_eq!(value, 1.0);
/// All(Number(value)) = quote!(-1).into_iter().extract()?;
/// assert_eq!(value, -1.0);
/// Number(value) = quote!(1.0).into_iter().extract()?;
/// assert_eq!(value, 1.0);
/// All(Number(value)) = quote!(-1.0).into_iter().extract()?;
/// assert_eq!(value, -1.0);
/// # Ok(())}
#[derive(Debug, Default)]
pub struct Number<T>(pub T);

impl FromMacro for Number<f32> {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        Ok(match i32::from_one(tt.clone()) {
            Ok(int) => Self(int as f32),
            Err(_) => Self(f32::from_one(tt)?),
        })
    }

    fn from_many(tokens: TokenStream) -> Result<Self, Error> {
        Ok(match i32::from_many(tokens.clone()) {
            Ok(int) => Self(int as f32),
            Err(_) => Self(f32::from_many(tokens)?),
        })
    }
}

impl FromMacro for Number<f64> {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        Ok(match i64::from_one(tt.clone()) {
            Ok(int) => Self(int as f64),
            Err(_) => Self(f64::from_one(tt)?),
        })
    }

    fn from_many(tokens: TokenStream) -> Result<Self, Error> {
        Ok(match i64::from_many(tokens.clone()) {
            Ok(int) => Self(int as f64),
            Err(_) => Self(f64::from_many(tokens)?),
        })
    }
}


macro_rules! get_litrs {
    ($($name: ident),*) => {
        $(impl FromMacro for litrs::$name<String> {
            fn from_one(tt: TokenTree) -> Result<Self, Error> {
                Ok(litrs_get!($name, tt))
            }
        })*
    };
}

impl FromMacro for litrs::BoolLit {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        Ok(litrs_get!(BoolLit, tt))
    }
}

get_litrs!(ByteLit, IntegerLit, FloatLit, CharLit, StringLit, ByteStringLit);


impl FromMacro for proc_macro2::TokenTree {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        Ok(tt)
    }
}

impl FromMacro for proc_macro2::TokenStream {
    const PREFER_MANY: bool = true;
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        Ok([tt].into_iter().collect())
    }
    fn from_many(ts: TokenStream) -> Result<Self, Error> {
        Ok(ts)
    }
}

/// Extracts a [`TokenStream::IntoIter`](proc_macro2::token_stream::IntoIter).
/// 
/// This is useful when nested in another extractor like [`Bracketed`](crate::Bracketed)
/// # Examples
/// ```
/// # use macroex::*;
/// # use quote::quote;
/// # fn main() -> Result<(), Error> {
/// let Bracketed(Iter(mut iter)) = quote!([1, 2]).into_iter().extract()?;
/// let USize::<1> = iter.extract()?;
/// let PunctOf::<','> = iter.extract()?;
/// let USize::<2> = iter.extract()?;
/// let EndOfStream = iter.extract()?;
/// # Ok(())}
pub struct Iter(pub proc_macro2::token_stream::IntoIter);

impl FromMacro for Iter {
    const PREFER_MANY: bool = true;
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        let ts: TokenStream = tt.into();
        Ok(Self(ts.into_iter()))
    }
    fn from_many(ts: TokenStream) -> Result<Self, Error> {
        Ok(Self(ts.into_iter()))
    }
}

impl FromMacro for proc_macro2::Ident {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        match tt {
            TokenTree::Ident(ident) => Ok(ident),
            tt => abort!(tt.span(), ExpectTokenTree("TokenTree::Ident", tt))

        }
    }
}

impl FromMacro for proc_macro2::Group {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        match tt {
            TokenTree::Group(group) => Ok(group),
            tt => abort!(tt.span(), ExpectTokenTree("TokenTree::Group", tt))
        }
    }
}

impl FromMacro for proc_macro2::Literal {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        match tt {
            TokenTree::Literal(lit) => Ok(lit),
            tt => abort!(tt.span(), ExpectTokenTree("TokenTree::Literal", tt))
        }
    }
}

impl FromMacro for proc_macro2::Punct {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        match tt {
            TokenTree::Punct(punct) => Ok(punct),
            tt => abort!(tt.span(), ExpectTokenTree("TokenTree::Punct", tt))
        }
    }
}

