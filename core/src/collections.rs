use std::{collections::{HashSet, BTreeSet, VecDeque}, hash::Hash, iter::{repeat, once}, fmt::Debug};

use proc_macro2::{TokenTree, TokenStream, Delimiter, Group, Punct, Spacing, Literal};

use crate::{FromMacro, CommaExtractor, Error, Bracketed, EndOfStream, StreamExtract, LitSome, Parenthesized, All, abort, Iter, SemiColonExtractor};

/// [`Optional`] provides an option type extractor.
/// 
///  Since [`Option<T>`] is ideomatic for default value handling in `macroex`, 
/// [`Optional`] should be used instead. 
/// 
/// * `from_one`: `None` matches the None case, everything else matches the Some case
/// * `from_many`: `Some ()`  matches everything in (), everything else matches all.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
struct Optional<T>(pub Option<T>);

impl<T: FromMacro> FromMacro for Optional<T> {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        match &tt {
            TokenTree::Ident(i) if i.to_string() == "None" => return Ok(Self(None)),
            _ => Ok(Self(Some(T::from_one(tt)?))),
        }
    }

    fn from_many(tokens: proc_macro2::TokenStream) -> Result<Self, Error> {
        let mut iter = tokens.clone().into_iter();
        let mut syn_some = || -> Result<TokenStream, Error>{
            let LitSome = iter.extract()?;
            let Parenthesized(group) = iter.extract()?;
            let EndOfStream = iter.extract()?;
            Ok(group)
        };
        if let Ok(stream) = syn_some() {
            let All(item) = stream.into_iter().extract()?;
            Ok(Self(Some(item)))
        } else {
            let All(item) = tokens.into_iter().extract()?;
            Ok(Self(Some(item)))
        }
    }
}

/// Extractor that rewrites `(..)` to `[..]`
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TupleList<T>(pub T);

impl<T> FromMacro for TupleList<T> where T: FromMacro {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        match tt {
            TokenTree::Group(g) if g.delimiter() == Delimiter::Parenthesis => {
                Ok(Self(T::from_one(
                    TokenTree::Group(
                        Group::new(Delimiter::Bracket, g.stream())
                    )
                )?))
            },
            tt => abort!(tt.span(), ExpectTokenTree("(..)", tt))
        }
    }
}

impl<T> IntoIterator for TupleList<T> where T: IntoIterator {
    type Item = T::Item;

    type IntoIter = T::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}


impl<T: FromMacro, const N: usize> FromMacro for [T; N] {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        let span = tt.span();
        let mut result = Vec::new();
        let Bracketed(Iter(mut iter)) = Bracketed::from_one(tt)?;
        loop {
            match iter.extract() {
                Ok(CommaExtractor(item)) => result.push(item),
                Err(e) if e.is_end_of_stream() => {
                    iter.extract::<EndOfStream>()?;
                    match result.try_into() {
                        Ok(r) => return Ok(r),
                        Err(_) => abort!(span, LengthMismatch)
                    }
                },
                Err(e) => return Err(e)
            }
        }
    }
}


/// Rewrites integer literals in a group into floating point literals.
/// 
/// This is more flexable and ergonomic than `Vec<Number<T>>`.
/// 
/// # Example
/// ```
/// # use macroex::*;
/// # use quote::quote;
/// # fn main() -> Result<(), Error> {
/// let NumberList::<Vec<f32>>(value) = quote!([1, -2, 3.0, -4.5]).into_iter().extract().unwrap();
/// assert_eq!(&value, &[1.0, -2.0, 3.0, -4.5]);
/// # Ok(())}
/// ```
/// See also [`Number`].
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NumberList<T>(pub T);

impl<T> IntoIterator for NumberList<T> where T: IntoIterator {
    type Item = T::Item;

    type IntoIter = T::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<T> FromMacro for NumberList<T> where T: FromMacro {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        let span = tt.span();
        match tt {
            TokenTree::Group(g) => {
                let mut group = TokenTree::Group(
                    Group::new(
                        g.delimiter(), 
                        g.stream().into_iter().map(|x| match x {
                            TokenTree::Literal(lit) => {
                                if let Ok(lit) = litrs::IntegerLit::try_from(&lit) {
                                    TokenTree::Literal(Literal::f64_unsuffixed(
                                        lit.value::<i64>().map(|x| x as f64).unwrap_or(f64::NAN)
                                    ))
                                } else {
                                    TokenTree::Literal(lit)
                                }
                            },
                            tt => tt
                        }).collect(),
                    )
                );
                group.set_span(span);
                Ok(Self(T::from_one(group)?))
            },
            tt => abort!(span, ExpectTokenTree("[..]", tt)),
        }
    }
}


macro_rules! impl_lists {
    ($($ty: ident, $method: ident $(,$bounds: ident)*);*) => {
$(
impl<T: FromMacro $(+ $bounds)*> FromMacro for $ty<T> {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        let mut result = $ty::new();
        let Bracketed(Iter(mut iter)) = Bracketed::from_one(tt)?;
        loop {
            match iter.extract() {
                Ok(CommaExtractor(item)) => drop(result.$method(item)),
                Err(e) if e.is_end_of_stream() => {
                    iter.extract::<EndOfStream>()?;
                    return Ok(result)
                },
                Err(e) => return Err(e)
            }
        }
    }
})*
    };
}

impl_lists!(Vec, push; VecDeque, push_back; HashSet, insert, Hash, Eq; BTreeSet, insert, Ord);

/// If input `v` is not a Group in [`Delimiter::Bracket`],
/// rewrite it as `[v, v, .., v]` (N times)
/// # Example
/// ```
/// # use macroex::*;
/// # use quote::quote;
/// # fn main() -> Result<(), Error> {
/// let Repeat::<[i32; 4], 4>(arr) = quote!([3,1,4,1]).into_iter().extract().unwrap();
/// assert_eq!(arr, [3, 1, 4, 1]);
/// 
/// let Repeat::<[i32; 4], 4>(arr) = quote!(3).into_iter().extract()?;
/// assert_eq!(arr, [3, 3, 3, 3]);
/// # Ok(())}
/// ```
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Repeat<T, const N: usize>(pub T);

impl<T, const N: usize> FromMacro for Repeat<T, N> where T: FromMacro{
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        let span = tt.span();
        match tt {
            TokenTree::Group(ref g) if g.delimiter() == Delimiter::Bracket => {
                Ok(Self(T::from_one(tt)?))
            },
            tt => {
                let mut group = TokenTree::Group(
                    Group::new(
                        Delimiter::Bracket, 
                        repeat(tt).take(N).flat_map(|x|{
                            [x, TokenTree::Punct(Punct::new(',', Spacing::Alone))]
                        }).collect()
                    )
                );
                group.set_span(span);
                Ok(Self(T::from_one(group)?))
            }
        }
    }
}

/// Extractor for syntax `[T; N]`
/// 
/// # Example
/// ```
/// # use macroex::*;
/// # use quote::quote;
/// # fn main() -> Result<(), Error> {
/// let ArrayRepeat::<String>(arr, len) = quote!(["foo"; 4]).into_iter().extract().unwrap();
/// assert_eq!(arr, "foo");
/// assert_eq!(len, 4);
/// # Ok(())}
/// ```
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ArrayRepeat<T>(pub T, pub usize);

impl<T: FromMacro> FromMacro for ArrayRepeat<T> {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        let Bracketed(Iter(mut iter)) = Bracketed::from_one(tt)?;
        let SemiColonExtractor(item) = iter.extract()?;
        let count = iter.extract()?;
        let EndOfStream = iter.extract()?;
        Ok(Self(item, count))
    }
}


/// If input is of the syntax `[v; N]`, 
/// rewrite the input into `[v, v, .. v,]` (N times)
/// # Example
/// ```
/// # use macroex::*;
/// # use quote::quote;
/// # fn main() -> Result<(), Error> {
/// let Splat(Stringify(value)) = quote!([4; 3]).into_iter().extract().unwrap();
/// assert_eq!(value, "[4 , 4 , 4 ,]");
/// # Ok(())}
/// ```
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Splat<T>(pub T);

impl<T: FromMacro> FromMacro for Splat<T> {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        if let Ok(ArrayRepeat::<TokenStream>(expr, len)) = ArrayRepeat::from_one(tt.clone()){
            let group = TokenTree::Group(
                Group::new(
                    Delimiter::Bracket, 
                    repeat::<TokenStream>(expr).take(len).flat_map(|x|{
                        x.into_iter().chain(once(TokenTree::Punct(Punct::new(',', Spacing::Alone))))
                    }).collect()
                )
            );
            Ok(Self(T::from_one(group)?))
        } else {
            Ok(Self(T::from_one(tt)?))
        }
    }
}