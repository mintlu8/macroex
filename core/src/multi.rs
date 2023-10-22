use proc_macro2::TokenTree;

use crate::{Extractor, FromMacro, Iter, Parenthesisized, StreamExtract, CommaExtractor, EndOfStream};

/// Extracts multiple items sequentiallly.
/// # Example:
/// ```
/// # use macroex::*;
/// # use quote::quote;
/// # fn main() -> Result<(), Error> {
/// let MultiExtractor((a, b, c)) = quote!(1 2.0 '3').into_iter().extract()?;
/// # let _ = 1u8.max(a); let _ = 2.0f32.max(b); let _ = '3'.max(c);
/// assert_eq!(a, 1);
/// assert_eq!(b, 2.0);
/// assert_eq!(c, '3');
/// # Ok(())}
/// ```
#[derive(Debug)]
pub struct MultiExtractor<T>(pub T);

macro_rules! multi_extract_tuple {
    () => {};
    ($this: ident $(,$thing: ident)* $(,)?) => {
        impl<$this, $($thing),*> Extractor for MultiExtractor<($this, $($thing),*)> where $this: Extractor, $($thing: Extractor),*{
            fn extract(iter: &mut impl Iterator<Item=proc_macro2::TokenTree>) -> Result<Self, crate::Error> {
                Ok(Self((
                    $this::extract(iter)?,
                    $($thing::extract(iter)?,)*
                )))
            }
        }
        multi_extract_tuple!($($thing),*);
    };
}

multi_extract_tuple!(A, B, C, D, E, F, G, H, I, J, K, L,);

macro_rules! tuple_impl {
    () => {};
    ($first: ident $(,$thing: ident)* $(,)?) => {
        impl<$first, $($thing),*> FromMacro for ($first, $($thing),*) where $first: FromMacro, $($thing: FromMacro),* {
            fn from_one(tt: TokenTree) -> Result<Self, crate::Error> {
                let Parenthesisized(Iter(mut iter)) = Parenthesisized::from_one(tt)?;
                let result = (
                    {
                        let CommaExtractor(x) = iter.extract::<CommaExtractor<$first>>()?;
                        x
                    },
                    $({
                        let CommaExtractor(x) = iter.extract::<CommaExtractor<$thing>>()?;
                        x
                    },)*
                );
                let EndOfStream = iter.extract()?;
                Ok(result)
            }
        }
        tuple_impl!($($thing),*);
    };
}

tuple_impl!(A, B, C, D, E, F, G, H, I, J, K, L,);