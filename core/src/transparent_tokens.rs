//! implements no changes versions of [`ToTokens`]
use quote::ToTokens;

use crate::*;

macro_rules! raw {
    ($($name: ident),* $(,)?) => {
        $(impl quote::ToTokens for $name{
            fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
                self.0.to_tokens(tokens)
            }
        })*
    };
}

macro_rules! no_bounds {
    ($($name: ident),* $(,)?) => {
        $(impl<T> quote::ToTokens for $name<T> where T: ToTokens{
            fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
                self.0.to_tokens(tokens)
            }
        })*
    };
}

raw!(
    IdentString, ParenthesizedGroup, BracketedGroup, CurlyBracedGroup,
);

no_bounds!(
    Number, NumberList, Splat, ArrayRepeat, TupleList,
    Parenthesized, Bracketed, CurlyBraced,
    NotParenthesized, NotBracketed, NotCurlyBraced,
);


impl<T, const N: usize> quote::ToTokens for Repeat<T, N> where T: ToTokens {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.0.to_tokens(tokens)
    }
}
