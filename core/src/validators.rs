
use proc_macro2::{TokenTree, Spacing, Delimiter};

use crate::{FromMacro, Error, abort, litrs_get};

/// Unit struct validator of a punctuation mark
/// # Example
/// ```
/// # use macroex::*;
/// # use quote::quote;
/// let PunctOf::<'%'> = quote!(%).into_iter().extract().unwrap();
/// ```
pub struct PunctOf<const PUNCT: char>;

impl<const PUNCT: char> FromMacro for PunctOf<PUNCT> {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        match &tt {
            TokenTree::Punct(punct) => {
                if punct.as_char() == PUNCT {
                    Ok(Self)
                } else {
                    abort!(punct.span(), ExpectPunct(PUNCT, tt))
                }
            },
            _ => abort!(tt.span(), ExpectPunct(PUNCT, tt))
        }
    }
}

/// Unit struct validator of a joint punctuation mark.
/// # Example
/// ```
/// # use macroex::*;
/// # use quote::quote;
/// let mut iter = quote!(=>).into_iter();
/// let PunctJoint::<'='> = iter.extract().unwrap();
/// let PunctAlone::<'>'> = iter.extract().unwrap();
/// ```
pub struct PunctJoint<const PUNCT: char>;

impl<const PUNCT: char> FromMacro for PunctJoint<PUNCT> {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        match &tt {
            TokenTree::Punct(punct) => {
                if punct.as_char() == PUNCT && punct.spacing() == Spacing::Joint {
                    Ok(Self)
                } else {
                    abort!(punct.span(), ExpectPunct(PUNCT, tt))
                }
            },
            _ => abort!(tt.span(), ExpectPunct(PUNCT, tt))
        }
    }
}

/// Unit struct validator of a alone punctuation mark.
/// # Example
/// ```
/// # use macroex::*;
/// # use quote::quote;
/// let mut iter = quote!(=>).into_iter();
/// let PunctJoint::<'='> = iter.extract().unwrap();
/// let PunctAlone::<'>'> = iter.extract().unwrap();
/// ```
pub struct PunctAlone<const PUNCT: char>;

impl<const PUNCT: char> FromMacro for PunctAlone<PUNCT> {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        match &tt {
            TokenTree::Punct(punct) => {
                if punct.as_char() == PUNCT && punct.spacing() == Spacing::Alone {
                    Ok(Self)
                } else {
                    abort!(punct.span(), ExpectPunct(PUNCT, tt))
                }
            },
            _ => abort!(tt.span(), ExpectPunct(PUNCT, tt))
        }
    }
}

/// Unit struct validator of an integer literal.
/// # Example
/// ```
/// # use macroex::*;
/// # use quote::quote;
/// let USize::<52> = quote!(52).into_iter().extract().unwrap();
/// ```
pub struct USize<const VALUE: usize>;

impl<const V: usize> FromMacro for USize<V> {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        let lit = litrs_get!(IntegerLit, tt);
        if Some(V) == lit.value(){
            Ok(Self)
        } else {
            abort!(tt.span(), ExpectInt(V, tt))
        }
    }
}

/// Unit struct validator for boolean literal [`true`].
/// # Example
/// ```
/// # use macroex::*;
/// # use quote::quote;
/// let LitTrue = quote!(true).into_iter().extract().unwrap();
/// ```
pub struct LitTrue;

/// Unit struct validator for boolean literal [`false`].
/// # Example
/// ```
/// # use macroex::*;
/// # use quote::quote;
/// let LitFalse = quote!(false).into_iter().extract().unwrap();
/// ```
pub struct LitFalse;

/// Unit struct validator for [`Some`].
/// # Example
/// ```
/// # use macroex::*;
/// # use quote::quote;
/// let LitSome = quote!(Some).into_iter().extract().unwrap();
/// ```
pub struct LitSome;

/// Unit struct validator for [`None`].
/// # Example
/// ```
/// # use macroex::*;
/// # use quote::quote;
/// let LitNone = quote!(None).into_iter().extract().unwrap();
/// ```
pub struct LitNone;

/// Unit struct validator for `()`.
/// 
/// # Example
/// ```
/// # use macroex::*;
/// # use quote::quote;
/// let Unit = quote!(()).into_iter().extract().unwrap();
/// ```
pub struct Unit;


impl FromMacro for LitTrue {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        let lit = litrs_get!(BoolLit, tt);
        if lit.value() == true {
            Ok(Self)
        } else {
            abort!(tt.span(), ExpectIdent("true", tt))
        }
    }
}

impl FromMacro for LitFalse {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        let lit = litrs_get!(BoolLit, tt);
        if lit.value() == false {
            Ok(Self)
        } else {
            abort!(tt.span(), ExpectIdent("false", tt))
        }
    }
}

impl FromMacro for LitSome {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        match tt {
            TokenTree::Ident(ident) if ident.to_string() == "Some" => Ok(Self),
            _ => abort!(tt.span(), ExpectIdent("Some", tt))
        }
    }
}

impl FromMacro for LitNone {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        match tt {
            TokenTree::Ident(ident) if ident.to_string() == "None" => Ok(Self),
            _ => abort!(tt.span(), ExpectIdent("None", tt))
        }
    }
}

impl FromMacro for Unit {
    fn from_one(tt: TokenTree) -> Result<Self, Error> {
        match tt {
            TokenTree::Group(g) if 
                g.delimiter() == Delimiter::Parenthesis &&
                g.stream().is_empty() => {
                Ok(Self)
            },
            tt => abort!(tt.span(), ExpectTokenTree("()", tt)),
        }
    }
}

/// Define your own Ident validator. 
/// 
/// # Example
/// This creates an extractor Null which checks for an ident `null`.
/// ```
/// # use macroex::*;
/// # use quote::quote;
/// ident_validator!(pub Null "null");
/// let Null = quote!(null).into_iter().extract().unwrap();
/// ```
#[macro_export]
macro_rules! ident_validator {
    ($vis: vis $name: ident $value: literal) => {

        $vis struct $name;
        
        impl ::macroex::FromMacro for $name {
            fn from_one(tt: ::macroex::proc_macro2::TokenTree) -> Result<Self, ::macroex::Error> {
                match &tt {
                    ::macroex::proc_macro2::TokenTree::Ident(ident) if ident.to_string() == $value => Ok(Self),
                    _ => ::macroex::bail!(tt.span(), "Expected {}, {} found", $value, tt)
                }
            }
        }
    };
}