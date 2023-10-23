use proc_macro2::{TokenStream, Ident, TokenTree, Span, Literal};
use proc_macro_error::abort;
use syn::{DeriveInput, DataEnum, Generics, Attribute, FieldsNamed, FieldsUnnamed};
use quote::{quote, format_ident};

fn to_converter(case: &str, span: Span) -> fn(String) -> String {
    use convert_case::Case;
    match case {
        "lowercase" => |x| convert_case::Casing::to_case(&x, Case::Flat),
        "UPPERCASE" => |x| convert_case::Casing::to_case(&x, Case::UpperFlat),
        "PascalCase" => |x| convert_case::Casing::to_case(&x, Case::Pascal),
        "camelCase" => |x| convert_case::Casing::to_case(&x, Case::Camel),
        "snake_case" => |x| convert_case::Casing::to_case(&x, Case::Snake),
        "SCREAMING_SNAKE_CASE" => |x| convert_case::Casing::to_case(&x, Case::UpperSnake),
        "kebab-case" => |x| convert_case::Casing::to_case(&x, Case::Kebab),
        "SCREAMING-KEBAB-CASE" => |x| convert_case::Casing::to_case(&x, Case::UpperKebab),
        _ => abort!(span, "Invalid caseing, see https://serde.rs/container-attrs.html#rename_all for a list of supported casings.")
    }
}

fn parse_main_attr(attr: TokenStream, name: &mut String, rename: &mut fn(String) -> String) {
    let mut iter = attr.into_iter();
    loop {
        match iter.next() {
            Some(TokenTree::Ident(ident)) => {
                match ident.to_string().as_str() {
                    "rename" => {
                        match iter.next() {
                            Some(TokenTree::Punct(p)) if p.as_char() == '=' => (),
                            Some(tt) => abort!(tt.span(), r#"Expected `=`"#),
                            None => abort!(ident.span(), r#"Expected `=`"#),
                        };
                        match iter.next() {
                            Some(tt) => {
                                if let Ok(str) = litrs::StringLit::try_from(&tt) {
                                    *name = str.into_value().into_owned()
                                } else {
                                    abort!(tt.span(), "Expected string literal.")
                                }
                            }
                            None => abort!(ident.span(), "Expected string literal."),
                        }
                    },
                    "rename_all" => {
                        match iter.next() {
                            Some(TokenTree::Punct(p)) if p.as_char() == '=' => (),
                            Some(tt) => abort!(tt.span(), r#"Expected `=`"#),
                            None => abort!(ident.span(), r#"Expected `=`"#),
                        };
                        match iter.next() {
                            Some(tt) => {
                                if let Ok(str) = litrs::StringLit::try_from(&tt) {
                                    *rename = to_converter(str.value(), tt.span()) 
                                } else {
                                    abort!(tt.span(), "Expected string literal.")
                                }
                            }
                            None => abort!(ident.span(), "Expected string literal."),
                        }
                    },
                    _ => abort!(ident.span(), r#"Expected one of "rename" or "rename_all"."#)
                }
            }
            None => return,
            Some(tt) => abort!(tt.span(), r#"Expected one of "rename" or "rename_all"."#)
        }
    }
}


fn parse_attrs(attr: TokenStream, default: &mut Option<TokenStream>, repeat: &mut bool, required: &mut bool, rename: &mut Option<String>) {
    let mut iter = attr.into_iter();
    loop {
        match iter.next() {
            Some(TokenTree::Ident(ident)) => {
                match ident.to_string().as_str() {
                    "required" => *required = true,
                    "repeat" => *repeat = true,
                    "default" => {
                        match iter.next() {
                            Some(TokenTree::Punct(p)) if p.as_char() == '=' => (),
                            Some(tt) => abort!(tt.span(), r#"Expected `=`"#),
                            None => abort!(ident.span(), r#"Expected `=`"#),
                        };
                        match iter.next() {
                            Some(tt) => {
                                if let Ok(str) = litrs::StringLit::try_from(&tt) {
                                    *default = match syn::parse_str(&str.value()) {
                                        Ok(v) => Some(v),
                                        Err(err) => abort!(tt.span(), "{}", err),
                                    }
                                } else {
                                    abort!(tt.span(), "Expected string literal.")
                                }
                            }
                            None => abort!(ident.span(), "Expected string literal."),
                        }
                    },
                    "rename" => {
                        match iter.next() {
                            Some(TokenTree::Punct(p)) if p.as_char() == '=' => (),
                            Some(tt) => abort!(tt.span(), r#"Expected `=`"#),
                            None => abort!(ident.span(), r#"Expected `=`"#),
                        };
                        match iter.next() {
                            Some(tt) => {
                                if let Ok(str) = litrs::StringLit::try_from(&tt) {
                                    *rename = Some(str.into_value().into_owned()) 
                                } else {
                                    abort!(tt.span(), "Expected string literal.")
                                }
                            }
                            None => abort!(ident.span(), "Expected string literal."),
                        }
                    },
                    _ => abort!(ident.span(), r#"Expected one of "required", "repeat", "default" or "rename"."#)
                };
                match iter.next() {
                    Some(TokenTree::Punct(p)) if p.as_char() == ',' => (),
                    None => return,
                    Some(tt) => abort!(tt.span(), r#"Expected `,`"#)
                }
            }
            None => return,
            Some(tt) => abort!(tt.span(), r#"Expected one of "required", "repeat", "default" or "rename"."#)
        }
    }
} 

pub fn attr_extract<'t> (attrs: &'t [Attribute]) -> impl IntoIterator<Item = TokenStream> + 't{
    attrs.into_iter().filter_map(|x| match &x.meta {
        syn::Meta::List(list) => {
            if list.path.is_ident(&format_ident!("macroex")) {
                Some(list.tokens.clone())
            } else {
                None
            }
        },
        _ => None,
    })
}

pub fn from_struct(fields: FieldsNamed, global_rename: fn(String) -> String) -> Vec<TokenStream> {
    let mut result_fields = Vec::new();
        
    for field in fields.named {
        let ident = field.ident.unwrap();
        let ty = field.ty;
        let mut default = None;
        let (mut repeat, mut required) = (false, false);
        let mut rename = None;

        for attr in attr_extract(&field.attrs){ 
            parse_attrs(attr, &mut default, &mut repeat, &mut required, &mut rename);
        }

        let name = rename.unwrap_or_else(|
            |global_rename(ident.to_string())
        );

        let field_row = if repeat {
            quote!(
                #ident: map.remove_all(#name).into_iter()
                    .map(|x| <#ty as ::std::iter::IntoIterator>::Item::from_tokens(x))
                    .collect::<Result<_,_>>()?
            )
        } else if let Some(default) = default{
            quote!(#ident: match map.remove(#name) {
                Some(x) => <#ty>::from_tokens(x)?,
                None => #default,
            })
        } else if required{
            quote!(#ident: <#ty>::from_tokens(match map.remove(#name){
                Some(value) => value,
                None => bail!(span, "Required field `{}` not found.", #name),
            })?)
        } else {
            quote!(#ident: match map.remove(#name) {
                Some(x) => <#ty>::from_tokens(x)?,
                None => ::std::default::Default::default(),
            })
        };
        result_fields.push(field_row);
    }
    result_fields
}

pub fn from_tuple(fields: FieldsUnnamed) -> Vec<TokenStream> {
    let mut result_fields = Vec::new();
    let len = Literal::usize_unsuffixed(fields.unnamed.len());
    for (index, field) in fields.unnamed.into_iter().enumerate() {
        let index = Literal::usize_unsuffixed(index);
        let ty = field.ty;
        let conv_line = quote!(<#ty>::from_tokens(match tuple.next(){
            Some(x) => x,
            None => bail!(span, "Expected {} items, found {}.", #len, #index),
        })?);
        result_fields.push(conv_line);
    }
    result_fields
}

pub fn format_struct(name: Ident, string_name: String, generics: &Generics, fields: Vec<TokenStream>) -> TokenStream{
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    quote! {
#[automatically_derived]
const _: () = {
    use ::macroex::{FromMacro, FieldMap, IdentString, EitherStream, NamedStructExtractor, bail};
    use ::macroex::proc_macro2::{TokenTree, TokenStream};
    impl #impl_generics FromMacro for #name #ty_generics #where_clause {
        fn from_one(tt: TokenTree) -> Result<Self, macroex::Error> {
            let span = tt.span();
            let mut map = <FieldMap<IdentString, EitherStream>>::from_one(tt)?;
            Ok(Self {#(#fields),*})
        }
        fn from_many(tokens: proc_macro2::TokenStream) -> Result<Self, macroex::Error> {
            let NamedStructExtractor(name, items) = NamedStructExtractor::from_many(tokens)?;
            if name.to_string() != #string_name {
                bail!(name.span(), "Expected {}, found {}", stringify!(#name), name);
            }
            Self::from_one(TokenTree::Group(items))
        }
    }
};
    }
}

pub fn format_idents(name: Ident,  generics: &Generics, idents: Vec<String>) -> TokenStream{
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    quote! {
        #[automatically_derived]
        const _: () = {
            use ::macroex::proc_macro2::{Ident, Span};
            impl #impl_generics #name #ty_generics #where_clause {
                pub const IDENTS: &'static [&'static str] = &[#(#idents,)*];
                pub fn idents() -> Vec<Ident> {
                    Self::IDENTS.into_iter().map(|x| Ident::new(x, Span::call_site())).collect()
                }
            }
        };
            }
}

pub fn format_tuple_struct(name: Ident, string_name: String, generics: &Generics, fields: Vec<TokenStream>) -> TokenStream{
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    quote!{
#[automatically_derived]
const _: () = {
    use ::macroex::{FromMacro, FieldMap, IdentString, EitherStream, TupleList, TupleStructExtractor, bail};
    use ::macroex::proc_macro2::{TokenTree, TokenStream};
    impl #impl_generics FromMacro for #name #ty_generics #where_clause {
        fn from_one(tt: TokenTree) -> Result<Self, macroex::Error> {
            let span = tt.span();
            let mut tuple = <TupleList<::std::vec::Vec<EitherStream>>>::from_one(tt)?.into_iter();
            Ok(Self(#(#fields),*))
        }
        fn from_many(tokens: proc_macro2::TokenStream) -> Result<Self, macroex::Error> {
            let TupleStructExtractor(name, items) = TupleStructExtractor::from_many(tokens)?;
            if name.to_string() != #string_name {
                bail!(name.span(), "Expected {}, found {}", stringify!(#name), name);
            }
            Self::from_one(TokenTree::Group(items))
        }
    }
};
    }.into()
}

pub fn format_unit(name: Ident, string_name: String, generics: &Generics) -> TokenStream{
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    quote!{
#[automatically_derived]
const _: () = {
    use ::macroex::{FromMacro, FieldMap, IdentString, EitherStream, StructExtractor, bail};
    use ::macroex::proc_macro2::{TokenTree, TokenStream};
    impl #impl_generics FromMacro for #name #ty_generics #where_clause {
        fn from_one(tt: TokenTree) -> Result<Self, macroex::Error> {
            let span = tt.span();
            let IdentString(ident) = IdentString::from_one(tt)?;
            if ident == #string_name {
                Ok(Self)
            } else {
                bail!(span, "Expected {}, found {}.", #string_name, ident);
            }
        }
    }
};
    }.into()
}

pub fn from_enum(enum_ident: Ident, generics: &Generics, attrs: Vec<Attribute>, data: DataEnum) -> TokenStream {
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let mut _name = String::new();
    let mut variant_rename: fn(String) -> String = |s| s;

    for attr in attr_extract(&attrs) {
        parse_main_attr(attr, &mut _name, &mut variant_rename);
    };

    let mut unit_branches = Vec::new();
    let mut tuple_branches = Vec::new();
    let mut struct_branches = Vec::new();

    for variant in data.variants {
        let ident = variant.ident;
        let mut string_name = variant_rename(ident.to_string());
        let mut rename: fn(String) -> String = |x| x;
        for attr in attr_extract(&variant.attrs) {
            parse_main_attr(attr, &mut string_name, &mut rename);
        };
        match variant.fields {
            syn::Fields::Named(named) => {
                let arms = from_struct(named, rename);
                struct_branches.push(quote!(
                    #string_name => Ok(#enum_ident::#ident {
                        #(#arms),*
                    })
                ));
                if arms.len() == 0 {
                    unit_branches.push(quote!(
                        #string_name => #enum_ident::#ident{}
                    ))
                }
            },
            syn::Fields::Unnamed(unnamed) => {
                let arms = from_tuple(unnamed);
                tuple_branches.push(quote!(
                    #string_name => Ok(#enum_ident::#ident (
                        #(#arms),*
                    ))
                ));
                if arms.len() == 0 {
                    unit_branches.push(quote!(
                        #string_name => #enum_ident::#ident()
                    ))
                }
            }
            syn::Fields::Unit => {
                unit_branches.push(quote!(
                    #string_name => #enum_ident::#ident
                ))
            },
        };
    }

    quote!{
#[automatically_derived]
#[allow(unreachable_code)]
const _: () = {
    use ::macroex::{FromMacro, FieldMap, IdentString, EitherStream, StructExtractor, TupleList, bail};
    use ::macroex::proc_macro2::{TokenTree, TokenStream, Delimiter};
    use ::std::collections::HashMap;
    use ::std::vec::Vec;
    impl #impl_generics FromMacro for #enum_ident #ty_generics #where_clause {
        fn from_one(tt: TokenTree) -> Result<Self, macroex::Error> {
            let span = tt.span();
            let IdentString(ident) = IdentString::from_one(tt)?;
            Ok(match ident.as_str() {
                #(#unit_branches,)*
                _ => bail!(span, "Invalid variant {} for enum {}", ident, stringify!(#enum_ident)),
            })
        }
        fn from_many(tokens: TokenStream) -> Result<Self, ::macroex::Error> {
            let StructExtractor(name, group) = StructExtractor::from_many(tokens)?;
            let mut map = ::std::default::Default::default();
            let mut tuple = ::std::default::Default::default();
            match group.delimiter() {
                Delimiter::Brace => {
                    map = <FieldMap<IdentString, EitherStream>>::from_one(group.into())?;
                }
                Delimiter::Parenthesis => {
                    tuple = <TupleList<Vec<EitherStream>>>::from_one(group.into())?.into_iter();
                }
                _ => return Err(::macroex::Error::unreachable()),
            }
            
            let span = name.span();
            match name.to_string().as_str() {
                #(#tuple_branches,)*
                #(#struct_branches,)*
                _ => bail!(span, "Invalid variant {} for enum {}", name, stringify!(#enum_ident)),
            }
        }
    }
};
    }
}

pub fn from_macro(tokens: TokenStream) -> TokenStream {
    let ast: DeriveInput = syn::parse2(tokens).unwrap();
    let name = ast.ident;
    match ast.data {
        syn::Data::Union(union) => abort!(union.union_token.span, "Union not supported."),
        syn::Data::Struct(s) => {
            match s.fields {
                syn::Fields::Named(fields) => {
                    let mut string_name = name.to_string();
                    let mut rename: fn(String) -> String = |s| s;
                    for attr in attr_extract(&ast.attrs) {
                        parse_main_attr(attr, &mut string_name, &mut rename);
                    };
                    let field_idents = fields.named.iter().map(|x|x.ident.as_ref().unwrap().to_string()).collect();
                    let fields = from_struct(fields, rename);
                    let a = format_struct(name.clone(), string_name, &ast.generics, fields);
                    let b = format_idents(name, &ast.generics, field_idents);
                    quote!(#a #b)
                }
                syn::Fields::Unnamed(fields) => {
                    let mut string_name = name.to_string();
                    let mut rename: fn(String) -> String = |s| s;
                    for attr in attr_extract(&ast.attrs) {
                        parse_main_attr(attr, &mut string_name, &mut rename);
                    };
                    let fields = from_tuple(fields);
                    format_tuple_struct(name, string_name, &ast.generics, fields)
                }
                syn::Fields::Unit => {
                    let mut string_name = name.to_string();
                    let mut rename: fn(String) -> String = |s| s;
                    for attr in attr_extract(&ast.attrs) {
                        parse_main_attr(attr, &mut string_name, &mut rename);
                    };
                    format_unit(name, string_name, &ast.generics)
                },
            }
        }
        syn::Data::Enum(e) => from_enum(name, &ast.generics, ast.attrs, e),
    }
}