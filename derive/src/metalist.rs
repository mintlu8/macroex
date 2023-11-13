use proc_macro2::{TokenStream, Ident, TokenTree, Span};
use proc_macro_error::abort;
use syn::{DeriveInput, Generics, Attribute, FieldsNamed};
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

fn parse_main_attr(attr: TokenStream, rename: &mut fn(String) -> String) {
    let mut iter = attr.into_iter();
    loop {
        match iter.next() {
            Some(TokenTree::Ident(ident)) => {
                match ident.to_string().as_str() {
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

        let name = rename.unwrap_or_else(
            ||global_rename(ident.to_string())
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

pub fn format_struct(name: Ident, generics: Generics, fields: Vec<TokenStream>) -> TokenStream{
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    quote! {
#[automatically_derived]
const _: () = {
    use ::macroex::{FromMacro, MetaMap, IdentString, EitherStream, NamedStructExtractor, Parenthesized, bail};
    use ::macroex::proc_macro2::{TokenTree, TokenStream, Span};
    impl #impl_generics FromMacro for #name #ty_generics #where_clause {
        fn from_one(tt: TokenTree) -> Result<Self, macroex::Error> {
            let Parenthesized(span) = Parenthesized::from_one(tt)?;
            Self::from_many(span)
        }
        fn from_many(tokens: proc_macro2::TokenStream) -> Result<Self, macroex::Error> {
            let span = Span::call_site();
            let mut map = MetaMap::from_many(tokens)?;
            Ok(Self {#(#fields),*})
        }
    }
};
    }
}

pub fn from_meta(tokens: TokenStream) -> TokenStream {
    let ast: DeriveInput = syn::parse2(tokens).unwrap();
    let name = ast.ident;
    match ast.data {
        syn::Data::Union(union) => abort!(union.union_token.span, "Only named struct is allowed."),
        syn::Data::Enum(e) => abort!(e.enum_token.span, "Only named struct is allowed."),
        syn::Data::Struct(s) => {
            match s.fields {
                syn::Fields::Unit => {
                    abort!(s.struct_token.span, "Only named struct is allowed.");
                },
                syn::Fields::Unnamed(_) => {
                    abort!(s.struct_token.span, "Only named struct is allowed.");
                }
                syn::Fields::Named(fields) => {
                    let mut rename: fn(String) -> String = |s| s;
                    for attr in attr_extract(&ast.attrs) {
                        parse_main_attr(attr, &mut rename);
                    };
                    let fields = from_struct(fields, rename);
                    format_struct(name, ast.generics, fields)
                }
            }
        }
    }
}