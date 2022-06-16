extern crate proc_macro;

use convert_case::{Case, Casing};
use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, quote, quote_spanned, ToTokens};
use syn::spanned::Spanned;
use syn::{Data, Error, Fields, Type};

macro_rules! derive_error {
    ($string: tt) => {
        Error::new(Span::call_site(), $string)
            .to_compile_error()
            .into()
    };
}

#[proc_macro_derive(GetParams)]
pub fn get_params_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_get_params(&ast)
}

fn impl_get_params(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let data = &ast.data;

    let mut variant_match_arms;

    match data {
        Data::Enum(data_enum) => {
            variant_match_arms = TokenStream2::new();

            for variant in &data_enum.variants {
                let variant_name = &variant.ident;

                // Variant can have unnamed fields like `Variant(i32, i64)`
                // Variant can have named fields like `Variant {x: i32, y: i32}`
                // Variant can be named Unit like `Variant`
                match &variant.fields {
                    Fields::Unnamed(fields) => {
                        let mut alphabet = (b'a'..=b'z')
                            .map(char::from)
                            .map(String::from)
                            .map(|s| format_ident!("{}", s))
                            .collect::<Vec<_>>();

                        let mut field_names = TokenStream2::new();
                        let mut vec_inits = TokenStream2::new();
                        let mut vec_extends = TokenStream2::new();
                        for field in fields.unnamed.iter() {
                            let field_name = alphabet.remove(0);
                            field_names.extend(quote_spanned! { field.span() =>
                                #field_name,
                            });

                            match &field.ty {
                                Type::Path(type_path)
                                    if type_path.clone().into_token_stream().to_string()
                                        == "bool" =>
                                {
                                    vec_inits.extend(quote_spanned! {variant.span()=>
                                    match #field_name {true => "on", false => "off"}.into(),})
                                }
                                // check if type is a vec
                                Type::Path(type_path)
                                    if type_path
                                        .clone()
                                        .into_token_stream()
                                        .to_string()
                                        .starts_with("Vec<") =>
                                {
                                    vec_extends.extend(quote_spanned! {variant.span()=>
                                    vec.extend(#field_name.iter().map(serde_json::Value::from));})
                                }
                                Type::Path(type_path)
                                    if type_path
                                        .clone()
                                        .into_token_stream()
                                        .to_string()
                                        .starts_with("Vec <") =>
                                {
                                    vec_extends.extend(quote_spanned! {variant.span()=>
                                    vec.extend(#field_name.iter().map(serde_json::Value::from));})
                                }
                                _ => vec_inits.extend(quote_spanned! {variant.span()=>
                                #field_name.to_owned().into(),}),
                            }
                        }

                        if vec_extends.is_empty() {
                            variant_match_arms.extend(quote_spanned! {variant.span()=>
                                #name::#variant_name (#field_names) => {
                                    vec![#vec_inits]
                                },
                            });
                        } else {
                            variant_match_arms.extend(quote_spanned! {variant.span()=>
                                        #name::#variant_name (#field_names) => {
                                            let mut vec = vec![#vec_inits];
                                            #vec_extends
                                            vec
                                        },
                            });
                        }
                    }
                    Fields::Unit => {
                        variant_match_arms.extend(quote_spanned! {variant.span()=>
                                    #name::#variant_name => vec![],
                        });
                    }
                    Fields::Named(_) => {
                        todo!("Named fields");
                    }
                };
            }
        }
        _ => return derive_error!("get_params is only implemented for enums"),
    };

    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();

    let expanded = quote! {
        impl #impl_generics #name #ty_generics #where_clause {
            pub fn get_params(&self) -> Vec<serde_json::Value> {
            match self {
                #variant_match_arms
            }
        }
        }
    };

    TokenStream::from(expanded)
}

#[proc_macro_derive(IntoJsonValue)]
pub fn into_json_value_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_into_json_value_derive(&ast)
}

fn impl_into_json_value_derive(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let data = &ast.data;

    let mut variant_match_arms;

    match data {
        Data::Enum(data_enum) => {
            variant_match_arms = TokenStream2::new();

            for variant in &data_enum.variants {
                let variant_name = &variant.ident;

                // Variant can have unnamed fields like `Variant(i32, i64)`
                // Variant can have named fields like `Variant {x: i32, y: i32}`
                // Variant can be named Unit like `Variant`
                let fields_in_variant = match &variant.fields {
                    Fields::Unnamed(_) => quote_spanned! {variant.span()=> (..) },
                    Fields::Unit => quote_spanned! { variant.span()=> },
                    Fields::Named(_) => quote_spanned! {variant.span()=> {..} },
                };

                let variant_string = variant_name.to_string().to_case(Case::Snake).to_string();

                variant_match_arms.extend(quote_spanned! {variant.span()=>
                            #name::#variant_name #fields_in_variant => #variant_string.to_string().into(),
                });
            }
        }
        _ => return derive_error!("get_params is only implemented for enums"),
    };

    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();

    let expanded = quote! {
        impl From<#impl_generics #name #ty_generics> for serde_json::Value #where_clause {
            fn from(val: #name) -> Self {
                match val {
                    #variant_match_arms
                }
            }
        }
        impl From<#impl_generics &#name #ty_generics> for serde_json::Value #where_clause {
            fn from(val: &#name) -> Self {
                match *val {
                    #variant_match_arms
                }
            }
        }
    };

    TokenStream::from(expanded)
}
