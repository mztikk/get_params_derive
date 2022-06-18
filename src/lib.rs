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

                            // if let Type::Path(type_path) = &field.ty {
                            //     println!("{}", type_path.into_token_stream());
                            // }

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
                                // check if type is an option
                                Type::Path(type_path)
                                    if type_path
                                        .clone()
                                        .into_token_stream()
                                        .to_string()
                                        .starts_with("Option <") =>
                                {
                                    vec_extends.extend(quote_spanned! {variant.span()=>
                                        if let Some(val) = #field_name {
                                            vec.push(val.to_owned().into());
                                        }
                                    });
                                }
                                Type::Path(type_path)
                                    if type_path
                                        .clone()
                                        .into_token_stream()
                                        .to_string()
                                        .starts_with("Option<") =>
                                {
                                    vec_extends.extend(quote_spanned! {variant.span()=>
                                        if let Some(val) = #field_name {
                                            vec.push(val.to_owned().into());
                                        }
                                    });
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
            pub(crate) fn get_params(&self) -> Vec<serde_json::Value> {
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

#[proc_macro_derive(FromRawCommand)]
pub fn from_raw_command_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_from_raw_command_derive(&ast)
}

fn impl_from_raw_command_derive(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let data = &ast.data;

    let mut method_construction;

    match data {
        Data::Enum(data_enum) => {
            method_construction = TokenStream2::new();

            for variant in &data_enum.variants {
                let variant_name = &variant.ident;
                let variant_snake_case = variant_name.to_string().to_case(Case::Snake).to_string();

                let mut param_construction = TokenStream2::new();

                match &variant.fields {
                    Fields::Unnamed(fields) => {
                        for (i, field) in fields.unnamed.iter().enumerate() {
                            match &field.ty {
                                Type::Path(type_path)
                                    if type_path.clone().into_token_stream().to_string()
                                        == "bool" =>
                                {
                                    param_construction.extend(quote_spanned! {variant.span()=>
                                        if raw.params.len() > #i { match raw.params[#i].as_str().unwrap() { "on" => true, "off" => false, _ => false } } else { panic!("Value for non optional field '{} - {}' in '{}' is missing", #i+1, stringify!(#field), stringify!(#variant_name)) },
                                    });
                                }
                                Type::Path(type_path)
                                    if type_path
                                        .clone()
                                        .into_token_stream()
                                        .to_string()
                                        .starts_with("Option <") =>
                                {
                                    param_construction.extend(quote_spanned! {variant.span()=>
                                                if raw.params.len() > #i { Some(serde_json::from_value(raw.params[#i].to_owned()).unwrap()) } else { None },
                                            });
                                }
                                _ => {
                                    param_construction.extend(quote_spanned! {variant.span()=>
                                        if raw.params.len() > #i { serde_json::from_value(raw.params[#i].to_owned()).unwrap() } else { panic!("Value for non optional field '{} - {}' in '{}' is missing", #i+1, stringify!(#field), stringify!(#variant_name)) },
                                    });
                                }
                            }
                        }
                    }
                    Fields::Unit => {}
                    _ => todo!(),
                };

                if param_construction.is_empty() {
                    method_construction.extend(quote_spanned! {variant.span()=>
                        #variant_snake_case => #name::#variant_name,
                    });
                } else {
                    method_construction.extend(quote_spanned! {variant.span()=>
                        #variant_snake_case => #name::#variant_name(#param_construction),
                    });
                }
            }
        }
        _ => return derive_error!("FromRawCommand is only implemented for enums"),
    };

    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();

    let expanded = quote! {
        impl From<RawCommand> for #impl_generics #name #ty_generics #where_clause {
            fn from(raw: RawCommand) -> Self {
                match raw.method.as_str() {
                    #method_construction
                    _ => panic!("Unknown method"),
                }
            }
        }
        impl From<&RawCommand> for #impl_generics #name #ty_generics #where_clause {
            fn from(raw: &RawCommand) -> Self {
                match raw.method.as_str() {
                    #method_construction
                    _ => panic!("Unknown method"),
                }
            }
        }
    };

    TokenStream::from(expanded)
}
