use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use std::{borrow::Borrow, collections::HashMap};
use syn::{parse_macro_input, DeriveInput};

#[derive(Clone)]
struct ParsedFields {
    types: Vec<syn::Type>,
    idents: Vec<proc_macro2::Ident>,
}

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let struct_ident = &input.ident;

    let builder_ident = create_builder_ident(struct_ident);

    let parsed_fields = parse_fields(&input);

    if let Err(msg) = parsed_fields {
        panic!("{msg}");
    }
    let parsed_fields = parsed_fields.ok().unwrap();

    let parsed_fields_cloned = parsed_fields.clone();

    let zipped_fields = parsed_fields_cloned
        .idents
        .into_iter()
        .zip(parsed_fields_cloned.types.into_iter());

    let map: HashMap<proc_macro2::Ident, syn::Type> = HashMap::from_iter(zipped_fields);

    let idents = &parsed_fields.idents;

    let types = &parsed_fields.types;

    let option_map: HashMap<proc_macro2::Ident, syn::Type> = map
        .borrow()
        .into_iter()
        .map(|(k, v)| (k, find_optional_type(v)))
        .filter(|(_, v)| v.is_some())
        .map(|(k, v)| (k.clone(), v.unwrap()))
        .collect();

    let option_idents: Vec<&proc_macro2::Ident> = option_map.borrow().keys().collect();

    let option_types: Vec<&syn::Type> = option_map.borrow().values().collect();
    let orig_without_option: HashMap<proc_macro2::Ident, syn::Type> = map
        .into_iter()
        .filter(|(i, _)| !option_map.contains_key(i))
        .collect();
    let orig_without_option_idents: Vec<&proc_macro2::Ident> =
        orig_without_option.borrow().keys().collect();

    let orig_without_option_types: Vec<&syn::Type> =
        orig_without_option.borrow().values().collect();
    expand(
        struct_ident,
        &builder_ident,
        idents,
        types,
        &orig_without_option_idents,
        &orig_without_option_types,
        &option_idents,
        &option_types,
    )
}

fn create_builder_ident(struct_ident: &Ident) -> Ident {
    Ident::new(&format!("{}Builder", struct_ident), Span::call_site())
}

fn expand(
    struct_ident: &Ident,
    builder_ident: &Ident,
    idents: &[Ident],
    types: &[syn::Type],
    orig_without_option_idents: &[&Ident],
    orig_without_option_types: &[&syn::Type],
    option_idents: &[&Ident],
    option_types: &[&syn::Type],
) -> TokenStream {
    let tokens = quote! {
        impl #struct_ident {

            pub fn builder() -> #builder_ident {
                 #builder_ident {
                    #(#idents  : Default::default()),*
                 }
            }
        }

        pub struct #builder_ident {
            #(#idents : #types),*
        }

        impl #builder_ident {
            #(pub fn #orig_without_option_idents(&mut self, #orig_without_option_idents: #orig_without_option_types) -> &mut #builder_ident {
                self.#orig_without_option_idents = #orig_without_option_idents;
                self
            })*

            #(pub fn #option_idents(&mut self, #option_idents: #option_types) -> &mut #builder_ident {
                self.#option_idents = Some(#option_idents);
                self
            })*

            pub fn build(&mut self) -> std::result::Result<#struct_ident, std::boxed::Box<dyn std::error::Error>> {
                Ok(#struct_ident {
                    #(#idents : self.#idents.clone()),*
                })
            }
        }
    };

    // Hand the output tokens back to the compiler.
    proc_macro::TokenStream::from(tokens)
}

fn find_attribute_of_field() -> () {}

fn find_optional_type(syn_type: &syn::Type) -> Option<syn::Type> {
    match syn_type {
        syn::Type::Path(type_path) => {
            let path = &type_path.path;
            let segments = &path.segments;
            if let Some(path_segment) = segments.first() {
                let ident = &path_segment.ident;
                if ident.to_string().contains("Option") {
                    eprintln!("ident: {ident}");
                    let path_arguments = path_segment.arguments.clone();
                    if let syn::PathArguments::AngleBracketed(anglebr_generic_args) = path_arguments
                    {
                        let args = anglebr_generic_args.args;
                        if let Some(arg) = args.first() {
                            if let syn::GenericArgument::Type(type_var) = arg {
                                return Some(type_var.to_owned());
                            }
                        }
                    }
                }
                return None;
            }
            None
        }
        _ => None,
    }
}

fn parse_fields(derive_input: &DeriveInput) -> Result<ParsedFields, &'static str> {
    pub use syn::Data::*;
    if let Struct(stru) = &derive_input.data {
        pub use syn::Fields::*;
        let fields = &stru.fields;
        if let Named(named_fields) = fields {
            let iter_ty = named_fields.named.iter();
            let iter_ident = iter_ty.clone();
            let vec_ty: Vec<syn::Type> = iter_ty.map(|f| f.ty.clone()).collect();
            let vec_ident: Vec<proc_macro2::Ident> =
                iter_ident.map(|f| f.ident.to_owned().unwrap()).collect();
            if vec_ident.len() != vec_ty.len() {
                return Err("Mismatched parsed length of identifiers,types");
            }
            return Ok(ParsedFields {
                types: vec_ty,
                idents: vec_ident,
            });
        }
        Err("No named fields")
    } else {
        Err("Data type is not a struct")
    }
}
