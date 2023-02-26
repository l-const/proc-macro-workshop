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
    let mut map_with_option_types = map.clone();
    let orig_without_option: HashMap<proc_macro2::Ident, syn::Type> = map
        .into_iter()
        .filter(|(i, t)| !option_map.contains_key(i))
        .collect();
    let orig_without_option_idents: Vec<&proc_macro2::Ident> =
        orig_without_option.borrow().keys().collect();

    let orig_without_option_types: Vec<&syn::Type> =
        orig_without_option.borrow().values().collect();
    map_with_option_types.extend(option_map.clone().into_iter());
    // Needed to correct the order of the fields to match according to how they were defined in the original struct definition
    let complete_pairs: Vec<(&proc_macro2::Ident, &syn::Type)> = idents
        .into_iter()
        .map(|id| (id, map_with_option_types.get(id).unwrap()))
        .collect();
    let complete_idents: Vec<&proc_macro2::Ident> =
        complete_pairs.iter().map(|(id, _)| *id).collect();
    let complete_types: Vec<&syn::Type> = complete_pairs.iter().map(|(_, ty)| *ty).collect();

    // Create modified TokenStream
    let tokens = quote! {
        impl #struct_ident {

            pub fn builder() -> #builder_ident {
                 #builder_ident {
                    #(#complete_idents  : Default::default()),*
                 }
            }
        }

        pub struct #builder_ident {
            #(#complete_idents : #types),*
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

fn create_builder_ident(struct_ident: &Ident) -> Ident {
    Ident::new(&format!("{}Builder", struct_ident), Span::call_site())
}

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

// fn find_optional_types<'a>(parsed_types: &'a ParsedFields) -> Option<ParsedFields> {
//     let optional_parsed_types = parsed_types.types.iter().filter(|&&t| t.to_string().starts_with("Option<")).collect();
//     if optional_parsed_types.types.len() > 0 {
//         return Some(optional_parsed_types);
//     }
//     None
// }

// impl ParsedFields<'_> {
//     fn iter(& self) -> impl Iterator<Item = (&& syn::Type, && proc_macro2::Ident)> {
//       self.types.iter().zip(self.idents.iter())
//     }
// }

// impl <'parse> IntoIterator for ParsedFields<'parse> {
//     type Item = (&'parse syn::Type, &'parse proc_macro2::Ident);
//     type IntoIter = std::iter::Zip<std::vec::IntoIter<&'parse syn::Type>,std::vec::IntoIter<&'parse proc_macro2::Ident>>;
//     fn into_iter(self) -> Self::IntoIter {
//         self.types.into_iter().zip(self.idents.into_iter())
//     }
// }

// impl <'a> FromIterator<(&'a syn::Type, &'a proc_macro2::Ident)> for ParsedFields<'a> {
//     fn from_iter<T: IntoIterator<Item = (&'a syn::Type, &'a proc_macro2::Ident)>>(iter: T) -> Self {
//         let idents: Vec<_> = iter.into_iter().map(|(t, i)| i).collect();
//         let types: Vec<_> = iter.into_iter().map(|(t, i)| t).collect();

//         ParsedFields { idents,  types}
//     }
// }
