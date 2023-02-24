use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};

use quote::quote;
use syn::{
    parse_macro_input,
    token::{Enum, Struct},
    DeriveInput, Fields,
};

struct ParsedFields<'parse> {
    types: Vec<&'parse syn::Type>,
    idents: Vec<&'parse proc_macro2::Ident>,
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

    // Create modified TokenStream
    let tokens = quote! {
        impl #struct_ident {

            pub fn builder() -> #builder_ident {
                 #builder_ident {}
            }
        }

        pub struct #builder_ident;

        impl #builder_ident {

        }
    };

    // Hand the output tokens back to the compiler.
    proc_macro::TokenStream::from(tokens)
}

fn create_builder_ident(struct_ident: &Ident) -> Ident {
    Ident::new(&format!("{}Builder", struct_ident), Span::call_site())
}

fn parse_fields(derive_input: &DeriveInput) -> Result<ParsedFields<'_>, &'static str> {
    pub use syn::Data::*;
    if let Struct(stru) = &derive_input.data {
        pub use syn::Fields::*;
        let fields = &stru.fields;
        if let Named(namedFields) = fields {
            let iter_ty = namedFields.named.iter();
            let iter_ident = iter_ty.clone();
            let vec_ty: Vec<&syn::Type> = iter_ty.map(|f| &f.ty).collect();
            let vec_ident: Vec<&proc_macro2::Ident> =
                iter_ident.map(|f| f.ident.as_ref().unwrap()).collect();
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
