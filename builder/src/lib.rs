use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

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

    let parsed_fields = parsed_fields.ok().unwrap();
    let idents = parsed_fields.idents;
    let types = parsed_fields.types;
    // Create modified TokenStream
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
            #(pub fn #idents(&mut self, #idents: #types) -> &mut #builder_ident {
                self.#idents = #idents;
                self
            })*

            pub fn build(&mut self) -> std::result::Result<#struct_ident, Box<dyn std::error::Error>> {
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

fn find_optional_types(parse_types: &[Ident]) -> Option<&[Ident]> {
    unimplemented!()
}

fn parse_fields(derive_input: &DeriveInput) -> Result<ParsedFields<'_>, &'static str> {
    pub use syn::Data::*;
    if let Struct(stru) = &derive_input.data {
        pub use syn::Fields::*;
        let fields = &stru.fields;
        if let Named(named_fields) = fields {
            let iter_ty = named_fields.named.iter();
            let iter_ident = iter_ty.clone();
            let vec_ty: Vec<&syn::Type> = iter_ty.map(|f| &f.ty).collect();
            let vec_ident: Vec<&proc_macro2::Ident> =
                iter_ident.map(|f| f.ident.as_ref().unwrap()).collect();
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
