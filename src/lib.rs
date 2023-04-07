extern crate proc_macro;

use crate::proc_macro::TokenStream;

use quote::quote;

use syn::punctuated::Punctuated;
use syn::{parse_macro_input, Token};

struct WidthAttr {
    width: syn::LitInt,
}

/// The width attribute should take the form #[width = 2]
impl syn::parse::Parse for WidthAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let _: syn::Token![=] = input.parse()?;
        let width: syn::LitInt = input.parse()?;
        Ok(Self { width })
    }
}

struct AltAttr {
    chr: syn::LitChar,
}

/// Alternate representation attributes look like #[alt(0b00, 0b11)]
impl syn::parse::Parse for AltAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let _: syn::Token![=] = input.parse()?;
        let chr: syn::LitChar = input.parse()?;
        Ok(Self { chr })
    }
}

#[proc_macro_derive(Codec, attributes(width, display, alt))]
pub fn codec_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::Item);

    // Test for correct usage
    let enum_ast = match input {
        syn::Item::Enum(e) => e,
        _ => {
            return syn::Error::new_spanned(input, "Codec can only be derived for enums")
                .to_compile_error()
                .into()
        }
    };

    let variants = enum_ast.variants;
    let enum_ident = enum_ast.ident;
    let mut max_variant = 0u8;
    let mut variant_idents = Vec::new();

    let mut variants_to_char = Vec::new();
    let mut chars_to_variant = Vec::new();
    let mut alt_discriminants = Vec::new();
    let mut unsafe_alts = Vec::new();

    for variant in variants.iter() {
        let ident = &variant.ident;
        variant_idents.push(ident.clone());
        let discriminant = &variant.discriminant;

        if let Some((_, syn::Expr::Lit(expr_lit))) = discriminant {
            let value = match &expr_lit.lit {
                syn::Lit::Byte(lit_byte) => lit_byte.value(),
                syn::Lit::Int(lit_int) => lit_int.base10_parse::<u8>().unwrap(),
                _ => {
                    return syn::Error::new_spanned(
                        ident,
                        "Codec derivations require byte or integer discriminants",
                    )
                    .to_compile_error()
                    .into();
                }
            };

            alt_discriminants.push(quote! { #value => Ok(Self::#ident) });
            unsafe_alts.push(quote! { #value => Self::#ident });

            max_variant = max_variant.max(value);
        } else {
            return syn::Error::new_spanned(ident, "Codec derivations require discriminants")
                .to_compile_error()
                .into();
        }

        let mut char_repr = ident.to_string().chars().next().unwrap();

        for attr in &variant.attrs {
            if attr.path.is_ident("display") {
                let alt_attr: AltAttr = match syn::parse2(attr.tokens.clone()) {
                    Ok(attr) => attr,
                    Err(err) => return err.to_compile_error().into(),
                };
                char_repr = alt_attr.chr.value();
            } else if attr.path.is_ident("alt") {
                let discs: Punctuated<syn::ExprLit, Token![,]> =
                    match attr.parse_args_with(Punctuated::parse_terminated) {
                        Ok(discs) => discs,
                        Err(err) => return err.to_compile_error().into(),
                    };

                for d in discs.into_iter() {
                    alt_discriminants.push(quote! { #d => Ok(Self::#ident) });
                    unsafe_alts.push(quote! { #d => Self::#ident });
                }
            };
        }

        variants_to_char.push(quote! { Self::#ident => #char_repr });
        chars_to_variant.push(quote! { #char_repr => Ok(Self::#ident) });
    }

    // TODO: we should be able to use the best fitting width if we know the max
    // value of the variants, ceil(log2(max_variant)). This should be carefully
    // tested.
    //
    //    let mut width = max_variant.ilog2();

    let mut width = 8; // default width
    for attr in &enum_ast.attrs {
        if attr.path.is_ident("width") {
            width = match syn::parse2::<WidthAttr>(attr.tokens.clone()) {
                Ok(w) => w.width.base10_parse::<u8>().unwrap(),
                Err(err) => return err.to_compile_error().into(),
            }
        };
    }

    if max_variant >= u8::pow(2, width as u32) {
        return syn::Error::new_spanned(
            enum_ident,
            "Codec specifier does not fit inside bit width",
        )
        .to_compile_error()
        .into();
    }

    // Generate the implementation
    let output = quote! {
        impl Codec for #enum_ident {
            type Error = ParseBioErr;
            const WIDTH: u8 = #width;
            fn unsafe_from_bits(b: u8) -> Self {
                match b {
                    #(#unsafe_alts),*,
                    _ => panic!(),
                }
            }

            fn try_from_bits(b: u8) -> Result<Self, ParseBioErr> {
                match b {
                    #(#alt_discriminants),*,
                    _ => Err(ParseBioErr),
                }
            }

            fn from_char(c: char) -> Result<Self, ParseBioErr> {
                match c {
                    #(#chars_to_variant),*,
                    _ => Err(ParseBioErr),
                }
            }

            fn to_char(self) -> char {
                match self {
                    #(#variants_to_char),*,
                }
            }
        }

        impl #enum_ident {
            pub fn iter() -> impl Iterator<Item = Self> {
                vec![ #(Self::#variant_idents,)* ].into_iter()
            }
        }
    };
    output.into()
}
