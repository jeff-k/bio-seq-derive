extern crate proc_macro;

use crate::proc_macro::TokenStream;

use quote::quote;

use syn::punctuated::Punctuated;
use syn::{parse_macro_input, Token};

struct WidthAttr {
    width: syn::LitInt,
}

impl syn::parse::Parse for WidthAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let _: syn::Token![=] = input.parse()?;
        let width: syn::LitInt = input.parse()?;
        Ok(Self { width })
    }
}

fn codec_width(attrs: &[syn::Attribute]) -> u8 {
    let mut width: u8 = 4;
    for attr in attrs {
        if attr.path.is_ident("width") {
            width = match syn::parse2::<WidthAttr>(attr.tokens.clone()) {
                Ok(w) => w.width.base10_parse::<u8>().unwrap(),
                _ => 4,
            };
        };
    }
    width
}

struct AltAttr {
    chr: syn::LitChar,
}

impl syn::parse::Parse for AltAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let _: syn::Token![=] = input.parse()?;
        let chr: syn::LitChar = input.parse()?;
        Ok(Self { chr })
    }
}

/// Parse alternative representations
fn codec_altrepr(attrs: &[syn::Attribute]) -> Option<char> {
    for attr in attrs {
        if attr.path.is_ident("display") {
            return match syn::parse2::<AltAttr>(attr.tokens.clone()) {
                Ok(c) => Some(c.chr.value()),
                _ => panic!("expecting char in 'display' attribute"),
            };
        };
    }
    None
}

/// Parse alternative discriminants
fn codec_altdisc(attrs: &[syn::Attribute]) -> Option<Vec<syn::ExprLit>> {
    for attr in attrs {
        if attr.path.is_ident("alt") {
            let discs: Punctuated<syn::ExprLit, Token![,]> =
                attr.parse_args_with(Punctuated::parse_terminated).unwrap();
            return Some(discs.iter().cloned().collect());
        };
    }
    None
}

#[proc_macro_derive(Codec, attributes(width, display, alt))]
pub fn codec_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::Item);

    // Basic test for correct usage
    let e = match input {
        syn::Item::Enum(e) => e,
        _ => {
            return syn::Error::new_spanned(input, "Codec can only be derived for enums")
                .to_compile_error()
                .into()
        }
    };

    let variants = e.variants;
    let ident = e.ident;
    let width: u8 = codec_width(&e.attrs);

    let mut max_variant = 0u64;

    let mut variant_idents = vec![];

    // Test that the maximum discriminant is less than the greatest amount
    // supported by the bitwidth and collect the list of identifiers.
    for v in variants.iter() {
        //let ident = &v.ident;
        variant_idents.push(v.ident.clone());
        let discriminant = &v.discriminant;

        if let Some((_, syn::Expr::Lit(expr_lit))) = discriminant {
            let value = match &expr_lit.lit {
                syn::Lit::Byte(lit_byte) => lit_byte.value() as u64,
                syn::Lit::Int(lit_int) => lit_int.base10_parse::<u64>().unwrap(),
                _ => {
                    return syn::Error::new_spanned(
                        &v.ident,
                        "Codec derivations require byte or integer discriminants",
                    )
                    .to_compile_error()
                    .into();
                }
            };

            max_variant = max_variant.max(value);
        } else {
            return syn::Error::new_spanned(&v.ident, "Codec derivations require discriminants")
                .to_compile_error()
                .into();
        }
    }

    if max_variant >= u64::pow(2, width as u32) {
        return syn::Error::new_spanned(ident, "Codec specifier does not fit inside bit width")
            .to_compile_error()
            .into();
    }

    // Collect the mappings of variants to character representations
    let variants_to_char = variants.iter().map(|v| {
        let ident = &v.ident;
        let discriminant = &v.discriminant;

        let alt_char = codec_altrepr(&v.attrs);

        match alt_char {
            Some(alt) => quote! { Self::#ident => #alt },
            None => match discriminant {
                Some(_d) => {
                    let char_repr: char = ident.to_string().chars().next().unwrap();
                    quote! { Self::#ident => #char_repr }
                }
                None => {
                    syn::Error::new_spanned(ident, "Could not parse alternative representation")
                        .to_compile_error()
                }
            },
        }
    });

    // Collect the mappings of character representations to variants
    let chars_to_variant = variants.iter().map(|v| {
        let ident = &v.ident;
        let discriminant = &v.discriminant;

        let alt_char = codec_altrepr(&v.attrs);

        match alt_char {
            Some(alt) => quote! { #alt => Ok(Self::#ident) },
            None => match discriminant {
                Some(_d) => {
                    let char_repr: char = ident.to_string().chars().next().unwrap();
                    quote! { #char_repr => Ok(Self::#ident) }
                }
                None => {
                    syn::Error::new_spanned(ident, "Could not parse alternative representation")
                        .to_compile_error()
                }
            },
        }
    });

    // Handle alternative discriminants. This is for cases like the 6-bit
    // Amino acid alphabet, where for convenience we can have multiple
    // bit representations to construct a variant.
    let mut altds = Vec::new();
    let mut altds_unsafe = Vec::new();

    for v in &variants {
        let ident = &v.ident;

        let discriminant = &v.discriminant;

        match discriminant {
            Some((_, d)) => {
                altds.push(quote! { #d => Ok(Self::#ident) });
                altds_unsafe.push(quote! { #d => Self::#ident });
            }
            None => panic!(),
        }

        let alt_disc = codec_altdisc(&v.attrs);
        if let Some(ds) = alt_disc {
            for d in ds {
                altds.push(quote! { #d => Ok(Self::#ident) });
                altds_unsafe.push(quote! { #d => Self::#ident });
            }
        }
    }

    // Generate the implementation
    let output = quote! {
        impl Codec for #ident {
            type Error = ParseBioErr;
            const WIDTH: u8 = #width;
            fn unsafe_from_bits(b: u8) -> Self {
                match b {
                    #(#altds_unsafe),*,
                    _ => panic!(),
                }
            }

            fn try_from_bits(b: u8) -> Result<Self, ParseBioErr> {
                match b {
                    #(#altds),*,
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

        impl #ident {
            pub fn iter() -> impl Iterator<Item = Self> {
                vec![ #(Self::#variant_idents,)* ].into_iter()
            }
        }
    };
    output.into()
}
