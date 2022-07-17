use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{
    parenthesized,
    parse::{Parse, ParseStream},
    parse2, parse_macro_input, parse_str,
    punctuated::Punctuated,
    token::Paren,
    Expr, Field, FnArg, Ident, ItemStruct, ItemTrait, LitInt, Pat, Token, TraitItem,
    TraitItemMethod,
};

struct OpCodeInput {
    macro_name: Ident,
    _comma: Token![,],
    instrs: Punctuated<Instr, Token![;]>,
}

struct Instr {
    mne: Ident,
    op: Option<Punctuated<Expr, Token![,]>>,
}

impl Parse for OpCodeInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(OpCodeInput {
            macro_name: input.parse()?,
            _comma: input.parse()?,
            instrs: input.parse_terminated(Instr::parse)?,
        })
    }
}

impl Parse for Instr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Instr {
            mne: input.parse()?,
            op: Punctuated::parse_separated_nonempty(input).ok(),
        })
    }
}

#[test]
fn test_parse_opcode() {
    let _instr: Instr = syn::parse_quote! { brk };
    let _instr: Instr = syn::parse_quote! { push a };
    let _instr: Instr = syn::parse_quote! { and a, imm };

    let _instrs: Punctuated<Instr, syn::token::Semi> = syn::parse_quote! {
        brk;
        push a;
        and a, imm;
    };
}

#[proc_macro]
pub fn opcodes(input: TokenStream) -> TokenStream {
    let OpCodeInput {
        macro_name, instrs, ..
    } = parse_macro_input!(input as OpCodeInput);

    let mut ix = vec![];
    let mut asm = vec![];

    for i in 0..instrs.len() {
        ix.push(LitInt::new(&i.to_string(), Span::call_site()));
        let Instr { mne, op } = &instrs[i];
        asm.push(quote! { #mne #op});
    }

    quote! {
        macro_rules! #macro_name {
            ($opcode:expr, $m:ident) => {
                match $opcode {
                    #(
                        #ix => $m!(#asm),
                    )*
                }
            }
        }
    }
    .into()
}

fn impl_delegate_macro_name(trait_name: &Ident) -> Ident {
    parse_str(&format!("impl_delegate_trait_{}", trait_name)).unwrap()
}

#[proc_macro_attribute]
pub fn delegate(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let item = parse_macro_input!(item as ItemTrait);

    let trait_name = &item.ident;
    let macro_name: Ident = impl_delegate_macro_name(trait_name);

    let mut methods = vec![];

    for item in item.items.iter() {
        let TraitItemMethod { attrs, sig, .. } = if let TraitItem::Method(method) = item {
            method
        } else {
            panic!("delegate trait can only contain methods");
        };

        let method_name = &sig.ident;
        let args = sig.inputs.iter().filter_map(|arg| {
            if let FnArg::Typed(arg) = arg {
                if let Pat::Ident(ident) = arg.pat.as_ref() {
                    Some(ident.ident.clone())
                } else {
                    None
                }
            } else {
                None
            }
        });

        methods.push(quote! {
            #(#attrs)*
            #sig {
                self.inner.#method_name(#(#args),*)
            }
        });
    }

    let ret = quote! {
         #item

         macro_rules! #macro_name {
            ($ty:ty) => {
                impl #trait_name for $ty {
                    #(#methods)*
                }
            }
         }
    };

    // eprintln!("{}", ret.to_string());

    ret.into()
}

#[proc_macro_attribute]
pub fn context(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let ItemStruct {
        attrs,
        vis,
        ident,
        generics,
        fields,
        ..
    } = parse_macro_input!(item as ItemStruct);

    let mut types = vec![];
    let mut ctor = quote! {};
    let mut ctor_args = vec![];

    gen_types(
        &mut types,
        &mut ctor,
        &mut ctor_args,
        &mut vec![],
        quote! { #(#attrs)* #vis struct #ident #generics },
        &ident,
        &fields.iter().collect::<Vec<_>>(),
    );

    types.reverse();

    let ret = quote! {
        #(#types)*

        impl #ident {
            fn new(#(#ctor_args),*) -> Self {
                #ctor
            }
        }
    };

    // eprintln!("{}", ret.to_string());

    ret.into()
}

struct SplitArg {
    _paren: Paren,
    inner_type: Ident,
    _colon: Token![:],
    trait_bound: Punctuated<Ident, Token![+]>,
}

impl Parse for SplitArg {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        Ok(SplitArg {
            _paren: parenthesized!(content in input),
            inner_type: content.parse()?,
            _colon: content.parse()?,
            trait_bound: content.parse_terminated(Ident::parse)?,
        })
    }
}

fn gen_types(
    code: &mut Vec<proc_macro2::TokenStream>,
    ctor: &mut proc_macro2::TokenStream,
    ctor_args: &mut Vec<proc_macro2::TokenStream>,
    impl_traits: &mut Vec<Ident>,
    sig: proc_macro2::TokenStream,
    ty: &Ident,
    fields: &[&Field],
) {
    let mut cur_fields = vec![];
    let mut field_vals = vec![];
    let mut has_inner = false;

    for i in 0..fields.len() {
        let field = fields[i];

        let (split, rest_attr): (Vec<_>, Vec<_>) = field
            .attrs
            .iter()
            .cloned()
            .partition(|attr| attr.path.is_ident("split"));

        if i == 0 || split.is_empty() {
            let field = Field {
                attrs: rest_attr,
                ..field.clone()
            };

            cur_fields.push(quote! { #field, });
            ctor_args.push(quote! { #field });

            let val = field.ident;
            field_vals.push(quote! { #val, });

            continue;
        }

        assert_eq!(split.len(), 1);
        assert!(rest_attr.is_empty());

        let split = split[0].tokens.clone();
        let arg = parse2::<SplitArg>(split).unwrap();
        let inner_type = &arg.inner_type;

        gen_types(
            code,
            ctor,
            ctor_args,
            impl_traits,
            quote! { struct #inner_type },
            inner_type,
            &fields[i..],
        );

        cur_fields.push(quote! { #(#rest_attr)* inner: #inner_type, });
        impl_traits.append(&mut arg.trait_bound.into_iter().collect());
        has_inner = true;
        break;
    }

    for impl_trait in impl_traits.iter() {
        let impl_macro = impl_delegate_macro_name(impl_trait);
        code.push(quote! {
            #impl_macro!(#ty);
        });
    }

    code.push(quote! {
        #sig {
            #(#cur_fields)*
        }
    });

    if has_inner {
        field_vals.push(quote! { inner: #ctor });
    }

    *ctor = quote! {
        #ty {
            #(#field_vals)*
        }
    };
}
