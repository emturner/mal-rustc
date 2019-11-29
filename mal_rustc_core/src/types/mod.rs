use proc_macro2::TokenStream;
use quote::{quote, ToTokens};

pub mod runtime;

use runtime::MalAtom;

impl<'a> ToTokens for MalAtom<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            MalAtom::Nil => tokens.extend(quote!(MalAtom::Nil)),
            MalAtom::Bool(b) => tokens.extend(quote!(MalAtom::Bool(#b))),
            MalAtom::Special(s) => tokens.extend(quote!(MalAtom::Special(#s))),
            MalAtom::String(s) => tokens.extend(quote!(MalAtom::String(#s.into()))),
            MalAtom::Int(i) => tokens.extend(quote!(MalAtom::Int(#i))),
            MalAtom::Symbol(s) => tokens.extend(quote!(MalAtom::Symbol(#s))),
            MalAtom::SExp(sexp) => tokens.extend(quote!(MalAtom::SExp(vec![#(#sexp),*]))),
            MalAtom::Vector(v) => tokens.extend(quote!(MalAtom::Vector(vec![#(#v),*]))),
            MalAtom::Keyword(k) => tokens.extend(quote!(MalAtom::Keyword(#k))),
            MalAtom::HashMap(h) if h.len() == 0 => {
                tokens.extend(quote!(MalAtom::HashMap(std::collections::HashMap::new())))
            }
            MalAtom::HashMap(h) => {
                let mut hm_tokens = quote!(let mut hm = std::collections::HashMap::new(););

                for (k, v) in h.into_iter() {
                    hm_tokens.extend(quote!(hm.insert(#k.into(), #v);));
                }

                hm_tokens.extend(quote!(hm));

                tokens.extend(quote!(MalAtom::HashMap({#hm_tokens})))
            }
        }
    }
}