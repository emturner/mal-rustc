use proc_macro2::TokenStream;
use quote::{quote, ToTokens};

pub mod runtime;

pub use runtime::{MalAtom};

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
            MalAtom::HashMap(h) if h.is_empty() => {
                tokens.extend(quote!(MalAtom::HashMap(std::collections::HashMap::new())))
            }
            MalAtom::HashMap(h) => {
                let mut hm_tokens = quote!(let mut hm = std::collections::HashMap::new(););

                for (k, v) in h.iter() {
                    hm_tokens.extend(quote!(hm.insert(#k.into(), #v);));
                }

                hm_tokens.extend(quote!(hm));

                tokens.extend(quote!(MalAtom::HashMap({#hm_tokens})))
            }
        }
    }
}

pub enum MalResultComp<'a> {
    Ok(MalAtom<'a>),
    Err(String)
}

impl<'a> ToTokens for MalResultComp<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            MalResultComp::Ok(mal_atom) => tokens.extend(quote!(Ok(#mal_atom)?)),
            MalResultComp::Err(mal_err) => tokens.extend(quote!(Err(#mal_err)?))
        }
    }
}

pub struct MalFuncCallTemplate<'a> {
    name: &'a str,
    num_args: MalArgCount
}

pub enum MalArgCount {
    Many,
    Known(u32),
}

pub struct MalFuncCall<'a> {
    template: &'a MalFuncCallTemplate<'a>,
    args: &'a[MalAtom<'a>]
}

impl<'a> MalFuncCall<'a> {
    pub fn new(func: &'a MalFuncCallTemplate<'a>, args: &'a[MalAtom<'a>]) -> MalFuncCall<'a> {
        Self {
            template: func,
            args: args
        }
    }
}

impl<'a> ToTokens for MalFuncCall<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self.template.num_args {
            MalArgCount::Many => {
                let name = self.template.name;
                let args = self.args;
                tokens.extend(quote!(#name(#(#args),*)))
            },
            _ => unimplemented!()
        }
    }
}