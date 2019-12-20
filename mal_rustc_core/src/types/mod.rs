use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use std::collections::HashMap;

pub mod runtime;

#[derive(Debug, PartialEq, Eq)]
pub enum MalAtomComp<'a> {
    Nil,
    Bool(bool),
    Special(&'a str),
    // ideally this would be &'a str, but we need to escape the control characters
    // so need to allocate a new string
    String(String),
    Int(i64),
    Symbol(&'a str),
    SExp(Vec<MalAtomComp<'a>>),
    Vector(Vec<MalAtomComp<'a>>),
    Keyword(&'a str),
    HashMap(HashMap<String, MalAtomComp<'a>>),
}
impl<'a> ToTokens for MalAtomComp<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let comp = match self {
            MalAtomComp::Nil => quote!(MalAtom::Nil),
            MalAtomComp::Bool(b) => quote!(MalAtom::Bool(#b)),
            MalAtomComp::Special(s) => quote!(MalAtom::Special(#s.to_string())),
            MalAtomComp::String(s) => quote!(MalAtom::String(#s.into())),
            MalAtomComp::Int(i) => quote!(MalAtom::Int(#i)),
            MalAtomComp::Symbol(s) => quote!(MalAtom::Symbol(#s.into())),
            MalAtomComp::SExp(sexp) => quote!(MalAtom::SExp(vec![#(#sexp),*])),
            MalAtomComp::Vector(v) => quote!(MalAtom::Vector(vec![#(#v),*])),
            MalAtomComp::Keyword(k) => quote!(MalAtom::Keyword(#k.to_string())),
            MalAtomComp::HashMap(h) if h.is_empty() => {
                (quote!(MalAtom::HashMap(std::collections::HashMap::new())))
            }
            MalAtomComp::HashMap(h) => {
                let mut hm_tokens = quote!(let mut hm = std::collections::HashMap::new(););

                for (k, v) in h.iter() {
                    hm_tokens.extend(quote!(hm.insert(#k.into(), #v);));
                }

                hm_tokens.extend(quote!(hm));

                quote!(MalAtom::HashMap({#hm_tokens}))
            }
        };

        tokens.extend(quote!(#comp))
    }
}

pub enum MalResultComp<'a> {
    Ok(MalAtomComp<'a>),
    Err(String),
}

impl<'a> ToTokens for MalResultComp<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            MalResultComp::Ok(mal_atom) => tokens.extend(quote!(#mal_atom)),
            MalResultComp::Err(mal_err) => tokens.extend(quote!(Err(#mal_err)?)),
        }
    }
}

pub struct MalFuncCallTemplate<'a> {
    pub name: &'a str,
    pub num_args: MalArgCount,
}

pub enum MalArgCount {
    Many,
    Known(u32),
}

pub struct MalFuncCall<'a> {
    template: &'a MalFuncCallTemplate<'a>,
    args: Vec<TokenStream>,
}

impl<'a> MalFuncCall<'a> {
    pub fn new(func: &'a MalFuncCallTemplate<'a>, args: Vec<TokenStream>) -> MalFuncCall<'a> {
        Self {
            template: func,
            args,
        }
    }
}

impl<'a> ToTokens for MalFuncCall<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self.template.num_args {
            MalArgCount::Many => {
                let name = format_ident!("{}", self.template.name);
                let args = &self.args;
                tokens.extend(quote!(#name(vec![#(&#args),*])?))
            }
            _ => unimplemented!(),
        }
    }
}
