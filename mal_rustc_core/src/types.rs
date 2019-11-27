use proc_macro2::{TokenTree, Spacing, Span, Punct, TokenStream};
use quote::{TokenStreamExt, ToTokens, quote};

use std::fmt::{self, Display};

#[derive(Debug, PartialEq, Eq)]
pub enum MalAtom<'a> {
    Nil,
    Bool(bool),
    Special(&'a str),
    // ideally this would be &'a str, but we need to escape the control characters
    // so need to allocate a new string
    String(String),
    Int(i64),
    Symbol(&'a str),
    SExp(Vec<MalAtom<'a>>),
}

impl<'a> ToTokens for MalAtom<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            MalAtom::Nil => tokens.append_all(quote!(MalAtom::Nil)),
            MalAtom::Bool(b) => tokens.append_all(quote!(MalAtom::Bool(#b))),
            MalAtom::Special(s) => tokens.append_all(quote!(MalAtom::Special(#s))),
            MalAtom::String(s) => tokens.append_all(quote!(MalAtom::String(#s))),
            MalAtom::Int(i) => tokens.append_all(quote!(MalAtom::Int(#i))),
            MalAtom::Symbol(s) => tokens.append_all(quote!(MalAtom::Symbol(#s))),
            MalAtom::SExp(sexp) => tokens.append_all(quote!(MalAtom::SExp(vec![])))
        }
    }
}

impl<'a> Display for MalAtom<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MalAtom::Nil => write!(f, "nil"),
            MalAtom::Bool(b) => b.fmt(f),
            MalAtom::Special(s) => s.fmt(f),
            MalAtom::String(s) => s.fmt(f),
            MalAtom::Int(i) => i.fmt(f),
            MalAtom::Symbol(s) => s.fmt(f),
            MalAtom::SExp(sexp) => write!(f, "sexp")
        }
    }
}