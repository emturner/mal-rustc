use crate::types::*;
use proc_macro2::TokenStream;
use quote::quote;
use std::collections::HashMap;

pub fn create_core_env<'a>() -> HashMap<&'a str, MalFuncCallTemplate<'a>> {
    let mut env = HashMap::new();
    env.insert(
        "+",
        MalFuncCallTemplate {
            name: "mal_builtin_plus",
            num_args: MalArgCount::Many,
        },
    );
    env.insert(
        "-",
        MalFuncCallTemplate {
            name: "mal_builtin_sub",
            num_args: MalArgCount::Many,
        },
    );
    env.insert(
        "*",
        MalFuncCallTemplate {
            name: "mal_builtin_mul",
            num_args: MalArgCount::Many,
        },
    );
    env.insert(
        "/",
        MalFuncCallTemplate {
            name: "mal_builtin_div",
            num_args: MalArgCount::Many,
        },
    );
    env
}

#[allow(clippy::implicit_hasher)]
pub fn lower(ast: &MalAtomComp, env: &HashMap<&str, MalFuncCallTemplate>) -> TokenStream {
    match ast {
        MalAtomComp::SExp(ref s) if !s.is_empty() => lower_sexp(s, env),
        MalAtomComp::Vector(ref v) => lower_vector(v, env),
        MalAtomComp::HashMap(ref h) => lower_hashmap(h, env),
        ast => quote!(#ast),
    }
}

fn lower_hashmap<'a>(
    h: &HashMap<String, MalAtomComp<'a>>,
    env: &HashMap<&str, MalFuncCallTemplate>,
) -> TokenStream {
    let mut hm_tokens = quote!(let mut hm = std::collections::HashMap::new(););

    for (k, v) in h.iter() {
        let v = lower(v, env);
        hm_tokens.extend(quote!(hm.insert(#k.into(), #v);));
    }

    hm_tokens.extend(quote!(hm));

    quote!(MalAtom::HashMap({#hm_tokens}))
}

fn lower_vector(v: &[MalAtomComp], env: &HashMap<&str, MalFuncCallTemplate>) -> TokenStream {
    let elements = v.iter().map(|e| lower(e, env));
    quote!(MalAtom::Vector(vec![#(#elements),*]))
}

fn lower_sexp(sexp: &[MalAtomComp], env: &HashMap<&str, MalFuncCallTemplate>) -> TokenStream {
    if let MalAtomComp::Symbol(s) = sexp[0] {
        match env.get(s) {
            Some(func) => lower_mal_func_call_template(func, &sexp[1..], env),
            None => {
                let err = MalResultComp::Err(format!("Function '{}' not defined", s));
                quote!(#err)
            }
        }
    } else {
        let err = MalResultComp::Err("Expected a function".into());
        quote!(#err)
    }
}

fn lower_mal_func_call_template(
    func: &MalFuncCallTemplate,
    args: &[MalAtomComp],
    env: &HashMap<&str, MalFuncCallTemplate>,
) -> TokenStream {
    let args = args.iter().map(|atom| lower(atom, env)).collect();
    let call = MalFuncCall::new(func, args);
    quote!(#call)
}