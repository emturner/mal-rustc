use crate::types::*;
use env::Env;
use env::MalAtomCompRef;
use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote;
use std::collections::HashMap;

pub mod env;

pub fn create_core_env<'a>() -> Env<'a> {
    let mut env = Env::new(None);
    env.set(
        "+",
        MalAtomCompRef::Func(MalFuncCallTemplate {
            name: "mal_builtin_plus".into(),
            num_args: MalArgCount::Many,
        }),
    );
    env.set(
        "-",
        MalAtomCompRef::Func(MalFuncCallTemplate {
            name: "mal_builtin_sub".into(),
            num_args: MalArgCount::Many,
        }),
    );
    env.set(
        "*",
        MalAtomCompRef::Func(MalFuncCallTemplate {
            name: "mal_builtin_mul".into(),
            num_args: MalArgCount::Many,
        }),
    );
    env.set(
        "/",
        MalAtomCompRef::Func(MalFuncCallTemplate {
            name: "mal_builtin_div".into(),
            num_args: MalArgCount::Many,
        }),
    );
    env
}

#[allow(clippy::implicit_hasher)]
pub fn lower(ast: &MalAtomComp, env: &Env, assign_to: u32) -> TokenStream {
    match ast {
        MalAtomComp::SExp(ref s) if !s.is_empty() => lower_sexp(s, env, assign_to),
        MalAtomComp::Vector(ref v) => lower_vector(v, env, assign_to),
        MalAtomComp::HashMap(ref h) => lower_hashmap(h, env, assign_to),
        ast => {
            let assign_to = format_ident!("temp{}", assign_to);
            quote!(let #assign_to = #ast;)
        }
    }
}

fn lower_hashmap<'a>(
    h: &HashMap<String, MalAtomComp<'a>>,
    env: &Env,
    assign_to: u32,
) -> TokenStream {
    let (keys_temps, elems) = h
        .iter()
        .zip(assign_to..)
        .map(|((key, elem), assign_to)| {
            let temp = format_ident!("temp{}", assign_to);
            ((key, temp), lower(elem, env, assign_to))
        })
        .unzip::<_, _, Vec<_>, Vec<_>>();
    let mut hm_tokens = quote!(let mut hm = std::collections::HashMap::new(););

    for (k, t) in keys_temps.iter() {
        hm_tokens.extend(quote!(hm.insert(#k.into(), #t);));
    }

    hm_tokens.extend(quote!(hm));

    let temp = format_ident!("temp{}", assign_to);
    quote!(
        #(#elems)*
        let #temp = MalAtom::HashMap({#hm_tokens});
    )
}

fn lower_vector(v: &[MalAtomComp], env: &Env, assign_to: u32) -> TokenStream {
    let (elems, temps) = v
        .iter()
        .zip(assign_to..)
        .map(|(elem, assign_to)| {
            let temp = format_ident!("temp{}", assign_to);
            (lower(elem, env, assign_to), temp)
        })
        .unzip::<_, _, Vec<_>, Vec<_>>();

    let temp = format_ident!("temp{}", assign_to);
    quote!(
        #(#elems)*
        let #temp = MalAtom::Vector(vec![#(#temps),*]);
    )
}

fn lower_sexp(sexp: &[MalAtomComp], env: &Env, assign_to: u32) -> TokenStream {
    if let MalAtomComp::Symbol(s) = sexp[0] {
        match env.find(s) {
            Some(MalAtomCompRef::Func(func)) => {
                lower_mal_func_call_template(func, &sexp[1..], env, assign_to)
            }
            _ => {
                let err = MalResultComp::Err(format!("Function '{}' not defined", s));
                quote!(#err)
            }
        }
    } else {
        let err = MalResultComp::Err("Expected a function".to_string());
        quote!(#err)
    }
}

fn lower_mal_func_call_template(
    func: &MalFuncCallTemplate,
    args: &[MalAtomComp],
    env: &Env,
    assign_to: u32,
) -> TokenStream {
    let (args, arg_names) = args
        .iter()
        .zip(assign_to..)
        .map(|(atom, u)| {
            let assign_to = format_ident!("temp{}", u);
            (lower(atom, env, u), quote!(#assign_to))
        })
        .unzip::<_, _, Vec<_>, _>();

    let call = MalFuncCall::new(func, arg_names);

    let assign_to = format_ident!("temp{}", assign_to);
    quote!(#(#args)* let #assign_to = #call;)
}
