use crate::types::*;
use env::Env;
use env::MalAtomCompRef;
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use std::collections::HashMap;

pub mod env;

pub fn create_core_env<'a>() -> Env<'a> {
    let mut env = Env::new(None);
    env.set("+".into(), MalAtomCompRef::Func("mal_builtin_plus".into()));
    env.set("-".into(), MalAtomCompRef::Func("mal_builtin_sub".into()));
    env.set("*".into(), MalAtomCompRef::Func("mal_builtin_mul".into()));
    env.set("/".into(), MalAtomCompRef::Func("mal_builtin_div".into()));
    // drain core assignments
    env.get_new_vars();
    env
}

pub fn lower(ast: &MalAtomComp, env: &mut Env, assign_to: u32) -> TokenStream {
    let temp = get_ident(assign_to);
    let tokens = match ast {
        MalAtomComp::SExp(ref s) if !s.is_empty() => lower_sexp(s, env, assign_to),
        MalAtomComp::Vector(ref v) => lower_vector(v, env, assign_to),
        MalAtomComp::HashMap(ref h) => lower_hashmap(h, env, assign_to),
        MalAtomComp::Symbol(ref s) => {
            if let Some(MalAtomCompRef::Var(name)) = env.find(s) {
                let name = format_ident!("{}", name);
                // symbol vals are references of temporaries, so this won't cause a move
                quote!(let #temp = #name.is_defined(#s)?;)
            } else {
                let err = MalResultComp::Err(format!("Exception: symbol '{}' not found", s));
                lower_mal_result_temp_assignment(err, assign_to)
            }
        }
        ast => {
            let assign_to = temp;
            quote!(let #assign_to = &#ast;)
        }
    };

    if assign_to > 0 {
        tokens
    } else {
        // top level - create all the assigned vars in uninitialized state
        let assignments = env.get_new_vars();
        let assignments = assignments
            .iter()
            .map(|(a, new)| {
                let rust_name = match env.find(a) {
                    Some(MalAtomCompRef::Var(name)) | Some(MalAtomCompRef::Func(name)) => name,
                    _ => unreachable!(),
                };
                let rust_name = format_ident!("{}", rust_name);

                if *new {
                    quote!(let mut #rust_name = &MalAtom::Undefined;)
                } else {
                    quote!(let mut #rust_name = #rust_name;)
                }
            })
            .collect::<Vec<_>>();

        quote!(
            #(#assignments)*
            #tokens
        )
    }
}

fn lower_hashmap<'a>(
    h: &HashMap<String, MalAtomComp<'a>>,
    env: &mut Env,
    assign_to: u32,
) -> TokenStream {
    let (keys_temps, elems) = h
        .iter()
        .zip(assign_to..)
        .map(|((key, elem), assign_to)| {
            let temp = get_ident(assign_to);
            ((key, temp), lower(elem, env, assign_to))
        })
        .unzip::<_, _, Vec<_>, Vec<_>>();
    let mut hm_tokens = if keys_temps.is_empty() {
        quote!(let hm = std::collections::HashMap::new();)
    } else {
        quote!(let mut hm = std::collections::HashMap::new();)
    };

    for (k, t) in keys_temps.iter() {
        hm_tokens.extend(quote!(hm.insert(#k.to_string(), #t.clone());));
    }

    hm_tokens.extend(quote!(hm));

    let temp = get_ident(assign_to);
    quote!(
        #(#elems)*
        let #temp = &MalAtom::HashMap(Rc::new({#hm_tokens}));
    )
}

fn lower_vector(v: &[MalAtomComp], env: &mut Env, assign_to: u32) -> TokenStream {
    let (elems, temps) = v
        .iter()
        .zip(assign_to..)
        .map(|(elem, assign_to)| {
            let temp = get_ident(assign_to);
            (lower(elem, env, assign_to), temp)
        })
        .unzip::<_, _, Vec<_>, Vec<_>>();

    let temp = get_ident(assign_to);
    quote!(
        #(#elems)*
        let #temp = &MalAtom::Vector(Rc::new(vec![#(#temps.clone()),*]));
    )
}

fn lower_sexp(sexp: &[MalAtomComp], env: &mut Env, assign_to: u32) -> TokenStream {
    match sexp[0] {
        MalAtomComp::Symbol("def!") => lower_def(&sexp[1..], env, assign_to),
        MalAtomComp::Symbol("let*") => lower_let(&sexp[1..], env, assign_to),
        MalAtomComp::Symbol("do") => lower_do(&sexp[1..], env, assign_to),
        MalAtomComp::Symbol(s) => {
            if let Some(MalAtomCompRef::Func(func)) = env.find(s) {
                lower_mal_func_call_template(&func, &sexp[1..], env, assign_to)
            } else {
                let err = MalResultComp::Err(format!("Exception: function '{}' not found.", s));
                lower_mal_result_temp_assignment(err, assign_to)
            }
        }
        _ => {
            let err = MalResultComp::Err("Expected a function".to_string());
            lower_mal_result_temp_assignment(err, assign_to)
        }
    }
}

fn lower_def(args: &[MalAtomComp], env: &mut Env, assign_to: u32) -> TokenStream {
    let temp = get_ident(assign_to);
    if args.len() < 2 {
        let err = MalResultComp::Err("Exception: 'def!' requires two arguments".to_string());
        lower_mal_result_temp_assignment(err, assign_to)
    } else if let MalAtomComp::Symbol(s) = args[0] {
        let rust_var_name = get_rust_var_name(s);
        env.set(s.into(), MalAtomCompRef::Var(rust_var_name.clone()));

        let val = lower(&args[1], env, assign_to);
        let var = format_ident!("{}", rust_var_name);

        quote!(
            #val
            #var = #temp;
        )
    } else {
        let err = MalResultComp::Err("Exception: expected symbol".to_string());
        lower_mal_result_temp_assignment(err, assign_to)
    }
}

fn lower_let(args: &[MalAtomComp], env: &mut Env, assign_to: u32) -> TokenStream {
    if args.len() < 2 {
        let err = MalResultComp::Err("Exception: 'let*' requires two arguments".to_string());
        lower_mal_result_temp_assignment(err, assign_to)
    } else if let MalAtomComp::SExp(v) | MalAtomComp::Vector(v) = &args[0] {
        //      make new inner Env with env as outer
        let mut inner_env = Env::new(Some(env));

        //      lower symbol/value pairs to let bindings in inner [using lower_def?]
        let bindings = (0..v.len())
            .step_by(2)
            .map(|i| lower_def(&v[i..], &mut inner_env, 0))
            .collect::<Vec<_>>();

        //      lower third arg (ast)
        let body = lower(&args[1], &mut inner_env, 0);

        //      wrap in new block and return third arg
        let temp = get_ident(assign_to);
        let temp0 = get_ident(0);
        quote!(
            let #temp = &{|| -> MalResult {
                #(#bindings)*
                #body
                Ok(#temp0.clone())
            }}()?;
        )
    } else {
        let err = MalResultComp::Err("Exception: expected s-exp or vector".to_string());
        lower_mal_result_temp_assignment(err, assign_to)
    }
}

fn lower_do(args: &[MalAtomComp], env: &mut Env, assign_to: u32) -> TokenStream {
    if args.is_empty() {
        let temp = get_ident(assign_to);
        let err = MalResultComp::Err("Exception: 'do' requires at least one argument".to_string());
        quote!(let #temp: &MalAtom = &#err;)
    } else {
        let lowered = args.iter().map(|a| lower(a, env, assign_to));
        quote!(#(#lowered)*)
    }
}

fn get_rust_var_name(mal_symbol: &str) -> String {
    format!("_mal_{}", mal_symbol)
}

fn lower_mal_func_call_template(
    name: &str,
    args: &[MalAtomComp],
    env: &mut Env,
    assign_to: u32,
) -> TokenStream {
    let (args, arg_names) = args
        .iter()
        .zip(assign_to..)
        .map(|(atom, u)| {
            let assign_to = get_ident(u);
            (lower(atom, env, u), quote!(#assign_to))
        })
        .unzip::<_, _, Vec<_>, _>();

    let call = MalFuncCall::new(name, arg_names);

    let assign_to = get_ident(assign_to);
    quote!(#(#args)* let #assign_to = #call;)
}

fn lower_mal_result_temp_assignment(result: MalResultComp, assign_to: u32) -> TokenStream {
    let temp = get_ident(assign_to);
    quote!(let #temp: &MalAtom = &#result;)
}

pub fn get_ident(assign_to: u32) -> Ident {
    format_ident!("_{}", assign_to)
}
