use std::collections::HashMap;
use std::fs;
use std::io::{self, Write};
use std::path::Path;
use std::process::Command;

extern crate quote;
use quote::quote;

extern crate proc_macro2;
use proc_macro2::TokenStream;

extern crate nom;
use nom::{error::*, Err};

extern crate mal_rustc_core;
use mal_rustc_core::{parser, MAL_RUNTIME, types::{MalAtom, MalFuncCallTemplate, MalResultComp, MalFuncCall}};

fn main() {
    loop {
        print!("user> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(0) => break,
            Ok(_) => rep(&input),
            Err(e) => {
                println!("{}", e);
                break;
            }
        }
    }
}

fn compile_mal(input: &str) -> Result<String, String> {
    let parse_result = parser::parse_mal_atom(input);

    if let Ok((_, ast)) = parse_result {
        let env = HashMap::new();
        let rust_tokens = lower(ast, &env);

        let output = format!(
            "{}\r\n{}",
            MAL_RUNTIME,
            quote!(
                fn main() {
                    println!("{}", #rust_tokens);
                }
            )
        );

        Ok(output)
    } else {
        let err = match parse_result {
            Err(Err::Error(e)) | Err(Err::Failure(e)) => {
                if e.errors
                    .iter()
                    .any(|e| VerboseErrorKind::Context("UNBALANCED") == e.1)
                {
                    "(EOF|end of input|unbalanced)".into()
                } else {
                    convert_error(input, e)
                }
            }
            _ => "".into(),
        };
        Err(err)
    }
}

fn rep(input: &str) {
    match compile_mal(input) {
        Ok(rust) => {
            if let Err(e) = fs::write(Path::new("mal-gen.rs"), rust) {
                println!("{}", e);
            } else if let Ok(result) = Command::new("rustc").arg("mal-gen.rs").output() {
                if !result.stderr.is_empty() {
                    println!("{}", String::from_utf8_lossy(&result.stderr));
                } else if let Ok(result) = Command::new("./mal-gen").output() {
                    // just print as user input ends in <enter>
                    println!("{}", String::from_utf8_lossy(&result.stdout));
                }
            }
        }
        Err(e) => println!("{}", e),
    }
}

fn lower(ast: MalAtom, env: &HashMap<&str, MalFuncCallTemplate>) -> TokenStream {
    match ast {
        MalAtom::SExp(ref s) if !s.is_empty() => lower_sexp(s, env),
        ast => quote!(#ast),
    }
}

fn lower_sexp(sexp: &[MalAtom], env: &HashMap<&str, MalFuncCallTemplate>) -> TokenStream {
    if let MalAtom::Symbol(s) = sexp[0] {
        match env.get(s) {
            Some(func) => lower_mal_func_call_template(func, &sexp[1..]),
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

fn lower_mal_func_call_template(func: &MalFuncCallTemplate, args: &[MalAtom]) -> TokenStream {
    let call = MalFuncCall::new(func, args);
    quote!(#call)
}
