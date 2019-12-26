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
use mal_rustc_core::{
    comp::{env::Env, *},
    parser, MAL_RUNTIME,
};

fn main() {
    let mut tokens = TokenStream::new();
    let mut env = create_core_env();

    loop {
        print!("user> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(0) => break,
            Ok(_) => {
                if !input.chars().all(|c| c.is_whitespace()) {
                    rep(&input, &mut env, &mut tokens)
                }
            }
            Err(e) => {
                println!("{}", e);
                break;
            }
        }
    }
}

fn compile_mal(input: &str, env: &mut Env, tokens: &mut TokenStream) -> Result<String, String> {
    let parse_result = parser::parse_mal_atom(input);

    if let Ok((_, ast)) = parse_result {
        let rust_tokens = lower(&ast, env, 0);

        tokens.extend(rust_tokens);
        tokens.extend(quote!(let _result: MalResult = Ok(temp0.clone());));

        let output = format!(
            "#![allow(non_snake_case)]
            {}
            {}",
            MAL_RUNTIME,
            quote!(
                fn mal_main() -> MalResult {
                    #tokens
                    _result
                }
                fn main() {
                    match mal_main() {
                        Ok(ok) => println!("{}", ok),
                        Err(err) => println!("{}", err),
                    }
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

fn rep(input: &str, env: &mut Env, tokens: &mut TokenStream) {
    match compile_mal(input, env, tokens) {
        Ok(rust) => {
            if let Err(e) = fs::write(Path::new("mal-gen.rs"), rust) {
                println!("{}", e);
            } else if let Ok(result) = Command::new("rustc").arg("mal-gen.rs").output() {
                if !result.stderr.is_empty() {
                    println!("{}", String::from_utf8_lossy(&result.stderr));
                } else if let Ok(result) = Command::new("./mal-gen").output() {
                    println!("{}", String::from_utf8_lossy(&result.stdout));
                }
            }
        }
        Err(e) => println!("{}", e),
    }
}
