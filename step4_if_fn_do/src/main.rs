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
    comp::{env::Env, get_ident, *},
    parser, MAL_RUNTIME,
};

const MAL_REPL_NEW_MARKER: &str = "####NEW####";

fn main() {
    let mut tokens = vec![];
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

fn compile_mal(
    input: &str,
    env: &mut Env,
    saved_tokens: &mut Vec<TokenStream>,
) -> Result<String, String> {
    let parse_result = parser::parse_mal_atom(input);

    if let Ok((_, ast)) = parse_result {
        let rust_tokens = lower(&ast, env, 0);
        let temp = get_ident(0);
        let result_tokens = quote!(let _result: MalResult = Ok(#temp.clone()););

        let tokens = quote!(
            #(#saved_tokens
            *counter += 1;
            )*

            println!(#MAL_REPL_NEW_MARKER);

            #rust_tokens
            *counter += 1;

            #result_tokens
        );
        saved_tokens.insert(saved_tokens.len(), rust_tokens);
        saved_tokens.insert(saved_tokens.len(), result_tokens);

        let output = format!(
            "#![allow(non_snake_case)]
            {}
            {}",
            MAL_RUNTIME,
            quote!(
                fn mal_main(counter: &mut i32) -> MalResult {
                    #tokens
                    _result
                }
                fn main() {
                    let mut counter = 1;
                    match mal_main(&mut counter) {
                        Ok(ok) => println!("{}", ok),
                        Err(err) => {
                            println!("{}", err);
                            std::process::exit(counter);
                        },
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

fn rep(input: &str, env: &mut Env, tokens: &mut Vec<TokenStream>) {
    match compile_mal(input, env, tokens) {
        Ok(rust) => {
            if let Err(e) = fs::write(Path::new("mal-gen.rs"), rust) {
                println!("{}", e);
            } else if let Ok(result) = Command::new("rustc").arg("mal-gen.rs").output() {
                if !result.stderr.is_empty() {
                    println!("{}", String::from_utf8_lossy(&result.stderr));
                } else if let Ok(result) = Command::new("./mal-gen").output() {
                    if let Some(code) = result.status.code() {
                        if code > 0 {
                            // counter starts at 1
                            tokens.truncate(code as usize - 1);
                        }
                    }
                    let output = String::from_utf8_lossy(&result.stdout)
                        .lines()
                        .skip_while(|l| l != &MAL_REPL_NEW_MARKER)
                        .skip(1)
                        .collect::<Vec<_>>()
                        .join("\n");

                    println!("{}", output);
                }
            }
        }
        Err(e) => println!("{}", e),
    }
}
