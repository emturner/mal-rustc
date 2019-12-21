use std::fs;
use std::io::{self, Write};
use std::path::Path;
use std::process::Command;

extern crate quote;
use quote::quote;

extern crate proc_macro2;

extern crate nom;
use nom::{error::*, Err};

extern crate mal_rustc_core;
use mal_rustc_core::{comp::*, parser, MAL_RUNTIME};

fn main() {
    loop {
        print!("user> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(0) => break,
            Ok(_) => {
                if !input.chars().all(|c| c.is_whitespace()) {
                    rep(&input)
                }
            }
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
        let env = create_core_env();
        let rust_tokens = lower(&ast, &env, 0);

        let output = format!(
            "{}\r\n{}",
            MAL_RUNTIME,
            quote!(
                fn mal_main() -> MalResult {
                    #rust_tokens
                    Ok(temp0)
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
