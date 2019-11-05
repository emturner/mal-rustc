use std::fs;
use std::io::{self, Write};
use std::path::Path;
use std::process::Command;

extern crate quote;
use quote::quote;

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
    let output = quote!(
        fn main() {
            print!("{}", #input);
        }
    )
    .to_string();

    Ok(output)
}

fn rep(input: &str) {
    match compile_mal(input) {
        Ok(rust) => {
            if let Err(e) = fs::write(Path::new("mal-gen.rs"), rust) {
                println!("{}", e);
            } else {
                if let Ok(result) = Command::new("rustc").arg("mal-gen.rs").output() {
                    if !result.stderr.is_empty() {
                        println!("{}", String::from_utf8_lossy(&result.stderr));
                    } else if let Ok(result) = Command::new("./mal-gen").output() {
                        // just print as user input ends in <enter>
                        println!("{}", String::from_utf8_lossy(&result.stdout));
                    }
                }
            }
        }
        Err(e) => println!("{}", e),
    }
}
