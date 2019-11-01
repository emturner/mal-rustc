use std::io::{self, Write};
use std::fs;
use std::path::Path;
use std::process::{Command};

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
    let output = format!(r#"
        fn main() {{
            print!("{}");
        }}
    "#, input);

    Ok(output)
}

fn rep(input: &str) {
    match compile_mal(input) {
        Ok(rust) => {
            if let Err(e) = fs::write(Path::new("mal.rs"), rust) {
                println!("{}", e);
            } else {
                if let Ok(_) = Command::new("rustc").arg("mal.rs").output() {
                    if let Ok(result) = Command::new("./mal").output() {
                        print!("{}", String::from_utf8_lossy(&result.stdout));
                    }
                }
            }
        },
        Err(e) => println!("{}", e),
    }

}
