use std::process::Command;

fn main() {
    if let Err(err) = Command::new("tailwindcss")
        .args("-c ./tailwind.config.js -o ./static/styles.css".split_whitespace())
        .status()
    {
        let message = format!("Command \"tailwindcss -c ./tailwind.config.js -o ./static/styles.css\" returned error, is tailwindcss installed? Err: {}", err);
        println!("cargo:warning={}", message);
    }
}
