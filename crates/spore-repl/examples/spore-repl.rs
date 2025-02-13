use spore_repl::Repl;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    Repl::default().run()?;
    Ok(())
}
