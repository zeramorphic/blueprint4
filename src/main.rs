mod parse;

fn main() {
    let src_name: String = "test/LeanAPAP/ap.tex".to_owned();
    let src = std::fs::read_to_string(&src_name).unwrap();

    match parse::parse_latex(src, src_name) {
        Ok(blocks) => {
            println!("successfully parsed LaTeX input");
        }
        Err(errors) => {
            for error in errors {
                println!("{:?}", miette::Report::new(error));
            }
        }
    }
}
