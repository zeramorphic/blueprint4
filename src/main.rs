mod bluecode;
mod latex_to_bluecode;
mod parse_latex;

fn main() {
    let src_name: String = "test/LeanAPAP/ap.tex".to_owned();
    let src = std::fs::read_to_string(&src_name).unwrap();

    match parse_latex::parse_latex(&src, &src_name) {
        Ok(blocks) => match latex_to_bluecode::latex_to_bluecode(&src, &src_name, blocks) {
            Ok(items) => println!("{items:#?}"),
            Err(errors) => {
                for error in errors {
                    println!("{:?}", miette::Report::new(error));
                }
            }
        },
        Err(errors) => {
            for error in errors {
                println!("{:?}", miette::Report::new(error));
            }
        }
    }
}
