mod bluecode;
mod bluecode_to_html;
mod latex_to_bluecode;
mod parse_latex;

fn main() {
    let src_name: String = "test/LeanAPAP/ap.tex".to_owned();
    let src = std::fs::read_to_string(&src_name).unwrap();

    match parse_latex::parse_latex(&src, &src_name) {
        Ok(blocks) => match latex_to_bluecode::latex_to_bluecode(&src, &src_name, blocks) {
            Ok(items) => {
                let header = "<script src=\"https://polyfill.io/v3/polyfill.min.js?features=es6\"></script><script id=\"MathJax-script\" async src=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js\"></script>";
                std::fs::write(
                    "test/LeanAPAP/ap.html",
                    format!("{}{}", header, bluecode_to_html::bluecode_to_html(&items)),
                )
                .unwrap();
            }
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
