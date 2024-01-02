/// Statically load a vector of strings from a text file.
/// Empty lines and lines starting with a `#` are ignored.
#[macro_export]
macro_rules! resource {
    ($name:literal) => {
        include_str!(concat!("res/", $name))
            .lines()
            .filter(|line| !line.trim().is_empty() && !line.starts_with('#'))
            .collect::<Vec<&'static str>>()
    };
}
