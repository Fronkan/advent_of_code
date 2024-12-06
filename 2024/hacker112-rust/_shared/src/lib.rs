use std::{
    fs::File,
    io::{self, BufRead},
    path::Path,
};

pub fn read_lines<P>(filename: P) -> impl Iterator<Item = String>
where
    P: AsRef<Path>,
{
    let file = File::open(filename).unwrap();
    let iter = io::BufReader::new(file).lines().flatten();
    iter
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_lines_worke() {
        let lines = read_lines("./test_lines.txt");
        let all_lines: Vec<String> = lines.collect();
        assert_eq!(all_lines, vec!["A", "B", "C"]);
    }
}
