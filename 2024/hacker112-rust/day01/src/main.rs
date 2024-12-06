use shared::read_lines;

fn main() {
    let filename = "./input";
    let (first, second) = read_sorted_lists(filename);

    let sum = part1(&first, &second);

    println!("part 1: {}", sum);

    let similarities = part2(&first, &second);
    println!("part 2: {}", similarities);
}

fn part2(first: &Vec<u32>, second: &Vec<u32>) -> u32 {
    let similarities = similiarities_sum(&first, &second);
    similarities
}

fn part1(first: &Vec<u32>, second: &Vec<u32>) -> u32 {
    let sum: u32 = first
        .iter()
        .zip(second.iter())
        .map(|x| {
            let distance = x.0.abs_diff(*x.1);
            distance
        })
        .sum();
    sum
}

fn read_sorted_lists(filename: &str) -> (Vec<u32>, Vec<u32>) {
    let lines = read_lines(filename);

    let (mut first, mut second) =
        lines.fold((Vec::<u32>::new(), Vec::<u32>::new()), |mut acc, line| {
            let numbers: Vec<_> = line
                .split_whitespace()
                .map(|n| n.parse::<u32>().unwrap())
                .collect();

            acc.0.push(numbers[0]);
            acc.1.push(numbers[1]);

            return acc;
        });
    first.sort();
    second.sort();

    (first, second)
}

fn similiarities_sum(a: &Vec<u32>, b: &Vec<u32>) -> u32 {
    let similarities: u32 = a.iter().map(|n| similarity_score(n, b)).sum();
    similarities
}

fn similarity_score(needle: &u32, haystack: &Vec<u32>) -> u32 {
    let t = haystack
        .iter()
        .filter(|h| {
            let a = **h;
            let b = *needle;

            a == b
        })
        .count();

    t as u32 * needle
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lists() {
        let (first, second) = read_sorted_lists("./input_example");
        assert_eq!(first, vec![1, 2, 3, 3, 3, 4]);
        assert_eq!(second, vec![3, 3, 3, 4, 5, 9]);
    }

    #[test]
    fn test_part_1() {
        let (first, second) = read_sorted_lists("./input_example");
        let sum = part1(&first, &second);
        assert_eq!(sum, 11);
    }

    #[test]
    fn test_part_2() {
        let (first, second) = read_sorted_lists("./input_example");
        let sum = part2(&first, &second);
        assert_eq!(sum, 31);
    }
}
