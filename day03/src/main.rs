use itertools::Itertools;

fn main() {
    println!(
        "part 1: {:?}",
        include_str!("../input.txt")
            .lines()
            .map(|line| line
                .split_whitespace()
                .map(|s| s.parse().unwrap())
                .collect_vec())
            .map(|sides| sides
                .iter()
                .permutations(3)
                .all(|sides| valid_triangle(sides)))
            .filter(|valid| *valid)
            .count()
    );

    println!(
        "part 2: {:?}",
        transpose(
            include_str!("../input.txt")
                .lines()
                .map(|line| {
                    line.split_whitespace()
                        .map(|s| s.parse::<u32>().unwrap())
                        .collect_vec()
                })
                .collect_vec()
        )
        .into_iter()
        .flatten()
        .chunks(3)
        .into_iter()
        .map(|sides| {
            sides
                .into_iter()
                .permutations(3)
                .all(|sides| valid_triangle(sides.iter().map(|s| s as &u32).collect_vec()))
        })
        .filter(|valid| *valid)
        .count()
    );
}

fn valid_triangle(sides: Vec<&u32>) -> bool {
    let (a, b, c) = (sides[0], sides[1], sides[2]);
    a + b > *c
}

pub fn transpose<T: Copy>(v: Vec<Vec<T>>) -> Vec<Vec<T>> {
    let len = v[0].len();
    (0..len)
        .into_iter()
        .map(|i| v.iter().map(|row| row[i]).collect())
        .collect()
}

