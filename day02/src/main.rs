use std::cmp;

fn next_pos(bounds: (i32, i32)) -> impl Fn(char, (i32, i32)) -> (i32, i32) {
    return move |dir, pos: (i32, i32)| match dir {
        'U' => (cmp::max(pos.0 - 1, 0), pos.1),
        'D' => (cmp::min(pos.0 + 1, bounds.0), pos.1),
        'L' => (pos.0, cmp::max(pos.1 - 1, 0)),
        'R' => (pos.0, cmp::min(pos.1 + 1, bounds.1)),
        _ => unreachable!(),
    };
}

fn run_instructions(
    f: impl Fn(char, (i32, i32)) -> (i32, i32),
    is: Vec<char>,
    keypad: Vec<Vec<char>>,
    pos: (i32, i32),
) -> (char, (i32, i32)) {
    is.iter().fold(
        (keypad[pos.0 as usize][pos.1 as usize], pos),
        |(_, p), &i| {
            let np = f(i, p);
            let c = keypad[np.0 as usize][np.1 as usize];
            if c == ' ' {
                (keypad[p.0 as usize][p.1 as usize], p)
            } else {
                (c, np)
            }
        },
    )
}

fn main() {
    let instructions = include_str!("../input.txt")
        .lines()
        .map(|d| d.chars().collect::<Vec<char>>())
        .collect::<Vec<_>>();
    let p1_pad = vec![
        vec!['1', '2', '3'],
        vec!['4', '5', '6'],
        vec!['7', '8', '9'],
    ];
    let p2_pad = vec![
        vec![' ', ' ', '1', ' ', ' '],
        vec![' ', '2', '3', '4', ' '],
        vec!['5', '6', '7', '8', '9'],
        vec![' ', 'A', 'B', 'C', ' '],
        vec![' ', ' ', 'D', ' ', ' '],
    ];
    let part1 = instructions
        .iter()
        .fold(((1, 1), "".to_string()), |(pos, res), is| {
            let (p, next_pos) =
                run_instructions(next_pos((2, 2)), is.to_vec(), p1_pad.clone(), pos);
            (next_pos, res + &p.to_string())
        })
        .1;
    let part2 = instructions
        .iter()
        .fold(((2, 0), "".to_string()), |(pos, res), is| {
            let (p, next_pos) =
                run_instructions(next_pos((4, 4)), is.to_vec(), p2_pad.clone(), pos);
            (next_pos, res + &p.to_string())
        })
        .1;
    println!("part 1: {}", part1);
    println!("part 2: {}", part2);
}
