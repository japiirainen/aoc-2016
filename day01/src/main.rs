use std::collections::HashSet;

#[derive(Debug, Clone)]
enum Instruction {
    R(i32),
    L(i32),
}

#[derive(Debug, Clone, Copy)]
enum Direction {
    North,
    East,
    South,
    West,
}

fn parse_instruction(s: &str) -> Instruction {
    let (dir, dist) = s.split_at(1);
    let dist = dist.parse().unwrap();
    match dir {
        "R" => Instruction::R(dist),
        "L" => Instruction::L(dist),
        _ => unreachable!(),
    }
}

fn rotate(dir: &Direction, instruction: Instruction) -> Direction {
    match (dir, instruction) {
        (Direction::North, Instruction::L(_)) => Direction::West,
        (Direction::East, Instruction::L(_)) => Direction::North,
        (Direction::South, Instruction::L(_)) => Direction::East,
        (Direction::West, Instruction::L(_)) => Direction::South,
        (d, Instruction::R(_)) => rotate(
            &rotate(&rotate(d, Instruction::L(1)), Instruction::L(1)),
            Instruction::L(1),
        ),
    }
}

fn mov(pos: (i32, i32), dir: Direction) -> (i32, i32) {
    match dir {
        Direction::North => (pos.0, pos.1 + 1),
        Direction::East => (pos.0 + 1, pos.1),
        Direction::South => (pos.0, pos.1 - 1),
        Direction::West => (pos.0 - 1, pos.1),
    }
}

fn shortest_dist(pos: (i32, i32)) -> i32 {
    pos.0.abs() + pos.1.abs()
}

fn run_instructions(instructions: Vec<Instruction>) -> Vec<(i32, i32)> {
    let directions = instructions
        .iter()
        .scan(Direction::North, |dir, instruction| {
            *dir = rotate(dir, instruction.clone());
            Some(*dir)
        })
        .collect::<Vec<_>>();

    let distances = instructions
        .iter()
        .map(|d| match d {
            Instruction::R(d) => *d,
            Instruction::L(d) => *d,
        })
        .collect::<Vec<_>>();

    let moves = directions
        .iter()
        .zip(distances.iter())
        .flat_map(|(dir, dist)| (0..*dist).map(move |d| (dir, d)));

    moves
        .scan((0, 0), |pos, (dir, _)| {
            *pos = mov(*pos, *dir);
            Some(*pos)
        })
        .collect()
}

fn find_duplicate(moves: Vec<(i32, i32)>) -> (i32, i32) {
    fn go(moves: Vec<(i32, i32)>, mut visited: HashSet<(i32, i32)>) -> (i32, i32) {
        let ([x], xs) = moves.split_at(1) else {
            unreachable!()
        };
        if visited.contains(x) {
            return *x;
        }
        visited.insert(*x);
        go(xs.to_vec(), visited)
    }
    go(moves, HashSet::new())
}

pub fn main() {
    let input = include_str!("../input.txt");
    let instructions = input.split(", ").map(parse_instruction).collect::<Vec<_>>();
    let moves = run_instructions(instructions.clone());
    println!("part1: {}", shortest_dist(*moves.last().unwrap()));
    println!("part2: {}", shortest_dist(find_duplicate(moves)));
}
