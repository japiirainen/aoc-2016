use std::env;
use std::fs;
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
struct Room {
    name: Vec<String>,
    id: u32,
    checksum: String,
}

fn parse_room(room: &str) -> Room {
    let parts = room.split('-');
    let name = parts
        .clone()
        .take_while(|&p| p.chars().all(|c| c.is_alphabetic()))
        .map(|p| p.to_string())
        .collect::<Vec<_>>();
    let id = parts
        .clone()
        .last()
        .unwrap()
        .split('[')
        .next()
        .unwrap()
        .parse::<u32>()
        .unwrap();
    let checksum = parts
        .last()
        .unwrap()
        .split('[')
        .last()
        .unwrap()
        .trim_end_matches(']')
        .to_string();
    Room {name, id, checksum}
}

fn is_room_valid(r: &Room) -> bool {
    let mut counts = HashMap::new();
    for name in &r.name {
        for c in name.chars() {
            *counts.entry(c).or_insert(0) += 1;
        }
    }
    let mut counts = counts.into_iter().collect::<Vec<_>>();
    counts.sort_by(|a, b| b.1.cmp(&a.1).then(a.0.cmp(&b.0)));
    let checksum = counts
        .into_iter()
        .take(5)
        .map(|(c, _)| c)
        .collect::<String>();
    checksum == r.checksum.clone()
}

fn decrypt(r: &Room) -> String {
    r.name
       .join(" ")
       .chars()
       .map(|c| {
           if c == ' ' {
               ' '
           } else {
               (((c as u8 - b'a') + (r.id % 26) as u8) % 26 + b'a') as char
           }
       })
       .collect::<String>()
}

fn solve_file(file_path: &str) -> () {
    println!("Solving for file : {}", file_path);
    let contents = fs::read_to_string(file_path)
        .expect(format!("Failed to read file : {}", file_path).as_str());
    println!(
        "part 1 : {}",
        contents.lines().map(parse_room).filter(is_room_valid).map(|r| r.id).sum::<u32>()
    );
    println!(
        "part 2 : {}",
        contents
          .lines()
          .map(parse_room)
          .filter(is_room_valid)
          .filter(|r| decrypt(r) == "northpole object storage")
          .map(|r| r.id)
          .sum::<u32>()
    )
}

fn main() {
    let args: Vec<String> = env::args().collect();
    for file_path in &args[1..] {
        solve_file(file_path);
    }
}
