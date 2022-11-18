use std::collections::HashMap;

#[derive(Debug, PartialEq)]
struct Room {
    enc_name: Vec<String>,
    id: u32,
    checksum: String,
}

fn parse_room(room: &str) -> Room {
    let parts = room.split('-');
    let enc_name = parts
        .clone()
        .take_while(|&part| part.chars().all(|c| c.is_alphabetic()))
        .map(|part| part.to_string())
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
    Room {
        enc_name,
        id,
        checksum,
    }
}

fn valid_room(r: &Room) -> bool {
    let mut counts = HashMap::new();
    for name in &r.enc_name {
        for c in name.chars() {
            *counts.entry(c).or_insert(0) += 1;
        }
    }
    let mut counts = counts
        .into_iter()
        .collect::<Vec<_>>();
    counts.sort_by(|a, b| b.1.cmp(&a.1).then(a.0.cmp(&b.0)));
    let checksum = counts
        .into_iter()
        .take(5)
        .map(|(c, _)| c)
        .collect::<String>();
    checksum == r.checksum.clone()
}

fn main() {
    println!(
        "{:?}",
        include_str!("../input.txt")
            .lines()
            .map(parse_room)
            .filter(valid_room)
            .map(|r| r.id)
            .sum::<u32>()
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_room() {
        assert_eq!(
            parse_room("aaaaa-bbb-z-y-x-123[abxyz]"),
            Room {
                enc_name: vec![
                    "aaaaa".to_string(),
                    "bbb".to_string(),
                    "z".to_string(),
                    "y".to_string(),
                    "x".to_string()
                ],
                id: 123,
                checksum: "abxyz".to_string(),
            }
        );
    }

    #[test]
    fn test_valid_room() {
        assert_eq!(
            valid_room(&Room {
                enc_name: vec![
                    "aaaaa".to_string(),
                    "bbb".to_string(),
                    "z".to_string(),
                    "y".to_string(),
                    "x".to_string()
                ],
                id: 123,
                checksum: "abxyz".to_string(),
            }),
            true
        );
    }

    #[test]
    fn test_invalid_room() {
        assert_eq!(
            valid_room(&Room {
                enc_name: vec![
                    "totally".to_string(),
                    "real".to_string(),
                    "room".to_string()
                ],
                id: 200,
                checksum: "decoy".to_string(),
            }),
            false
        );
    }
}
