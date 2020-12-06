use crate::advent_helper::advent_helper::parse_line_groups;

use std::collections::HashMap;

fn each_answer(s: &str, split_char: &str) -> (usize, usize)
{
	let mut count: HashMap<char, usize> = HashMap::new();
	let ws: Vec<&str> = s.split(split_char).collect();

	for w in &ws
	{
		for (_, c) in w.chars().enumerate()
		{
			*count.entry(c).or_insert(0) += 1;
		}
	}

	let mut n: usize = 0;
	for (_, value) in (&count).into_iter()
	{
    if *value == ws.len()
    {
    		n += 1;
    }
	}

	return (count.len(), n);
}

pub fn run()
{
	let v = parse_line_groups("../input/input06.txt", ":");

	let mut part1: usize = 0;
	let mut part2: usize = 0;
	for s in v
	{
		let (v1, v2) = each_answer(&s, ":");
		part1 += v1;
		part2 += v2;
	}

	println!("Day 6");
	println!("  Part 1: {}", part1);
	println!("  Part 2: {}", part2);

	return;
}
