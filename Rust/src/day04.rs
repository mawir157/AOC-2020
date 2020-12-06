use crate::advent_helper::advent_helper::parse_line_groups;

use std::collections::HashMap;
use regex::Regex;

fn check_id(line: &str, minimal: bool) -> bool
{
	let mut part1 = true;
	let mut part2 = true;

	let fields = vec!["byr".to_string(), "iyr".to_string(), "eyr".to_string(),
	                  "hgt".to_string(), "hcl".to_string(), "ecl".to_string(),
	                  "pid".to_string()];
	let mut seen = HashMap::new();
	for f in fields.clone()
	{
		seen.insert(f,false,);
	}

	let ss: Vec<&str> = line.split(" ").collect();
	for q in ss
	{
		let p: Vec<&str> = q.split(":").collect();
		let key = p[0];
		let value = p[1];
		seen.insert(key.to_string(), true,);

		let mut ok = true;
		match key
		{
			"byr" =>
			{
				let re = Regex::new(r"(19[2-9][0-9]|200[0-2])").unwrap();
				ok = re.is_match(&value);
			},
			"iyr" =>
			{
				let re = Regex::new(r"(201[0-9]|2020)").unwrap();
				ok = re.is_match(&value);
			},
			"eyr" =>
			{
				let re = Regex::new(r"(202[0-9]|2030)").unwrap();
				ok = re.is_match(&value);
			},
			"hgt" =>
			{
				let re = Regex::new(r"(59in|6[0-9]in|7[0-6]in|1[5-8][0-9]cm|19[0-3]cm)").unwrap();
				ok = re.is_match(&value);
			},
			"hcl" =>
			{
				let re = Regex::new(r"^#([0-9a-f]){6}$").unwrap();
				ok = re.is_match(&value);
			},
			"ecl" =>
			{
				let re = Regex::new(r"^(amb|blu|brn|gry|grn|hzl|oth)").unwrap();
				ok = re.is_match(&value);
			},
			"pid" =>
			{
				let re = Regex::new(r"^([0-9]){9}$").unwrap();
				ok = re.is_match(&value);
			},
			"cid" => {},
			_  => {},
		}
		part2 &= ok;
	}

	for f in fields
	{
		part1 &= seen.get(&f).unwrap();
	}

	return part1 && (minimal || part2);
}

pub fn run()
{
	let v = parse_line_groups("../input/input04.txt", " ");

	let mut part1 = 0;
	let mut part2 = 0;
	for s in v
	{
		if check_id(&s, true)
		{
			part1 += 1;
		}

		if check_id(&s, false)
		{
			part2 += 1;
		}
	}

	println!("Day 4");
	println!("  Part 1: {}", part1);
	println!("  Part 2: {}", part2);

	return;
}
