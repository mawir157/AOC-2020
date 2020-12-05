use crate::advent_helper::advent_helper::read_strs;

fn check_string(line: &str) -> (bool, bool)
{

	let parts: Vec<&str> = line.split(" ").collect();
	let lh: Vec<&str>    = parts[0].split("-").collect();
	let lo: usize        = lh[0].trim().parse().unwrap();
	let hi: usize        = lh[1].trim().parse().unwrap();
	let c: char          = (parts[1]).chars().nth(0).unwrap();
	let pw: &str         = parts[2];
	let ct: usize        = pw.matches(c).count();

	return (
		((ct >= lo) && (ct <= hi)),
		 ((pw.chars().nth(lo-1).unwrap() == c) !=
		  (pw.chars().nth(hi-1).unwrap() == c))
	);
}

fn count(pws: &Vec<String>) -> (usize, usize)
{
 	let mut c1: usize = 0;
 	let mut c2: usize = 0;

 	for pw in pws
	{
		let (ok1, ok2) = check_string(pw);
		if ok1
		{
			c1 += 1;
		}
		if ok2
		{
			c2 += 1;
		}
	}
	return (c1, c2);
}

pub fn run()
{
	let v = read_strs("../input/input02.txt").unwrap();
	let (c1, c2) = count(&v);

	println!("Day 2");
	println!("  Part 1: {}", c1);
	println!("  Part 2: {}", c2);
	return;
}