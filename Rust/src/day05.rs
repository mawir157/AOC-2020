use crate::advent_helper::advent_helper::read_strs;

fn get_seat(ones: &str, label: &str, l: u32) -> u32
{
	let mut n: u32 = 0;
	let base: u32 = 2;
	for (i, c) in label.chars().enumerate()
	{
		if ones.contains(&c.to_string())
		{
			n += base.pow(l - 1 - (i as u32));
		}
	}

	return n;
}

fn missing(is: &Vec<u32>) -> u32
{
	for i in 1..(is.len() - 1)
	{
		if (is[i] + 1) != is[i+1]
		{
			return is[i] + 1;
		}
	}
	return 0;
}

pub fn run()
{
	let v = read_strs("../input/input05.txt").unwrap();
	let mut w: Vec<u32> = vec![];

	for s in v
	{
		w.push(get_seat("BR", &s, 10));
	}
	w.sort();

	println!("Day 5");
	println!("  Part 1: {}", w[w.len()-1]);
	println!("  Part 2: {}", missing(&w));

	return;
}
