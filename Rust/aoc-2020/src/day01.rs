// mod advent_helper;
use crate::advent_helper::advent_helper::read_ints;

fn part1(v: &Vec<i64>, t: i64) -> i64
{
	for i in v
	{
		for j in v
		{
			if t == i + j
			{
				return i*j;
			}	
		}
	}
	return 0;
}

fn part2(v: &Vec<i64>, t: i64) -> i64
{
	for i in v
	{
		for j in v
		{
			if i + j > t
			{
				continue;
			}
			for k in v
			{
				if t == i + j + k
				{
					return i*j*k;
				}	
			}
		}
	}
	return 0;
}

pub fn run()
{
	let v = read_ints("../../input/input01.txt").unwrap();

	println!("Day 1");
	println!("  Part 1: {}", part1(&v, 2020));
	println!("  Part 2: {}", part2(&v, 2020));
	return;
}
