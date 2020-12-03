mod advent_helper;
use advent_helper::advent_helper::read_strs;

fn tbg(w: usize, ss: &Vec<String>, dx: usize, dy: usize) -> usize
{
	let mut trees: usize = 0;
	let mut y: usize = 0;

	for s in ss.iter().step_by(dx)
	{
		if s.chars().nth(y).unwrap() == '#'
		{
			trees +=1;
		}

		y = (y + dy) % w;
	}
	return trees
}

fn main()
{
  let v = read_strs("../input/input03.txt").unwrap();
  let w = v[0].len();

  let t1 = tbg(w, &v, 1,1);
  let t2 = tbg(w, &v, 1,3);
  let t3 = tbg(w, &v, 1,5);
  let t4 = tbg(w, &v, 1,7);
  let t5 = tbg(w, &v, 2,1);

  println!("Day 3");
  println!("  Part 1: {}", t2);
  println!("  Part 2: {}", t1*t2*t3*t4*t5);

  return;
}
