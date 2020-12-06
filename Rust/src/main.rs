mod advent_helper;
mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
mod day06;

fn main() {
	let pattern = std::env::args().nth(1).expect("no pattern given");

	match pattern.as_str()
	{
		"01" => day01::run(), 
		"02" => day02::run(), 
		"03" => day03::run(), 
		"04" => day04::run(), 
		"05" => day05::run(), 
		"06" => day06::run(), 
		_  =>
		{
			day01::run();
			day02::run();
			day03::run();
			day04::run();
			day05::run();
			day06::run();
		},
	}



}
