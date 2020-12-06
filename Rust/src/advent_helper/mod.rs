#[allow(dead_code)]
pub mod advent_helper
{
	use std::{
		fs::File,
		io::{prelude::*, BufReader, Error, ErrorKind},
	};

	pub fn read_ints(path: &str) -> Result<Vec<i64>, Error>
	{
		let f = File::open(path).unwrap();
		let br = BufReader::new(f);
		let mut v = vec![];
		for line in br.lines() {
			v.push(line?
			        .trim()
			        .parse()
	 		        .map_err(|e| Error::new(ErrorKind::InvalidData, e))?);
		}
		Ok(v)
	}

	pub fn read_strs(path: &str) -> Result<Vec<String>, Error>
	{
		let f = File::open(path).unwrap();
		let br = BufReader::new(f);
		let mut v: Vec<String> = vec![];
		for line in br.lines() {
			v.push(line?);
		}
		Ok(v)
	}

	pub fn parse_line_groups(path: &str, sep: &str) -> Vec<String>
	{
		let ss = read_strs(path).unwrap();

		let mut v: Vec<String> = vec![];
		
		let mut temp: String = "".to_string();
		for s in ss
		{
			if s.chars().count() == 0
			{
				v.push(temp.clone());
				temp = "".to_string();
			}
			else
			{
				if temp.chars().count() == 0
				{
					temp = s;
				}
				else
				{
					temp = temp + sep + &s;
				}
			}
		}
		// for some reason rust wont allow an empty string at the end of br.lines()
		v.push(temp.clone());

		return v;
	}
}
