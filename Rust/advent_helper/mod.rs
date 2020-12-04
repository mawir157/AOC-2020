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
}
