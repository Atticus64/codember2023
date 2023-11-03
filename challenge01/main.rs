use std::collections::hash_map::HashMap;

fn get_code(phrase: &str) -> String {
	let phrase_str = phrase.to_string().split('\n').collect::<Vec<&str>>().join(" ");
	let words: Vec<String> = phrase_str.split(' ').map(|s| s.to_string().to_lowercase()).collect();

	let mut coincidences: HashMap<String, i32> = HashMap::new();
	let mut order = vec![];

	for word in words {
		if word.is_empty() {
				continue;
		}
		if !coincidences.contains_key(&word) {
			 order.push(word.to_string());
		}

		*coincidences.entry(word).or_insert(0) += 1;
	}


	let mut result = String::new();

	for word in order {
		let w = coincidences.get(&word).unwrap();

		let fmt = format!("{}{}", word, w);
		result.push_str(fmt.as_str());
	}

	result
}

fn main() {
	let encoded = std::fs::read_to_string("challenge01/message.txt").unwrap();
	println!("{}", get_code(encoded.as_str()));
}


#[cfg(test)]
mod tests {
    use crate::get_code;

    #[test]
    fn cases() {
        let phrase1 = "llaveS casa CASA casa llaves";
        let phrase2 = "taza ta za taza";
        let phrase3 = "casas casa casasas";
		assert_eq!(get_code(phrase1), "llaves2casa3");
		assert_eq!(get_code(phrase2), "taza2ta1za1");
		assert_eq!(get_code(phrase3), "casas1casa1casasas1");
    }
}