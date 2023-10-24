use std::{collections::HashMap, fmt::Display, hash::Hash};

pub(crate) fn get_clear_style_instructions<T, U>(
    to_clear: &HashMap<T, U>,
) -> HashMap<T, ClearStyle<&U>>
where
    T: Hash + Eq + Clone + Display,
{
    to_clear
        .iter()
        .map(|(k, v)| (k.clone(), ClearStyle(v)))
        .collect()
}

#[derive(Debug)]
pub(crate) struct ClearStyle<T>(pub T);

impl<T> PartialEq<ClearStyle<T>> for ClearStyle<T>
where
    T: Eq,
{
    fn eq(&self, other: &ClearStyle<T>) -> bool {
        let ClearStyle(original) = self;
        let ClearStyle(new) = other;

        original == new
    }
}

#[cfg(test)]
mod tests {
    use super::{get_clear_style_instructions, ClearStyle};

    use crate::hash_map_diff_error::check_hash_map_key_diffs;

    use std::collections::HashMap;

    #[test]
    fn get_clear_style_instructions_returns_hash_map_with_all_given_keys() {
        let hash_map = HashMap::from([("a", 0), ("b", 2), ("c", 4)]);

        let clear_style_instructions = get_clear_style_instructions(&hash_map);

        match check_hash_map_key_diffs(&hash_map, &clear_style_instructions) {
            Ok(()) => hash_map.keys().for_each(|k| {
                assert_eq!(
                    &ClearStyle(hash_map.get(k).unwrap()),
                    clear_style_instructions.get(k).unwrap()
                )
            }),
            Err(diffs) => {
                panic!("Given hash map and returned should have same keys, but {:?} differences were reported", diffs);
            }
        }
    }
}
