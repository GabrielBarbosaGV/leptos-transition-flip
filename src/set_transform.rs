use std::{collections::HashMap, fmt::Display, hash::Hash};

pub(crate) fn get_set_transform_instructions<T, U>(
    to_set: &HashMap<T, U>,
) -> HashMap<T, SetTransform<&U>>
where
    T: Hash + Eq + Clone + Display,
{
    to_set
        .iter()
        .map(|(k, v)| (k.clone(), SetTransform(v)))
        .collect()
}

#[derive(Debug)]
pub(crate) struct SetTransform<T>(pub T);

impl<T> PartialEq<SetTransform<T>> for SetTransform<T>
where
    T: Eq,
{
    fn eq(&self, other: &SetTransform<T>) -> bool {
        let SetTransform(original) = self;
        let SetTransform(new) = other;

        original == new
    }
}

#[cfg(test)]
mod tests {
    use super::{
        get_set_transform_instructions,
        SetTransform
    };
    use crate::hash_map_diff_error::check_hash_map_key_diffs;
    use std::collections::HashMap;
    #[test]
    fn get_set_transform_instructions_returns_hash_map_with_all_given_keys() {
        let hash_map = HashMap::from([("a", 1), ("b", 2), ("c", 3)]);

        let set_transform_and_transition_instructions = get_set_transform_instructions(&hash_map);

        match check_hash_map_key_diffs(&hash_map, &set_transform_and_transition_instructions) {
            Ok(()) => {
                hash_map.keys().for_each(|k| {
                    assert_eq!(
                        set_transform_and_transition_instructions.get(k).unwrap(),
                        &SetTransform(hash_map.get(k).unwrap())
                    );
                });
            }
            Err(diffs) => {
                panic!("Given hash map and returned should have the same keys, but {:?} differences were reported", diffs);
            }
        }
    }
}
