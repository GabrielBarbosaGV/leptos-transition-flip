use std::{collections::HashMap, fmt::Display, hash::Hash};

pub(crate) fn get_remove_transform_and_set_transition_instructions<T, U>(
    to_remove: &HashMap<T, U>,
) -> HashMap<T, RemoveTransformAndSetTransition<&U>>
where
    T: Hash + Eq + Clone + Display,
{
    to_remove
        .iter()
        .map(|(k, v)| (k.clone(), RemoveTransformAndSetTransition(v)))
        .collect()
}

#[derive(Debug)]
pub(crate) struct RemoveTransformAndSetTransition<T>(pub T);

impl<T> PartialEq<RemoveTransformAndSetTransition<T>> for RemoveTransformAndSetTransition<T>
where
    T: Eq,
{
    fn eq(&self, other: &RemoveTransformAndSetTransition<T>) -> bool {
        let RemoveTransformAndSetTransition(original) = self;
        let RemoveTransformAndSetTransition(new) = other;

        original == new
    }
}

#[cfg(test)]
mod tests {
    use super::{
        get_remove_transform_and_set_transition_instructions, RemoveTransformAndSetTransition,
    };

    use crate::hash_map_diff_error::check_hash_map_key_diffs;

    use std::collections::HashMap;

    #[test]
    fn get_remove_transform_and_set_transition_instructions_returns_hash_map_with_all_given_keys() {
        let hash_map = HashMap::from([("a", 0), ("b", 2), ("c", 4)]);

        let remove_transform_instructions =
            get_remove_transform_and_set_transition_instructions(&hash_map);

        match check_hash_map_key_diffs(&hash_map, &remove_transform_instructions) {
            Ok(()) => {
                hash_map.keys().for_each(|k| {
                    assert_eq!(
                        &RemoveTransformAndSetTransition(hash_map.get(k).unwrap()),
                        remove_transform_instructions.get(k).unwrap()
                    );
                });
            }
            Err(diffs) => {
                panic!("Given hash map and returned should have the same keys, but {:?} differences were reported", diffs);
            }
        }
    }
}
