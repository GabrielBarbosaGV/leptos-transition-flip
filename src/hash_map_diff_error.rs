use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    hash::Hash,
};

#[derive(Debug)]
pub(crate) struct HashMapDiffError<T> {
    pub present_in_original_but_not_new: HashSet<T>,
    pub present_in_new_but_not_original: HashSet<T>,
}

impl<T> PartialEq for HashMapDiffError<T>
where
    T: Eq + Hash,
{
    fn eq(&self, other: &Self) -> bool {
        let HashMapDiffError {
            present_in_original_but_not_new: first_original,
            present_in_new_but_not_original: second_original,
        } = self;
        let HashMapDiffError {
            present_in_original_but_not_new: first_new,
            present_in_new_but_not_original: second_new,
        } = other;

        first_original == first_new && second_original == second_new
    }
}

pub(crate) fn check_hash_map_key_diffs<'a, T, U, V>(
    original: &'a HashMap<T, U>,
    new: &'a HashMap<T, V>,
) -> Result<(), HashMapDiffError<T>>
where
    T: Hash + Eq + Clone + Display,
{
    let original_keys: HashSet<_> = original.keys().map(|t| t.clone()).collect();
    let new_keys: HashSet<_> = new.keys().map(|t| t.clone()).collect();

    if original_keys == new_keys {
        Ok(())
    } else {
        let present_in_original_but_not_new = original_keys
            .difference(&new_keys)
            .map(|t| t.clone())
            .collect();
        let present_in_new_but_not_original = new_keys
            .difference(&original_keys)
            .map(|t| t.clone())
            .collect();

        Err(HashMapDiffError {
            present_in_original_but_not_new,
            present_in_new_but_not_original,
        })
    }
}

#[cfg(test)]
mod tests {
    use std::collections::{
        HashSet,
        HashMap
    };

    use super::{HashMapDiffError, check_hash_map_key_diffs};

    #[test]
    fn check_hash_maps_key_diffs_returns_error_when_keys_differ() {
        let first_hash_map = HashMap::from([("a", 0), ("b", 1)]);
        let second_hash_map = HashMap::from([("c", 0), ("b", 1)]);

        if let Ok(_) = check_hash_map_key_diffs(&first_hash_map, &second_hash_map) {
            panic!("The function should return an error if hash maps have different keys");
        }
    }

    #[test]
    fn check_hash_map_key_diffs_returns_ok_when_keys_are_same() {
        let first_hash_map = HashMap::from([("a", 0), ("b", 1)]);
        let second_hash_map = HashMap::from([("a", 10), ("b", 20)]);

        if let Err(_) = check_hash_map_key_diffs(&first_hash_map, &second_hash_map) {
            panic!("The function should not report errors if keys are same");
        }
    }

    #[test]
    fn check_hash_map_key_diffs_returns_error_with_original_keys_missing_in_new() {
        let first_hash_map = HashMap::from([("a", 1), ("b", 2), ("c", 3)]);
        let second_hash_map = HashMap::from([("a", 1)]);

        match check_hash_map_key_diffs(&first_hash_map, &second_hash_map) {
            Ok(_) => panic!(
                "As keys from original are missing in new, an error should have been returned"
            ),
            Err(HashMapDiffError {
                present_in_original_but_not_new,
                ..
            }) => {
                let expected = HashSet::from(["b", "c"]);

                assert_eq!(expected, present_in_original_but_not_new);
            }
        }
    }

    #[test]
    fn check_hash_map_key_diff_returns_error_with_new_keys_missing_in_original() {
        let first_hash_map = HashMap::from([("a", 1)]);
        let second_hash_map = HashMap::from([("a", 2), ("b", 3), ("c", 4)]);

        match check_hash_map_key_diffs(&first_hash_map, &second_hash_map) {
            Ok(_) => panic!(
                "As keys from new are missing in original, an error should have been returneed"
            ),
            Err(HashMapDiffError {
                present_in_new_but_not_original,
                ..
            }) => {
                let expected = HashSet::from(["b", "c"]);

                assert_eq!(expected, present_in_new_but_not_original);
            }
        }
    }
}
