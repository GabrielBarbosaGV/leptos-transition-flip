use std::{collections::HashMap, fmt::Display, hash::Hash};

use crate::hash_map_diff_error::{check_hash_map_key_diffs, HashMapDiffError};

pub(crate) fn get_diff_positions_instructions<'a, T, U>(
    original: &'a HashMap<T, U>,
    new: &'a HashMap<T, U>,
) -> Result<HashMap<T, DiffPositions<&'a U>>, HashMapDiffError<T>>
where
    T: Hash + Eq + Clone + Display,
{
    check_hash_map_key_diffs(&original, &new)?;

    Ok(original
        .keys()
        .map(|k| {
            let original = original.get(k).unwrap();
            let new = new.get(k).unwrap();

            (k.clone(), DiffPositions(original, new))
        })
        .collect())
}

#[derive(Debug)]
pub(crate) struct DiffPositions<T>(pub T, pub T);

impl<T> PartialEq<DiffPositions<&T>> for DiffPositions<T>
where
    T: Eq,
{
    fn eq(&self, other: &DiffPositions<&T>) -> bool {
        let DiffPositions(original, new) = self;
        let DiffPositions(original_ref, new_ref) = other;

        original == *original_ref && new == *new_ref
    }
}

impl<T> PartialEq<DiffPositions<T>> for DiffPositions<T>
where
    T: Eq,
{
    fn eq(&self, other: &DiffPositions<T>) -> bool {
        let DiffPositions(original, new) = self;
        let DiffPositions(second_original, second_new) = other;

        original == second_original && new == second_new
    }
}

#[cfg(test)]
mod tests {
    use super::{get_diff_positions_instructions, DiffPositions};
    use crate::hash_map_diff_error::{check_hash_map_key_diffs, HashMapDiffError};
    use std::collections::HashMap;

    #[test]
    fn get_diff_positions_instructions_returns_ok_when_hash_maps_have_same_keys() {
        let first_hash_map = HashMap::from([("a", 10), ("b", 20), ("c", 30)]);
        let second_hash_map = HashMap::from([("a", 40), ("b", 50), ("c", 60)]);

        if let Err(_) = get_diff_positions_instructions(&first_hash_map, &second_hash_map) {
            panic!("Diffing hash maps with same keys should not return an error");
        }
    }

    #[test]
    fn get_diff_positions_instructions_returns_error_when_hash_maps_have_different_keys() {
        let first_hash_map = HashMap::from([("a", 0), ("b", 1), ("c", 2)]);
        let second_hash_map = HashMap::from([("d", 0), ("b", 2), ("c", 3)]);

        if let Ok(_) = get_diff_positions_instructions(&first_hash_map, &second_hash_map) {
            panic!("Diffing hash maps with different keys should return an error");
        }
    }

    #[test]
    fn get_diff_positions_instructions_returns_instructions_for_all_key_pairs() {
        let first_hash_map = HashMap::from([("a", 0), ("b", 10), ("c", 20)]);
        let second_hash_map = HashMap::from([("a", 0), ("b", 100), ("c", 200)]);

        match get_diff_positions_instructions(&first_hash_map, &second_hash_map) {
            Ok(diffs) => {
                first_hash_map.keys().for_each(|k| {
                    let diff = diffs.get(k).unwrap();

                    let original = first_hash_map.get(k).unwrap();
                    let new = second_hash_map.get(k).unwrap();

                    assert_eq!(diff, &DiffPositions(original, new));
                });
            }
            Err(diffs) => {
                panic!(
                    "No errors should have been returned, but {:?} key differences were reported",
                    diffs
                );
            }
        }
    }

    fn compare_hash_map_errors<T>(make_hash_map: T)
    where
        T: for<'a> Fn(
            &'a HashMap<&'static str, i32>,
            &'a HashMap<&'static str, i32>,
        ) -> Result<
            HashMap<&'static str, DiffPositions<&'a i32>>,
            HashMapDiffError<&'static str>,
        >,
    {
        let first_hash_map_pair = (
            HashMap::from([("a", 0), ("b", 1)]),
            HashMap::from([("c", 0), ("b", 2)]),
        );
        let second_hash_map_pair = (
            HashMap::from([("a", 0), ("b", 2)]),
            HashMap::from([("a", 1), ("b", 2), ("c", 3)]),
        );
        let third_hash_map_pair = (
            HashMap::from([("b", 0), ("c", 10)]),
            HashMap::from([("a", 0), ("c", 20)]),
        );

        let hash_map_pairs = [
            first_hash_map_pair,
            second_hash_map_pair,
            third_hash_map_pair,
        ];

        hash_map_pairs.iter().for_each(|(original, new)| {
            match (
                check_hash_map_key_diffs(original, new),
                make_hash_map(original, new),
            ) {
                (Err(first_diff), Err(second_diff)) => {
                    assert_eq!(first_diff, second_diff);
                }
                (Ok(_), Err(diffs)) => {
                    panic!("function returned error whilst check_hash_map_key_diffs did not. {:?} was the returned error", diffs);
                }
                (Err(diffs), Ok(_)) => {
                    panic!("function did not return an error whilst check_hash_map_key_diffs did. {:?} was the returnedd error", diffs);
                }
                (Ok(_), Ok(_)) => {
                    panic!(
                        "Both functions returned OK although errors were supposed to be returned"
                    );
                }
            }
        });
    }

    #[test]
    fn get_diff_positions_instructions_returns_check_hash_map_key_diffs_error() {
        compare_hash_map_errors(|original, new| {
            Ok(get_diff_positions_instructions(original, new)?)
        });
    }
}
