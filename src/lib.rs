//! Allows FLIP transitions between element positions by using provided NodeRefs from the Leptos
//! crate.

mod prepare_flip;
mod flip;
mod begin_flip;
mod flip_nodes;
mod flip_positions;
mod flip_diffs;
mod flip_transform;
mod compute_position;
mod diff_positions;
mod hash_map_diff_error;
mod set_transform;
mod remove_transform_and_set_transition;

use std::{
    collections::HashMap,
    fmt::Display,
    hash::Hash,
    cmp::{Eq, PartialEq},
};

fn get_clear_style_instructions<T, U>(to_clear: &HashMap<T, U>) -> HashMap<T, ClearStyle<&U>>
where
    T: Hash + Eq + Clone + Display,
{
    to_clear
        .iter()
        .map(|(k, v)| (k.clone(), ClearStyle(v)))
        .collect()
}

#[derive(Debug)]
struct ClearStyle<T>(T);

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
    use crate::{
        hash_map_diff_error::check_hash_map_key_diffs, get_clear_style_instructions, compute_position::get_compute_position_instructions,
        diff_positions::get_diff_positions_instructions, remove_transform_and_set_transition::get_remove_transform_and_set_transition_instructions,
        set_transform::get_set_transform_instructions, begin_flip::BeginFlip, ClearStyle, compute_position::ComputePosition,
        diff_positions::DiffPositions, flip_diffs::FlipDiffs, flip_nodes::FlipNodes, remove_transform_and_set_transition::RemoveTransformAndSetTransition, hash_map_diff_error::HashMapDiffError,
        set_transform::SetTransform,
    };
    use std::collections::{HashMap, HashSet};

    #[test]
    fn get_compute_position_instructions_returns_all_given_values() {
        let first_hash_map: HashMap<&'static str, i32> = HashMap::from([]);
        let second_hash_map: HashMap<&'static str, i32> =
            HashMap::from([("a", 0), ("b", 2), ("c", 4)]);
        let third_hash_map: HashMap<&'static str, i32> = HashMap::from([("t", 10), ("u", 20)]);

        let hash_maps = vec![first_hash_map, second_hash_map, third_hash_map];

        for hash_map in hash_maps {
            let result = get_compute_position_instructions(&hash_map);

            hash_map.iter().for_each(|(key, original_value)| {
                let ComputePosition(value) = result.get(key).expect("Key {k} should be in results");

                assert_eq!(*value, original_value);
            });
        }
    }

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

    #[test]
    fn get_set_transform_and_transition_instructions_returns_hash_map_with_all_given_keys() {
        let hash_map = HashMap::from([("a", 1), ("b", 2), ("c", 3)]);

        let set_transform_and_transition_instructions =
            get_set_transform_instructions(&hash_map);

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

    #[test]
    fn get_remove_transform_instructions_returns_hash_map_with_all_given_keys() {
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

    #[test]
    fn begin_flip_takes_ids_to_nodes() {
        let hash_map = HashMap::from([("a", 0), ("b", 1), ("b", 2)]);

        let flip_nodes = BeginFlip::set_initial_nodes(hash_map.clone());

        assert_eq!(flip_nodes.nodes(), &hash_map);
    }

    #[test]
    fn flip_nodes_computes_positions_with_resolver() {
        let nodes = HashMap::from([("a", 1), ("b", 2), ("c", 3)]);

        let resolver = |_: &_, p: &_| Ok(p * 10);

        let flip_nodes = FlipNodes::new(nodes);

        let cloned_nodes = flip_nodes.clone();

        let flip_positions = cloned_nodes
            .compute_positions(resolver)
            .expect("Computing positions should not return an error");

        flip_positions.positions().keys().for_each(|k| {
            assert_eq!(
                *flip_positions.positions().get(k).unwrap(),
                flip_nodes.nodes().get(k).unwrap() * 10
            );
        });
    }

    #[test]
    fn compute_diffs_return_flip_diffs() {
        let nodes = HashMap::from([("a", 1), ("b", 2), ("c", 3)]);

        let original_begin_flip = BeginFlip::set_initial_nodes(nodes.clone());

        let original_flip_positions = original_begin_flip
            .compute_positions(|_, p| Ok(*p * 10))
            .expect("All elements should be had after setting initial nodes for original");

        let new_begin_flip = BeginFlip::set_initial_nodes(nodes.clone());

        let new_flip_positions = new_begin_flip
            .compute_positions(|_, p| Ok(*p * 20))
            .expect("All elements should be had after setting initial nodes for new");

        let flip_diffs =
            original_flip_positions.compute_diffs(new_flip_positions, |_, old, new| *old - *new);

        let expected = HashMap::from([("a", -10), ("b", -20), ("c", -30)]);

        nodes.keys().for_each(|k| {
            let actual_diff = flip_diffs.as_ref().unwrap().diffs().get(k).unwrap();
            let expected_diff = expected.get(k).unwrap();

            assert_eq!(actual_diff, expected_diff);
        });
    }

    #[test]
    fn set_transforms_and_transitions_returns_flip_transform_and_position() {
        let nodes = HashMap::from([("a", 1), ("b", 2), ("c", 3)]);
        let diffs = HashMap::from([("a", 10), ("b", 20), ("c", 30)]);

        let cloned_nodes = nodes.clone();

        let flip_diffs = FlipDiffs::new(cloned_nodes, diffs);

        let flip_transform_and_transition = flip_diffs
            .set_transforms(|_, _, _| Ok(()))
            .expect("Setting transforms and transitions should not fail");

        assert_eq!(&nodes, flip_transform_and_transition.nodes());
    }

    #[test]
    fn flip_nodes_allows_clearing_of_styles() {
        let nodes = HashMap::from([("a", 1), ("b", 2), ("c", 3)]);

        let flip_transform_and_transition = FlipNodes::new(nodes);

        let _ = flip_transform_and_transition.clear_styles(|_, _| Ok(()));
    }
}
