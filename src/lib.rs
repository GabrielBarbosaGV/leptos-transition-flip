use leptos::NodeRef;
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    hash::Hash,
    ops::Deref,
};

use leptos::html::ElementDescriptor;
use web_sys::Element;

fn prepare_flip<'a, T, U, V>(
    ids_to_nodes: &'a HashMap<T, NodeRef<U>>,
    reflow_target: U,
    transition_style: &'static str,
) -> (impl FnOnce() -> Result<(), FlipError<T>> + 'a, impl FnOnce() -> Result<(), ClearError<T>> + 'a)
where
    T: Hash + Eq + Clone + Display,
    U: Deref<Target = V> + ElementDescriptor + Clone,
    V: Deref<Target = Element>,
{
    let do_flip = move || {
        let flip_nodes = BeginFlip::set_initial_nodes(ids_to_nodes);

        let flip_positions = flip_nodes.compute_positions(|k, v| match v.get() {
            Some(v) => {
                let bounding_client_rect = v.get_bounding_client_rect();

                let x = bounding_client_rect.x();
                let y = bounding_client_rect.y();

                Ok((x, y))
            }
            None => Err(k),
        }).map_err(|ts| FlipError::CouldNotGetHtmlElement(ts))?;

        flip(flip_positions, transition_style)
    };

    let clear = move || {
        let flip_nodes = BeginFlip::set_initial_nodes(ids_to_nodes);

        flip_nodes.clear_styles(|k, v| match v.get() {
            Some(html_element) => {
                html_element.clone().style("transform", "");
                html_element.clone().style("transition", "");

                Ok(())
            },
            None => Err(k.clone())
        }).map_err(|ts| ClearError::CouldNotGetHtmlElement(ts))?;

        Ok(())
    };

    (do_flip, clear)
}

#[derive(Debug)]
enum PrepareFlipError<T> {
    CouldNotGetHtmlElement(Vec<T>),
}

fn flip<'a, T, U, V>(
    flip_positions: FlipPositions<'a, T, NodeRef<U>, (f64, f64)>,
    transition_style: &'static str
) -> Result<(), FlipError<T>>
where
    T: Hash + Eq + Clone + Display,
    U: Deref<Target = V> + ElementDescriptor + Clone,
    V: Deref<Target = Element>,
{
    let nodes = flip_positions.nodes();

    let new_flip_nodes = BeginFlip::set_initial_nodes(nodes);

    let new_flip_positions = new_flip_nodes
        .compute_positions(|k, v| match v.get() {
            Some(v) => {
                let bounding_client_rect = v.get_bounding_client_rect();

                let x = bounding_client_rect.x();
                let y = bounding_client_rect.y();

                Ok((x, y))
            }
            None => Err(k),
        })
        .map_err(|e| FlipError::CouldNotGetHtmlElement(e))?;

    let flip_diffs = flip_positions
        .compute_diffs(new_flip_positions, |_, o, n| {
            let (original_x, original_y) = o;
            let (new_x, new_y) = n;

            let delta_x = original_x - new_x;
            let delta_y = original_y - new_y;

            (delta_x, delta_y)
        })
        .map_err(
            |HashMapDiffError {
                 present_in_original_but_not_new,
                 present_in_new_but_not_original,
             }| FlipError::HashMapDiffError {
                present_in_new_but_not_original,
                present_in_original_but_not_new,
            },
        )?;

    let flip_transform_and_transition =
        flip_diffs.set_transforms_and_transitions(|t, node, (delta_x, delta_y)| match node.get() {
            Some(html_element) => {
                html_element.clone().style("transform", &format!("translate({delta_x}px, {delta_y}px)"));
                html_element.clone().style("transition", transition_style);

                Ok(())
            },
            None => Err(t.clone())
        }).map_err(|ts| FlipError::CouldNotGetHtmlElement(ts))?;

    Ok(())
}

#[derive(Debug)]
enum FlipError<T> {
    CouldNotGetHtmlElement(Vec<T>),
    HashMapDiffError {
        present_in_new_but_not_original: HashSet<T>,
        present_in_original_but_not_new: HashSet<T>,
    },
}

enum ClearError<T> {
    CouldNotGetHtmlElement(Vec<T>)
}

#[derive(Debug)]
struct BeginFlip;

impl BeginFlip {
    fn set_initial_nodes<T, U>(ids_to_nodes: &HashMap<T, U>) -> FlipNodes<T, U>
    where
        T: Hash + Eq + Clone + Display,
    {
        FlipNodes::new(&ids_to_nodes)
    }
}

#[derive(Debug)]
struct FlipNodes<'a, T, U>(&'a HashMap<T, U>);

impl<'a, T, U> FlipNodes<'a, T, U>
where
    T: Hash + Eq + Clone + Display,
{
    fn new(ids_to_nodes: &'a HashMap<T, U>) -> Self {
        FlipNodes(ids_to_nodes)
    }

    fn nodes(&self) -> &HashMap<T, U> {
        let FlipNodes(nodes) = self;

        &nodes
    }

    fn compute_positions<V>(
        &'a self,
        resolver: impl Fn(&'a T, &'a U) -> Result<V, &'a T>,
    ) -> Result<FlipPositions<'a, T, U, V>, Vec<T>> {
        let compute_position_instructions = get_compute_position_instructions(self.nodes());

        let nodes = self.nodes();

        let positions: HashMap<_, _> = compute_position_instructions
            .iter()
            .map(|(k, v)| {
                let ComputePosition(v) = v;

                ((*k).clone(), resolver(k, v))
            })
            .collect();

        let problematic_keys: Vec<_> = positions
            .iter()
            .filter(|(k, v)| v.is_err())
            .map(|(_, v)| match *v {
                Ok(_) => panic!("Presence of error was previously asserted"),
                Err(e) => e
            })
            .map(|v| v.clone())
            .collect();

        if problematic_keys.len() > 0 {
            return Err(problematic_keys);
        }

        let positions: Result<HashMap<_, _>, &T> =
            positions.into_iter().map(|(k, v)| Ok((k, v?))).collect();

        Ok(FlipPositions::new(
            nodes,
            positions.map_err(|t| vec![t.clone()])?,
        ))
    }

    fn clear_styles(&'a self, resolver: impl for<'b> Fn(&'b T, &'b U) -> Result<(), T>) -> Result<(), Vec<T>> {
        let clear_instructions = get_clear_style_instructions(self.nodes());

        let results: Vec<Result<(), T>> = clear_instructions
            .iter()
            .map(|(k, v)| {
                let ClearStyle(v) = v;

                Ok(resolver(k, v)?)
            })
            .collect();

        let problematic_keys: Vec<_> = results
            .iter()
            .filter(|r| r.is_err())
            .map(|r| match r {
                Ok(()) => panic!("Presence of error was previously asserted"),
                Err(e) => e.clone()
            })
            .collect();

        if problematic_keys.len() > 0 {
            Err(problematic_keys)
        } else {
            Ok(())
        }
    }
}

impl<'a, T, U> PartialEq<FlipNodes<'a, T, U>> for FlipNodes<'a, T, U>
where
    T: Hash + Eq + Clone + Display,
    U: Eq,
{
    fn eq(&self, other: &FlipNodes<T, U>) -> bool {
        self.nodes() == other.nodes()
    }
}

#[derive(Debug)]
struct FlipPositions<'a, T, U, V> {
    nodes: &'a HashMap<T, U>,
    positions: HashMap<T, V>,
}

impl<'a, T, U, V> FlipPositions<'a, T, U, V>
where
    T: Hash + Eq + Clone + Display,
{
    fn new(nodes: &'a HashMap<T, U>, positions: HashMap<T, V>) -> Self {
        FlipPositions { nodes, positions }
    }

    fn nodes(&self) -> &'a HashMap<T, U> {
        self.nodes
    }

    fn positions(&self) -> &HashMap<T, V> {
        &self.positions
    }

    fn compute_diffs<X>(
        &self,
        other: FlipPositions<'a, T, U, V>,
        resolver: impl Fn(&T, &V, &V) -> X,
    ) -> Result<FlipDiffs<'a, T, U, X>, HashMapDiffError<T>> {
        let diffs = get_diff_positions_instructions(self.positions(), other.positions())?
            .iter()
            .map(|(k, v)| {
                let DiffPositions(o, n) = v;

                ((*k).clone(), resolver(k, o, n))
            })
            .collect();

        Ok(FlipDiffs::new(self.nodes(), diffs))
    }
}

#[derive(Debug)]
struct FlipDiffs<'a, T, U, V> {
    nodes: &'a HashMap<T, U>,
    diffs: HashMap<T, V>,
}

impl<'a, T, U, V> FlipDiffs<'a, T, U, V>
where
    T: Hash + Eq + Clone + Display,
{
    fn new(nodes: &'a HashMap<T, U>, diffs: HashMap<T, V>) -> Self {
        FlipDiffs { nodes, diffs }
    }

    fn nodes(&self) -> &'a HashMap<T, U> {
        let FlipDiffs { nodes, .. } = self;

        &nodes
    }

    fn diffs(&self) -> &HashMap<T, V> {
        let FlipDiffs { diffs, .. } = self;

        &diffs
    }

    fn set_transforms_and_transitions(
        &self,
        resolver: impl Fn(&T, &U, &V) -> Result<(), T>,
    ) -> Result<FlipTransformAndTransition<'a, T, U>, Vec<T>> {
        let set_transform_and_transition_instructions =
            get_set_transform_and_transition_instructions(self.nodes());

        let mut problematic_keys = Vec::new();

        set_transform_and_transition_instructions
            .iter()
            .for_each(|(k, v)| {
                let SetTransformAndTransition(v) = v;

                match resolver(k, v, self.diffs().get(k).unwrap()) {
                    Ok(()) => {}
                    Err(t) => {
                        problematic_keys.push(t.clone());
                    }
                }
            });

        if problematic_keys.len() > 0 {
            return Err(problematic_keys);
        }

        Ok(FlipTransformAndTransition::new(self.nodes()))
    }
}

#[derive(Debug)]
struct FlipTransformAndTransition<'a, T, U> {
    nodes: &'a HashMap<T, U>,
}

impl<'a, T, U> FlipTransformAndTransition<'a, T, U>
where
    T: Hash + Eq + Clone + Display,
{
    fn new(nodes: &'a HashMap<T, U>) -> Self {
        FlipTransformAndTransition { nodes }
    }

    fn nodes(&self) -> &'a HashMap<T, U> {
        let FlipTransformAndTransition { nodes } = self;

        &nodes
    }
}

fn get_compute_position_instructions<T, U>(
    to_compute: &HashMap<T, U>,
) -> HashMap<&T, ComputePosition<&U>>
where
    T: Hash + Eq + Clone + Display,
{
    to_compute
        .iter()
        .map(|(k, v)| (k, ComputePosition(v)))
        .collect()
}

struct ComputePosition<T>(T);

fn get_diff_positions_instructions<'a, T, U>(
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
struct DiffPositions<T>(T, T);

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

#[derive(Debug)]
struct HashMapDiffError<T> {
    present_in_original_but_not_new: HashSet<T>,
    present_in_new_but_not_original: HashSet<T>,
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

fn check_hash_map_key_diffs<'a, T, U, V>(
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

fn get_set_transform_and_transition_instructions<T, U>(
    to_set: &HashMap<T, U>,
) -> HashMap<T, SetTransformAndTransition<&U>>
where
    T: Hash + Eq + Clone + Display,
{
    to_set
        .iter()
        .map(|(k, v)| (k.clone(), SetTransformAndTransition(v)))
        .collect()
}

#[derive(Debug)]
struct SetTransformAndTransition<T>(T);

impl<T> PartialEq<SetTransformAndTransition<T>> for SetTransformAndTransition<T>
where
    T: Eq,
{
    fn eq(&self, other: &SetTransformAndTransition<T>) -> bool {
        let SetTransformAndTransition(original) = self;
        let SetTransformAndTransition(new) = other;

        original == new
    }
}

fn get_remove_transform_instructions<T, U>(
    to_remove: &HashMap<T, U>,
) -> HashMap<T, RemoveTransform<&U>>
where
    T: Hash + Eq + Clone + Display,
{
    to_remove
        .iter()
        .map(|(k, v)| (k.clone(), RemoveTransform(v)))
        .collect()
}

#[derive(Debug)]
struct RemoveTransform<T>(T);

impl<T> PartialEq<RemoveTransform<T>> for RemoveTransform<T>
where
    T: Eq,
{
    fn eq(&self, other: &RemoveTransform<T>) -> bool {
        let RemoveTransform(original) = self;
        let RemoveTransform(new) = other;

        original == new
    }
}

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
        check_hash_map_key_diffs, get_clear_style_instructions, get_compute_position_instructions,
        get_diff_positions_instructions, get_remove_transform_instructions,
        get_set_transform_and_transition_instructions, BeginFlip, ClearStyle, ComputePosition,
        DiffPositions, FlipDiffs, FlipNodes, HashMapDiffError,
        RemoveTransform, SetTransformAndTransition,
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
            get_set_transform_and_transition_instructions(&hash_map);

        match check_hash_map_key_diffs(&hash_map, &set_transform_and_transition_instructions) {
            Ok(()) => {
                hash_map.keys().for_each(|k| {
                    assert_eq!(
                        set_transform_and_transition_instructions.get(k).unwrap(),
                        &SetTransformAndTransition(hash_map.get(k).unwrap())
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

        let remove_transform_instructions = get_remove_transform_instructions(&hash_map);

        match check_hash_map_key_diffs(&hash_map, &remove_transform_instructions) {
            Ok(()) => {
                hash_map.keys().for_each(|k| {
                    assert_eq!(
                        &RemoveTransform(hash_map.get(k).unwrap()),
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

        let flip_nodes = BeginFlip::set_initial_nodes(&hash_map);

        assert_eq!(flip_nodes.nodes(), &hash_map);
    }

    #[test]
    fn flip_nodes_computes_positions_with_resolver() {
        let nodes = HashMap::from([("a", 1), ("b", 2), ("c", 3)]);
        let compute_position_instructions = get_compute_position_instructions(&nodes);

        let resolver = |_: &_, p: &_| Ok(p * 10);

        let flip_nodes = FlipNodes::new(&nodes);

        let flip_positions = flip_nodes
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

        let original_begin_flip = BeginFlip::set_initial_nodes(&nodes);

        let original_flip_positions = original_begin_flip
            .compute_positions(|_, p| Ok(*p * 10))
            .expect("All elements should be had after setting initial nodes for original");

        let new_begin_flip = BeginFlip::set_initial_nodes(&nodes);

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

        let flip_diffs = FlipDiffs::new(&nodes, diffs);

        let flip_transform_and_transition = flip_diffs
            .set_transforms_and_transitions(|_, node, diff| Ok(()))
            .expect("Setting transforms and transitions should not fail");

        assert_eq!(&nodes, flip_transform_and_transition.nodes());
    }

    #[test]
    fn flip_nodes_allows_clearing_of_styles() {
        let nodes = HashMap::from([("a", 1), ("b", 2), ("c", 3)]);

        let flip_transform_and_transition = FlipNodes::new(&nodes);

        let _ = flip_transform_and_transition.clear_styles(|k, v| Ok(()));
    }
}
