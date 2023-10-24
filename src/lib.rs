//! Allows FLIP transitions between element positions by using provided NodeRefs from the Leptos
//! crate.

mod begin_flip;
mod clear_style;
mod compute_position;
mod diff_positions;
mod flip;
mod flip_diffs;
mod flip_nodes;
mod flip_positions;
mod flip_transform;
mod hash_map_diff_error;
mod prepare_flip;
mod remove_transform_and_set_transition;
mod set_transform;

#[cfg(test)]
mod tests {
    use crate::{
        begin_flip::BeginFlip,
        clear_style::{get_clear_style_instructions, ClearStyle},
        compute_position::{get_compute_position_instructions, ComputePosition},
        diff_positions::{get_diff_positions_instructions, DiffPositions},
        flip_diffs::FlipDiffs,
        flip_nodes::FlipNodes,
        hash_map_diff_error::{check_hash_map_key_diffs, HashMapDiffError},
        remove_transform_and_set_transition::{
            get_remove_transform_and_set_transition_instructions, RemoveTransformAndSetTransition,
        },
        set_transform::{get_set_transform_instructions, SetTransform},
    };

    use std::collections::{HashMap, HashSet};

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
