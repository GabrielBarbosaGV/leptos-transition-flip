use std::{collections::HashMap, fmt::Display, hash::Hash};

use crate::{
    diff_positions::get_diff_positions_instructions, diff_positions::DiffPositions,
    flip_diffs::FlipDiffs, hash_map_diff_error::HashMapDiffError,
};

#[derive(Debug, Clone)]
pub(crate) struct FlipPositions<T, U, V> {
    nodes: HashMap<T, U>,
    positions: HashMap<T, V>,
}

impl<T, U, V> FlipPositions<T, U, V>
where
    T: Hash + Eq + Clone + Display,
{
    pub fn new(nodes: HashMap<T, U>, positions: HashMap<T, V>) -> Self {
        FlipPositions { nodes, positions }
    }

    pub fn nodes(&self) -> &HashMap<T, U> {
        &self.nodes
    }

    pub fn take_nodes(self) -> HashMap<T, U> {
        self.nodes
    }

    pub fn positions(&self) -> &HashMap<T, V> {
        &self.positions
    }

    pub fn compute_diffs<X>(
        self,
        other: FlipPositions<T, U, V>,
        resolver: impl Fn(&T, &V, &V) -> X,
    ) -> Result<FlipDiffs<T, U, X>, HashMapDiffError<T>> {
        let diffs = get_diff_positions_instructions(self.positions(), other.positions())?
            .iter()
            .map(|(k, v)| {
                let DiffPositions(o, n) = v;

                ((*k).clone(), resolver(k, o, n))
            })
            .collect();

        Ok(FlipDiffs::new(self.take_nodes(), diffs))
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use crate::begin_flip::BeginFlip;

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
}
