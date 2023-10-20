use std::{
    collections::HashMap,
    fmt::Display,
    hash::Hash
};

use crate::{
    flip_diffs::FlipDiffs,
    DiffPositions,
    get_diff_positions_instructions,
    HashMapDiffError
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

