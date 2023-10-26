use std::{collections::HashMap, fmt::Display, hash::Hash};

use crate::{
    clear_style::get_clear_style_instructions, clear_style::ClearStyle,
    compute_position::get_compute_position_instructions, compute_position::ComputePosition,
    flip_positions::FlipPositions,
};

#[derive(Debug)]
pub(crate) struct FlipNodes<T, U>(HashMap<T, U>);

impl<T, U> Clone for FlipNodes<T, U>
where
    T: Hash + Eq + Clone + Display,
    U: Clone,
{
    fn clone(&self) -> Self {
        let FlipNodes(hash_map) = self;

        FlipNodes::new(hash_map.clone())
    }
}

impl<T, U> FlipNodes<T, U>
where
    T: Hash + Eq + Clone + Display,
    U: Clone,
{
    pub fn new(ids_to_nodes: HashMap<T, U>) -> Self {
        FlipNodes(ids_to_nodes)
    }

    pub fn nodes(&self) -> &HashMap<T, U> {
        let FlipNodes(nodes) = self;

        &nodes
    }

    pub fn take_nodes(self) -> HashMap<T, U> {
        let FlipNodes(nodes) = self;

        nodes
    }

    pub fn compute_positions<V>(
        self,
        resolver: impl Fn(&T, &U) -> Result<V, T>,
    ) -> Result<FlipPositions<T, U, V>, Vec<T>> {
        let cloned_nodes = self.clone();

        let compute_position_instructions = get_compute_position_instructions(cloned_nodes.nodes());

        let positions: HashMap<_, _> = compute_position_instructions
            .iter()
            .map(|(k, v)| {
                let ComputePosition(v) = v;

                ((*k).clone(), resolver(k, v))
            })
            .collect();

        let problematic_keys: Vec<_> = positions
            .iter()
            .filter(|(_, v)| v.is_err())
            .map(|(_, v)| match v {
                Ok(_) => panic!("Presence of error was previously asserted"),
                Err(e) => e,
            })
            .map(|v| v.clone())
            .collect();

        if problematic_keys.len() > 0 {
            return Err(problematic_keys);
        }

        let positions: Result<HashMap<_, _>, T> = positions
            .into_iter()
            .map(|(k, v)| Ok((k, v.map_err(|t| t.clone())?)))
            .collect();

        Ok(FlipPositions::new(
            self.take_nodes(),
            positions.map_err(|t| vec![t.clone()])?,
        ))
    }

    pub fn clear_styles(
        &self,
        resolver: impl for<'a> Fn(&'a T, &'a U) -> Result<(), T>,
    ) -> Result<(), Vec<T>> {
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
                Err(e) => e.clone(),
            })
            .collect();

        if problematic_keys.len() > 0 {
            Err(problematic_keys)
        } else {
            Ok(())
        }
    }
}

impl<T, U> PartialEq<FlipNodes<T, U>> for FlipNodes<T, U>
where
    T: Hash + Eq + Clone + Display,
    U: Eq + Clone,
{
    fn eq(&self, other: &FlipNodes<T, U>) -> bool {
        self.nodes() == other.nodes()
    }
}

#[cfg(test)]
mod tests {
    use super::FlipNodes;
    use std::collections::HashMap;

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
    fn flip_nodes_allows_clearing_of_styles() {
        let nodes = HashMap::from([("a", 1), ("b", 2), ("c", 3)]);

        let flip_transform_and_transition = FlipNodes::new(nodes);

        let _ = flip_transform_and_transition.clear_styles(|_, _| Ok(()));
    }
}
