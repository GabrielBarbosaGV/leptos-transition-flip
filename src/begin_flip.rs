use crate::flip_nodes::FlipNodes;

use std::{collections::HashMap, fmt::Display, hash::Hash};

#[derive(Debug)]
pub(crate) struct BeginFlip;

impl BeginFlip {
    pub fn set_initial_nodes<T, U>(ids_to_nodes: HashMap<T, U>) -> FlipNodes<T, U>
    where
        T: Hash + Eq + Clone + Display,
        U: Clone,
    {
        FlipNodes::new(ids_to_nodes)
    }
}

#[cfg(test)]
mod tests {
    use super::BeginFlip;
    use std::collections::HashMap;

    #[test]
    fn begin_flip_takes_ids_to_nodes() {
        let hash_map = HashMap::from([("a", 0), ("b", 1), ("b", 2)]);

        let flip_nodes = BeginFlip::set_initial_nodes(hash_map.clone());

        assert_eq!(flip_nodes.nodes(), &hash_map);
    }
}
