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
