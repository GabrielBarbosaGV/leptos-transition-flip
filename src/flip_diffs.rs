use std::{
    collections::HashMap,
    hash::Hash,
    fmt::Display
};

use crate::{
    flip_transform::FlipTransform,
    set_transform::{
        SetTransform,
        get_set_transform_instructions
    }
};

#[derive(Debug)]
pub(crate) struct FlipDiffs<T, U, V> {
    nodes: HashMap<T, U>,
    diffs: HashMap<T, V>,
}

impl<T, U, V> FlipDiffs<T, U, V>
where
    T: Hash + Eq + Clone + Display,
{
    pub fn new(nodes: HashMap<T, U>, diffs: HashMap<T, V>) -> Self {
        FlipDiffs { nodes, diffs }
    }

    pub fn nodes(&self) -> &HashMap<T, U> {
        let FlipDiffs { nodes, .. } = self;

        &nodes
    }

    pub fn take_nodes(self) -> HashMap<T, U> {
        let FlipDiffs { nodes, .. } = self;

        nodes
    }

    pub fn diffs(&self) -> &HashMap<T, V> {
        let FlipDiffs { diffs, .. } = self;

        &diffs
    }

    pub fn set_transforms(
        self,
        resolver: impl Fn(&T, &U, &V) -> Result<(), T>,
    ) -> Result<FlipTransform<T, U>, Vec<T>> {
        let set_transform_and_transition_instructions =
            get_set_transform_instructions(self.nodes());

        let mut problematic_keys = Vec::new();

        set_transform_and_transition_instructions
            .iter()
            .for_each(|(k, v)| {
                let SetTransform(v) = v;

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

        Ok(FlipTransform::new(self.take_nodes()))
    }
}
