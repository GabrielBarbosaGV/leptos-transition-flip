use std::{collections::HashMap, fmt::Display, hash::Hash};

use crate::remove_transform_and_set_transition::{
    get_remove_transform_and_set_transition_instructions, RemoveTransformAndSetTransition,
};

#[derive(Debug)]
pub(crate) struct FlipTransform<T, U> {
    nodes: HashMap<T, U>,
}

impl<T, U> FlipTransform<T, U>
where
    T: Hash + Eq + Clone + Display,
{
    pub fn new(nodes: HashMap<T, U>) -> Self {
        FlipTransform { nodes }
    }

    pub fn nodes(&self) -> &HashMap<T, U> {
        let FlipTransform { nodes } = self;

        &nodes
    }

    pub fn remove_transform_and_set_transition(
        self,
        resolver: impl Fn(&T, &U) -> Result<(), T>,
    ) -> Result<(), Vec<T>> {
        let remove_transform_and_set_transition_instructions =
            get_remove_transform_and_set_transition_instructions(self.nodes());

        let mut problematic_keys = Vec::new();

        remove_transform_and_set_transition_instructions
            .iter()
            .for_each(|(k, v)| {
                let RemoveTransformAndSetTransition(v) = v;

                match resolver(k, v) {
                    Ok(_) => (),
                    Err(t) => problematic_keys.push(t.clone()),
                }
            });

        if problematic_keys.len() > 0 {
            return Err(problematic_keys);
        }

        Ok(())
    }
}
