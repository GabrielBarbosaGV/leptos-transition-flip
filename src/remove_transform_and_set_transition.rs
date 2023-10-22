use std::{
    collections::HashMap,
    fmt::Display,
    hash::Hash
};

pub(crate) fn get_remove_transform_and_set_transition_instructions<T, U>(
    to_remove: &HashMap<T, U>,
) -> HashMap<T, RemoveTransformAndSetTransition<&U>>
where
    T: Hash + Eq + Clone + Display,
{
    to_remove
        .iter()
        .map(|(k, v)| (k.clone(), RemoveTransformAndSetTransition(v)))
        .collect()
}

#[derive(Debug)]
pub(crate) struct RemoveTransformAndSetTransition<T>(pub T);

impl<T, U> RemoveTransformAndSetTransition<HashMap<T, U>>
where
    T: Hash + Eq + Clone + Display,
{
    pub fn new(nodes: HashMap<T, U>) -> Self {
        RemoveTransformAndSetTransition(nodes)
    }
}

impl<T> PartialEq<RemoveTransformAndSetTransition<T>> for RemoveTransformAndSetTransition<T>
where
    T: Eq,
{
    fn eq(&self, other: &RemoveTransformAndSetTransition<T>) -> bool {
        let RemoveTransformAndSetTransition(original) = self;
        let RemoveTransformAndSetTransition(new) = other;

        original == new
    }
}

