use std::{
    fmt::Display,
    collections::HashMap,
    hash::Hash
};

pub(crate) fn get_set_transform_instructions<T, U>(
    to_set: &HashMap<T, U>,
) -> HashMap<T, SetTransform<&U>>
where
    T: Hash + Eq + Clone + Display,
{
    to_set
        .iter()
        .map(|(k, v)| (k.clone(), SetTransform(v)))
        .collect()
}

#[derive(Debug)]
pub(crate) struct SetTransform<T>(pub T);

impl<T> PartialEq<SetTransform<T>> for SetTransform<T>
where
    T: Eq,
{
    fn eq(&self, other: &SetTransform<T>) -> bool {
        let SetTransform(original) = self;
        let SetTransform(new) = other;

        original == new
    }
}

