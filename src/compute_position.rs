use std::{
    collections::HashMap,
    fmt::Display,
    hash::Hash
};

pub(crate) fn get_compute_position_instructions<T, U>(
    to_compute: &HashMap<T, U>,
) -> HashMap<&T, ComputePosition<&U>>
where
    T: Hash + Eq + Clone + Display,
{
    to_compute
        .iter()
        .map(|(k, v)| (k, ComputePosition(v)))
        .collect()
}

pub(crate) struct ComputePosition<T>(pub T);
