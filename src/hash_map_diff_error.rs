use std::{
    hash::Hash,
    collections::{
        HashSet,
        HashMap
    },
    fmt::Display
};

#[derive(Debug)]
pub(crate) struct HashMapDiffError<T> {
    pub present_in_original_but_not_new: HashSet<T>,
    pub present_in_new_but_not_original: HashSet<T>,
}

impl<T> PartialEq for HashMapDiffError<T>
where
    T: Eq + Hash,
{
    fn eq(&self, other: &Self) -> bool {
        let HashMapDiffError {
            present_in_original_but_not_new: first_original,
            present_in_new_but_not_original: second_original,
        } = self;
        let HashMapDiffError {
            present_in_original_but_not_new: first_new,
            present_in_new_but_not_original: second_new,
        } = other;

        first_original == first_new && second_original == second_new
    }
}

pub(crate) fn check_hash_map_key_diffs<'a, T, U, V>(
    original: &'a HashMap<T, U>,
    new: &'a HashMap<T, V>,
) -> Result<(), HashMapDiffError<T>>
where
    T: Hash + Eq + Clone + Display,
{
    let original_keys: HashSet<_> = original.keys().map(|t| t.clone()).collect();
    let new_keys: HashSet<_> = new.keys().map(|t| t.clone()).collect();

    if original_keys == new_keys {
        Ok(())
    } else {
        let present_in_original_but_not_new = original_keys
            .difference(&new_keys)
            .map(|t| t.clone())
            .collect();
        let present_in_new_but_not_original = new_keys
            .difference(&original_keys)
            .map(|t| t.clone())
            .collect();

        Err(HashMapDiffError {
            present_in_original_but_not_new,
            present_in_new_but_not_original,
        })
    }
}
