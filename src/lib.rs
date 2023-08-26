use std::{collections::HashMap, hash::Hash};

#[cfg(test)]
use proptest::proptest;

/// Used to map arbitrary keys to values.
///
/// The intent of this trait is to be able to map identifiers to each eventual NodeRef,
/// as, at first, they must be accessed to obtain their positions, and these positions
/// have to be updated later. To be able to know which NodeRef must be updated with
/// a given new style, lookup is done through this mapping.
///
/// `mapping.get(t)` is used to retrieve an item from the mapping by the key "t".
///
/// `mapping.insert(t, u)` is used to associate a key "t" with a value "u".
///
/// `mapping.keys()` returns all keys. This is necessary to make sure every NodeRef
/// gets its style set.
pub trait Mapping<T, U> {
    fn get(&self, t: &T) -> Option<&U>;

    fn insert(&mut self, t: T, u: U);

    fn keys(&self) -> Vec<T>;
}

impl<T, U> Mapping<T, U> for HashMap<T, U> where T: Hash + Eq + Clone + ToOwned<Owned = T> {
    fn get(&self, t: &T) -> Option<&U> {
        self.get(t)
    }

    fn insert(&mut self, t: T, u: U) {
        self.insert(t, u);
    }

    fn keys(&self) -> Vec<T> {
        self
            .keys()
            .into_iter()
            .map(|k| k.to_owned())
            .collect()
    }
}



#[cfg(test)]
proptest! {

}
