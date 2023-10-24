use std::{collections::HashMap, fmt::Display, hash::Hash};

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

#[cfg(test)]
mod tests {
    use super::{
        ComputePosition,
        get_compute_position_instructions
    };

    use std::collections::HashMap;

    #[test]
    fn get_compute_position_instructions_returns_all_given_values() {
        let first_hash_map: HashMap<&'static str, i32> = HashMap::from([]);
        let second_hash_map: HashMap<&'static str, i32> =
            HashMap::from([("a", 0), ("b", 2), ("c", 4)]);
        let third_hash_map: HashMap<&'static str, i32> = HashMap::from([("t", 10), ("u", 20)]);

        let hash_maps = vec![first_hash_map, second_hash_map, third_hash_map];

        for hash_map in hash_maps {
            let result = get_compute_position_instructions(&hash_map);

            hash_map.iter().for_each(|(key, original_value)| {
                let ComputePosition(value) = result.get(key).expect("Key {k} should be in results");

                assert_eq!(*value, original_value);
            });
        }
    }

}
