use leptos::{html::ElementDescriptor, *};
use std::{collections::HashMap, fmt::Display, hash::Hash};

enum Instruction<'a, T> {
    ComputePosition(&'a T),
    DiffPositions(&'a T, &'a T),
}

impl<T> Display for Instruction<'_, T>
where
    T: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ComputePosition(t) => write!(f, "Instruction::ComputePosition({})", t),
            Self::DiffPositions(old, new) => write!(f, "Instruction::DiffPositions({}, {})", old, new),
            _ => write!(f, "Not implemented"),
        }
    }
}

fn get_compute_position_instructions<T, V>(
    ids_to_values: &HashMap<T, V>,
) -> HashMap<T, Instruction<'_, V>>
where
    T: Hash + Eq + Clone + Display,
{
    ids_to_values
        .iter()
        .map(|(key, value)| (key.clone(), Instruction::ComputePosition(value)))
        .collect()
}

fn get_diff_positions_instructions<'a, T, V>(
    original_ids_to_positions: &'a HashMap<T, V>,
    new_ids_to_positions: &'a HashMap<T, V>,
) -> Result<HashMap<&'a T, Instruction<'a, V>>, Vec<&'a T>>
where T: Hash + Eq + Display + Clone
{
    let mut problematic_keys = vec![];

    let diff_positions_instructions = original_ids_to_positions.keys().map(|k| {
        problematic_keys.push(k);

        let old_position = original_ids_to_positions.get(k)?;
        let new_position = new_ids_to_positions.get(k)?;

        problematic_keys.pop();

        return Some((k, Instruction::DiffPositions(old_position, new_position)));
    }).collect::<Option<HashMap<&'a T, Instruction<'a, V>>>>();

    match diff_positions_instructions {
        Some(diff_positions_instructions) => Ok(diff_positions_instructions),
        None => Err(problematic_keys)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::get_compute_position_instructions;
    use crate::get_diff_positions_instructions;
    use crate::Instruction;

    #[test]
    fn get_compute_position_instructions_for_hash_map_returns_new_hash_map_that_has_all_given_items(
    ) {
        let ids_to_values_vec = vec![
            HashMap::from([("a", 0), ("b", 1), ("c", 2)]),
            HashMap::from([]),
        ];

        for ids_to_values in ids_to_values_vec {
            let instructions = get_compute_position_instructions(&ids_to_values);

            ids_to_values.iter().for_each(|(key, value)| {
                let instruction = instructions.get(key).expect("Was unable to obtain instruction for {key}");

                if let Instruction::ComputePosition(instruction_value) = instruction {
                    assert_eq!(instruction_value, &value);
                } else {
                    panic!("Only position computing instructions were expected, but, for key {key}, instruction was {instruction}");
                }
            });
        }
    }

    #[test]
    fn get_diff_position_instructions_for_hash_maps_return_new_hash_map_that_has_all_given_items() {
        let old_positions = HashMap::from([("a", 0), ("b", 1), ("c", 2)]);
        let new_positions = HashMap::from([("a", 1), ("b", 2), ("c", 3)]);

        let instructions = get_diff_positions_instructions(&old_positions, &new_positions).expect("Was unable to get instructions");

        instructions.iter().for_each(|(k, v)| {
            let instruction = instructions.get(*k).expect("Was unable to obtain instruction for {k}");

            if let Instruction::DiffPositions(old, new) = instruction {
                assert_eq!(old_positions.get(*k).expect("Was unable to get old position for {k}"), *old);
                assert_eq!(new_positions.get(*k).expect("Was unable to get new position for {k}"), *new);
            } else {
                panic!("Only position diffing instructions were expected, but, for key {k}, instruction was {instruction}");
            }
        });


        let old_positions = HashMap::from([("a", 0), ("b", 1)]);
        let new_positions = HashMap::from([("a", 0)]);

        if let Err(ks) = get_diff_positions_instructions(&old_positions, &new_positions) {
            let mut contains_b = false;

            ks.into_iter().for_each(|s| {
                if *s.to_string() == "b".to_string() {
                    contains_b = true;
                }
            });

            assert!(contains_b);
        } else {
            panic!("Method call should have returned and error");
        }
    }
}
