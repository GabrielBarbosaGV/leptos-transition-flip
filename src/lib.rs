//! Allows FLIP transitions between element positions by using provided NodeRefs from the Leptos
//! crate.

use leptos::{leptos_dom::console_log, NodeRef};
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    hash::Hash,
    ops::Deref,
    cmp::{Eq, PartialEq},
};

use leptos::html::ElementDescriptor;
use web_sys::HtmlElement;

/// Main function of this crate. Returns a [`Result`] whose Ok is a pair of closures, `(flip,
/// clear)`, which performs the transition when an element's position is changed, and clears the
/// styles of the transitioned elements, respectively. The Err variant contains a
/// [`PrepareFlipError`], whose single variant is a `CouldNotGetHtmlElement`, which itself
/// contains a [`Vec`] with all IDs for which getting an element was not
/// possible. The reason why [`prepare_flip`] has to calculate the positions of the elements is to
/// store them for later comparison with the new positions, and calculation of the element's
/// offset for the FLIP transition.
///
/// Example:
///
/// ```ignore
/// // ...
///
/// let reflow_target = create_node_ref(cx);
///
/// let first_node_ref = create_node_ref(cx);
/// let second_node_ref = create_node_ref(cx);
/// let third_node_ref = create_node_ref(cx);
///
/// let ids_to_nodes = HashMap::from([
///     (1, first_node_ref),
///     (2, second_node_ref),
///     (3, third_node_ref)
/// ]);
///
/// let (flip, clear) = prepare_flip(
///     ids_to_nodes,
///     reflow_target,
///     "transform 0.6s".to_string()
/// ).map_err(|e| format!("Prepare flip failed with error: {e}"))?;
///
/// // Perform actions that will change the node refs' elements' positions
/// // ...
///
/// flip().map_err(|e| format!("Flip failed with error: {e}"))?;
///
/// // ...
///
/// set_timeout(|| match clear() {
///     Ok(()) => (),
///     Err(e) => console_log(&format!("An error occurred when attempting to clear the elements' styles: {e}"))
/// }, Duration::from_millis(600));
///
/// Ok(())
///
/// ```
pub fn prepare_flip<T, U, V>(
    ids_to_nodes: HashMap<T, NodeRef<U>>,
    reflow_target: NodeRef<U>,
    transition_style: String,
) -> Result<
    (
        impl FnOnce() -> Result<(), FlipError<T>>,
        impl FnOnce() -> Result<(), ClearError<T>>,
    ),
    PrepareFlipError<T>,
>
where
    T: Hash + Eq + Clone + Display,
    U: Deref<Target = V> + ElementDescriptor + Clone,
    V: Deref<Target = HtmlElement>,
{
    let flip_nodes = BeginFlip::set_initial_nodes(ids_to_nodes.clone());

    let flip_positions = flip_nodes
        .compute_positions(|k, v| match v.get() {
            Some(v) => {
                let bounding_client_rect = v.get_bounding_client_rect();

                let x = bounding_client_rect.x();
                let y = bounding_client_rect.y();

                Ok((x, y))
            }
            None => Err(k.clone()),
        })
        .map_err(|ts| PrepareFlipError::CouldNotGetHtmlElement(ts))?;

    let do_flip = move || flip(flip_positions, reflow_target, transition_style);

    let clear = move || {
        let flip_nodes = BeginFlip::set_initial_nodes(ids_to_nodes);

        flip_nodes
            .clear_styles(|k, v| match v.get() {
                Some(html_element) => {
                    html_element.clone().style("transition", "");

                    Ok(())
                }
                None => Err(k.clone()),
            })
            .map_err(|ts| ClearError::CouldNotGetHtmlElement(ts))?;

        Ok(())
    };

    Ok((do_flip, clear))
}

/// Might occur when attempting to prepare a FLIP. When trying to obtain the position for a given
/// element, if it is not possible to get it from the node reference, will return a vector of all
/// IDs for which this is the case.
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum PrepareFlipError<T> {
    CouldNotGetHtmlElement(Vec<T>),
}

impl<T> Display for PrepareFlipError<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CouldNotGetHtmlElement(ts) => {
                write!(
                    f,
                    "Could not get element(s) from node reference(s) associated with {}",
                    ts.iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
}

fn flip<T, U, V>(
    flip_positions: FlipPositions<T, NodeRef<U>, (f64, f64)>,
    reflow_target: NodeRef<U>,
    transition_style: String,
) -> Result<(), FlipError<T>>
where
    T: Hash + Eq + Clone + Display,
    U: Deref<Target = V> + ElementDescriptor + Clone,
    V: Deref<Target = HtmlElement>,
{
    let nodes = flip_positions.nodes().clone();

    let new_flip_nodes = BeginFlip::set_initial_nodes(nodes);

    let new_flip_positions = new_flip_nodes
        .compute_positions(|k, v| match v.get() {
            Some(v) => {
                let bounding_client_rect = v.get_bounding_client_rect();

                let x = bounding_client_rect.x();
                let y = bounding_client_rect.y();

                console_log(&format!("X is {x}, Y is {y}"));

                Ok((x, y))
            }
            None => Err(k.clone()),
        })
        .map_err(|e| FlipError::CouldNotGetHtmlElement(e))?;

    let flip_diffs = flip_positions
        .compute_diffs(new_flip_positions, |_, o, n| {
            let (original_x, original_y) = o;
            let (new_x, new_y) = n;

            console_log(&format!(
                "Original X is {original_x}, original Y is {original_y}"
            ));

            console_log(&format!("New X is {new_x}, new Y is {new_y}"));

            let delta_x = original_x - new_x;
            let delta_y = original_y - new_y;

            (delta_x, delta_y)
        })
        .map_err(
            |HashMapDiffError {
                 present_in_original_but_not_new,
                 present_in_new_but_not_original,
             }| FlipError::HashMapDiffError {
                present_in_new_but_not_original,
                present_in_original_but_not_new,
            },
        )?;

    let flip_transform = flip_diffs
        .set_transforms(|t, node, (delta_x, delta_y)| match node.get() {
            Some(html_element) => {
                html_element
                    .clone()
                    .style("transform", &format!("translate({delta_x}px, {delta_y}px)"));

                Ok(())
            }
            None => Err(t.clone()),
        })
        .map_err(|ts| FlipError::CouldNotGetHtmlElement(ts))?;

    match reflow_target.get() {
        Some(html_element) => Ok(html_element.clone().offset_width()),
        None => Err(FlipError::CouldNotGetReflowTarget),
    }?;

    let flip_remove_transform_and_set_transition = flip_transform
        .remove_transform_and_set_transition(|t, node| match node.get() {
            Some(html_element) => {
                html_element
                    .clone()
                    .style("transition", transition_style.clone());
                html_element.clone().style("transform", "");

                Ok(())
            }
            None => Err(t.clone()),
        });

    flip_remove_transform_and_set_transition.map_err(|ts| FlipError::CouldNotGetHtmlElement(ts))?;

    Ok(())
}

/// Errors that might occur when FLIPping elements. Its variants and respective purposes are as
/// follows:
///
/// `CouldNotGetHtmlElement(Vec<T>)`: contains all IDs for which getting an HTML element was not
/// possible.
///
/// `CouldNotGetReflowTarget`: means that obtaining the HTML element from the reflow target was not
/// possible.
///
/// `HashMapDiffError {
///     present_in_original_but_not_new,
///     present_in_new_but_not_original
/// }`: might occur when attempting to obtain the offset of the original positions to the new ones.
/// The `present_in_original_but_not_new` contains elements which had IDs in the original HashMap,
/// but not the new, and its converse is the `present_in_new_but_not_original`.
#[derive(Debug, Clone)]
pub enum FlipError<T> {
    CouldNotGetHtmlElement(Vec<T>),
    CouldNotGetReflowTarget,
    HashMapDiffError {
        present_in_new_but_not_original: HashSet<T>,
        present_in_original_but_not_new: HashSet<T>,
    },
}

impl<T> PartialEq for FlipError<T> where T: Eq + Hash {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                FlipError::HashMapDiffError {
                    present_in_new_but_not_original: first_original,
                    present_in_original_but_not_new: second_original
                },

                FlipError::HashMapDiffError {
                    present_in_new_but_not_original: first_new,
                    present_in_original_but_not_new: second_new
                }
            ) => first_original == first_new && second_original == second_new,

            (a, b) => a == b
        }
    }
}

impl<T> Eq for FlipError<T> where T: Eq + Hash {}

impl<T> Display for FlipError<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CouldNotGetReflowTarget => {
                write!(f, "Could not get HTML element from given reflow target")
            }
            Self::CouldNotGetHtmlElement(ts) => {
                write!(
                    f,
                    "Could not get element(s) from node references(s) associated with {}",
                    ts.iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Self::HashMapDiffError {
                present_in_new_but_not_original,
                present_in_original_but_not_new,
            } => {
                write!(
                    f,
                    "An error occurred when trying to compute the differences in \
                    position between the original elements and the old elements. \
                    The elements present in the first hash map but not the second \
                    are {}, while the elements present in the second hash map but \
                    not the first are {}",
                    present_in_original_but_not_new
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<_>>()
                        .join(", "),
                    present_in_new_but_not_original
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
}

/// Might occur when clearing the styles of elements from the given node references. The single
/// variant CouldNotGetHtmlElement(`Vec<T>`) contains all IDs for which elements could not be
/// obtained from the given node references.
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ClearError<T> {
    CouldNotGetHtmlElement(Vec<T>),
}

impl<T> Display for ClearError<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CouldNotGetHtmlElement(ts) => {
                write!(
                    f,
                    "Could not get element(s) for node reference(s) associated with {}",
                    ts.iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
}

#[derive(Debug)]
struct BeginFlip;

impl BeginFlip {
    fn set_initial_nodes<T, U>(ids_to_nodes: HashMap<T, U>) -> FlipNodes<T, U>
    where
        T: Hash + Eq + Clone + Display,
        U: Clone,
    {
        FlipNodes::new(ids_to_nodes)
    }
}

#[derive(Debug)]
struct FlipNodes<T, U>(HashMap<T, U>);

impl<T, U> Clone for FlipNodes<T, U>
where
    T: Hash + Eq + Clone + Display,
    U: Clone,
{
    fn clone(&self) -> Self {
        let FlipNodes(hash_map) = self;

        FlipNodes::new(hash_map.clone())
    }
}

impl<T, U> FlipNodes<T, U>
where
    T: Hash + Eq + Clone + Display,
    U: Clone,
{
    fn new(ids_to_nodes: HashMap<T, U>) -> Self {
        FlipNodes(ids_to_nodes)
    }

    fn nodes(&self) -> &HashMap<T, U> {
        let FlipNodes(nodes) = self;

        &nodes
    }

    fn take_nodes(self) -> HashMap<T, U> {
        let FlipNodes(nodes) = self;

        nodes
    }

    fn compute_positions<V>(
        self,
        resolver: impl Fn(&T, &U) -> Result<V, T>,
    ) -> Result<FlipPositions<T, U, V>, Vec<T>> {
        let cloned_nodes = self.clone();

        let compute_position_instructions = get_compute_position_instructions(cloned_nodes.nodes());

        let positions: HashMap<_, _> = compute_position_instructions
            .iter()
            .map(|(k, v)| {
                let ComputePosition(v) = v;

                ((*k).clone(), resolver(k, v))
            })
            .collect();

        let problematic_keys: Vec<_> = positions
            .iter()
            .filter(|(_, v)| v.is_err())
            .map(|(_, v)| match v {
                Ok(_) => panic!("Presence of error was previously asserted"),
                Err(e) => e,
            })
            .map(|v| v.clone())
            .collect();

        if problematic_keys.len() > 0 {
            return Err(problematic_keys);
        }

        let positions: Result<HashMap<_, _>, T> = positions
            .into_iter()
            .map(|(k, v)| Ok((k, v.map_err(|t| t.clone())?)))
            .collect();

        Ok(FlipPositions::new(
            self.take_nodes(),
            positions.map_err(|t| vec![t.clone()])?,
        ))
    }

    fn clear_styles(
        &self,
        resolver: impl for<'a> Fn(&'a T, &'a U) -> Result<(), T>,
    ) -> Result<(), Vec<T>> {
        let clear_instructions = get_clear_style_instructions(self.nodes());

        let results: Vec<Result<(), T>> = clear_instructions
            .iter()
            .map(|(k, v)| {
                let ClearStyle(v) = v;

                Ok(resolver(k, v)?)
            })
            .collect();

        let problematic_keys: Vec<_> = results
            .iter()
            .filter(|r| r.is_err())
            .map(|r| match r {
                Ok(()) => panic!("Presence of error was previously asserted"),
                Err(e) => e.clone(),
            })
            .collect();

        if problematic_keys.len() > 0 {
            Err(problematic_keys)
        } else {
            Ok(())
        }
    }
}

impl<T, U> PartialEq<FlipNodes<T, U>> for FlipNodes<T, U>
where
    T: Hash + Eq + Clone + Display,
    U: Eq + Clone,
{
    fn eq(&self, other: &FlipNodes<T, U>) -> bool {
        self.nodes() == other.nodes()
    }
}

#[derive(Debug, Clone)]
struct FlipPositions<T, U, V> {
    nodes: HashMap<T, U>,
    positions: HashMap<T, V>,
}

impl<T, U, V> FlipPositions<T, U, V>
where
    T: Hash + Eq + Clone + Display,
{
    fn new(nodes: HashMap<T, U>, positions: HashMap<T, V>) -> Self {
        FlipPositions { nodes, positions }
    }

    fn nodes(&self) -> &HashMap<T, U> {
        &self.nodes
    }

    fn take_nodes(self) -> HashMap<T, U> {
        self.nodes
    }

    fn positions(&self) -> &HashMap<T, V> {
        &self.positions
    }

    fn compute_diffs<X>(
        self,
        other: FlipPositions<T, U, V>,
        resolver: impl Fn(&T, &V, &V) -> X,
    ) -> Result<FlipDiffs<T, U, X>, HashMapDiffError<T>> {
        let diffs = get_diff_positions_instructions(self.positions(), other.positions())?
            .iter()
            .map(|(k, v)| {
                let DiffPositions(o, n) = v;

                ((*k).clone(), resolver(k, o, n))
            })
            .collect();

        Ok(FlipDiffs::new(self.take_nodes(), diffs))
    }
}

#[derive(Debug)]
struct FlipDiffs<T, U, V> {
    nodes: HashMap<T, U>,
    diffs: HashMap<T, V>,
}

impl<T, U, V> FlipDiffs<T, U, V>
where
    T: Hash + Eq + Clone + Display,
{
    fn new(nodes: HashMap<T, U>, diffs: HashMap<T, V>) -> Self {
        FlipDiffs { nodes, diffs }
    }

    fn nodes(&self) -> &HashMap<T, U> {
        let FlipDiffs { nodes, .. } = self;

        &nodes
    }

    fn take_nodes(self) -> HashMap<T, U> {
        let FlipDiffs { nodes, .. } = self;

        nodes
    }

    fn diffs(&self) -> &HashMap<T, V> {
        let FlipDiffs { diffs, .. } = self;

        &diffs
    }

    fn set_transforms(
        self,
        resolver: impl Fn(&T, &U, &V) -> Result<(), T>,
    ) -> Result<FlipTransform<T, U>, Vec<T>> {
        let set_transform_and_transition_instructions =
            get_set_transform_and_transition_instructions(self.nodes());

        let mut problematic_keys = Vec::new();

        set_transform_and_transition_instructions
            .iter()
            .for_each(|(k, v)| {
                let SetTransformAndTransition(v) = v;

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

#[derive(Debug)]
struct FlipTransform<T, U> {
    nodes: HashMap<T, U>,
}

impl<T, U> FlipTransform<T, U>
where
    T: Hash + Eq + Clone + Display,
{
    fn new(nodes: HashMap<T, U>) -> Self {
        FlipTransform { nodes }
    }

    fn nodes(&self) -> &HashMap<T, U> {
        let FlipTransform { nodes } = self;

        &nodes
    }

    fn take_nodes(self) -> HashMap<T, U> {
        let FlipTransform { nodes } = self;

        nodes
    }

    fn remove_transform_and_set_transition(
        self,
        resolver: impl Fn(&T, &U) -> Result<(), T>,
    ) -> Result<FlipRemoveTransformAndSetTransition<HashMap<T, U>>, Vec<T>> {
        let remove_transform_and_set_transition_instructions =
            get_remove_transform_and_set_transition_instructions(self.nodes());

        let mut problematic_keys = Vec::new();

        remove_transform_and_set_transition_instructions
            .iter()
            .for_each(|(k, v)| {
                let FlipRemoveTransformAndSetTransition(v) = v;

                match resolver(k, v) {
                    Ok(_) => (),
                    Err(t) => problematic_keys.push(t.clone()),
                }
            });

        if problematic_keys.len() > 0 {
            return Err(problematic_keys);
        }

        Ok(FlipRemoveTransformAndSetTransition::new(self.take_nodes()))
    }
}

fn get_compute_position_instructions<T, U>(
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

struct ComputePosition<T>(T);

fn get_diff_positions_instructions<'a, T, U>(
    original: &'a HashMap<T, U>,
    new: &'a HashMap<T, U>,
) -> Result<HashMap<T, DiffPositions<&'a U>>, HashMapDiffError<T>>
where
    T: Hash + Eq + Clone + Display,
{
    check_hash_map_key_diffs(&original, &new)?;

    Ok(original
        .keys()
        .map(|k| {
            let original = original.get(k).unwrap();
            let new = new.get(k).unwrap();

            (k.clone(), DiffPositions(original, new))
        })
        .collect())
}

#[derive(Debug)]
struct DiffPositions<T>(T, T);

impl<T> PartialEq<DiffPositions<&T>> for DiffPositions<T>
where
    T: Eq,
{
    fn eq(&self, other: &DiffPositions<&T>) -> bool {
        let DiffPositions(original, new) = self;
        let DiffPositions(original_ref, new_ref) = other;

        original == *original_ref && new == *new_ref
    }
}

impl<T> PartialEq<DiffPositions<T>> for DiffPositions<T>
where
    T: Eq,
{
    fn eq(&self, other: &DiffPositions<T>) -> bool {
        let DiffPositions(original, new) = self;
        let DiffPositions(second_original, second_new) = other;

        original == second_original && new == second_new
    }
}

#[derive(Debug)]
struct HashMapDiffError<T> {
    present_in_original_but_not_new: HashSet<T>,
    present_in_new_but_not_original: HashSet<T>,
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

fn check_hash_map_key_diffs<'a, T, U, V>(
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

fn get_set_transform_and_transition_instructions<T, U>(
    to_set: &HashMap<T, U>,
) -> HashMap<T, SetTransformAndTransition<&U>>
where
    T: Hash + Eq + Clone + Display,
{
    to_set
        .iter()
        .map(|(k, v)| (k.clone(), SetTransformAndTransition(v)))
        .collect()
}

#[derive(Debug)]
struct SetTransformAndTransition<T>(T);

impl<T> PartialEq<SetTransformAndTransition<T>> for SetTransformAndTransition<T>
where
    T: Eq,
{
    fn eq(&self, other: &SetTransformAndTransition<T>) -> bool {
        let SetTransformAndTransition(original) = self;
        let SetTransformAndTransition(new) = other;

        original == new
    }
}

fn get_remove_transform_and_set_transition_instructions<T, U>(
    to_remove: &HashMap<T, U>,
) -> HashMap<T, FlipRemoveTransformAndSetTransition<&U>>
where
    T: Hash + Eq + Clone + Display,
{
    to_remove
        .iter()
        .map(|(k, v)| (k.clone(), FlipRemoveTransformAndSetTransition(v)))
        .collect()
}

#[derive(Debug)]
struct FlipRemoveTransformAndSetTransition<T>(T);

impl<T, U> FlipRemoveTransformAndSetTransition<HashMap<T, U>>
where
    T: Hash + Eq + Clone + Display,
{
    fn new(nodes: HashMap<T, U>) -> Self {
        FlipRemoveTransformAndSetTransition(nodes)
    }
}

impl<T> PartialEq<FlipRemoveTransformAndSetTransition<T>> for FlipRemoveTransformAndSetTransition<T>
where
    T: Eq,
{
    fn eq(&self, other: &FlipRemoveTransformAndSetTransition<T>) -> bool {
        let FlipRemoveTransformAndSetTransition(original) = self;
        let FlipRemoveTransformAndSetTransition(new) = other;

        original == new
    }
}

fn get_clear_style_instructions<T, U>(to_clear: &HashMap<T, U>) -> HashMap<T, ClearStyle<&U>>
where
    T: Hash + Eq + Clone + Display,
{
    to_clear
        .iter()
        .map(|(k, v)| (k.clone(), ClearStyle(v)))
        .collect()
}

#[derive(Debug)]
struct ClearStyle<T>(T);

impl<T> PartialEq<ClearStyle<T>> for ClearStyle<T>
where
    T: Eq,
{
    fn eq(&self, other: &ClearStyle<T>) -> bool {
        let ClearStyle(original) = self;
        let ClearStyle(new) = other;

        original == new
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        check_hash_map_key_diffs, get_clear_style_instructions, get_compute_position_instructions,
        get_diff_positions_instructions, get_remove_transform_and_set_transition_instructions,
        get_set_transform_and_transition_instructions, BeginFlip, ClearStyle, ComputePosition,
        DiffPositions, FlipDiffs, FlipNodes, FlipRemoveTransformAndSetTransition, HashMapDiffError,
        SetTransformAndTransition,
    };
    use std::collections::{HashMap, HashSet};

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

    #[test]
    fn get_diff_positions_instructions_returns_ok_when_hash_maps_have_same_keys() {
        let first_hash_map = HashMap::from([("a", 10), ("b", 20), ("c", 30)]);
        let second_hash_map = HashMap::from([("a", 40), ("b", 50), ("c", 60)]);

        if let Err(_) = get_diff_positions_instructions(&first_hash_map, &second_hash_map) {
            panic!("Diffing hash maps with same keys should not return an error");
        }
    }

    #[test]
    fn get_diff_positions_instructions_returns_error_when_hash_maps_have_different_keys() {
        let first_hash_map = HashMap::from([("a", 0), ("b", 1), ("c", 2)]);
        let second_hash_map = HashMap::from([("d", 0), ("b", 2), ("c", 3)]);

        if let Ok(_) = get_diff_positions_instructions(&first_hash_map, &second_hash_map) {
            panic!("Diffing hash maps with different keys should return an error");
        }
    }

    #[test]
    fn get_diff_positions_instructions_returns_instructions_for_all_key_pairs() {
        let first_hash_map = HashMap::from([("a", 0), ("b", 10), ("c", 20)]);
        let second_hash_map = HashMap::from([("a", 0), ("b", 100), ("c", 200)]);

        match get_diff_positions_instructions(&first_hash_map, &second_hash_map) {
            Ok(diffs) => {
                first_hash_map.keys().for_each(|k| {
                    let diff = diffs.get(k).unwrap();

                    let original = first_hash_map.get(k).unwrap();
                    let new = second_hash_map.get(k).unwrap();

                    assert_eq!(diff, &DiffPositions(original, new));
                });
            }
            Err(diffs) => {
                panic!(
                    "No errors should have been returned, but {:?} key differences were reported",
                    diffs
                );
            }
        }
    }

    fn compare_hash_map_errors<T>(make_hash_map: T)
    where
        T: for<'a> Fn(
            &'a HashMap<&'static str, i32>,
            &'a HashMap<&'static str, i32>,
        ) -> Result<
            HashMap<&'static str, DiffPositions<&'a i32>>,
            HashMapDiffError<&'static str>,
        >,
    {
        let first_hash_map_pair = (
            HashMap::from([("a", 0), ("b", 1)]),
            HashMap::from([("c", 0), ("b", 2)]),
        );
        let second_hash_map_pair = (
            HashMap::from([("a", 0), ("b", 2)]),
            HashMap::from([("a", 1), ("b", 2), ("c", 3)]),
        );
        let third_hash_map_pair = (
            HashMap::from([("b", 0), ("c", 10)]),
            HashMap::from([("a", 0), ("c", 20)]),
        );

        let hash_map_pairs = [
            first_hash_map_pair,
            second_hash_map_pair,
            third_hash_map_pair,
        ];

        hash_map_pairs.iter().for_each(|(original, new)| {
            match (
                check_hash_map_key_diffs(original, new),
                make_hash_map(original, new),
            ) {
                (Err(first_diff), Err(second_diff)) => {
                    assert_eq!(first_diff, second_diff);
                }
                (Ok(_), Err(diffs)) => {
                    panic!("function returned error whilst check_hash_map_key_diffs did not. {:?} was the returned error", diffs);
                }
                (Err(diffs), Ok(_)) => {
                    panic!("function did not return an error whilst check_hash_map_key_diffs did. {:?} was the returnedd error", diffs);
                }
                (Ok(_), Ok(_)) => {
                    panic!(
                        "Both functions returned OK although errors were supposed to be returned"
                    );
                }
            }
        });
    }

    #[test]
    fn get_diff_positions_instructions_returns_check_hash_map_key_diffs_error() {
        compare_hash_map_errors(|original, new| {
            Ok(get_diff_positions_instructions(original, new)?)
        });
    }

    #[test]
    fn get_set_transform_and_transition_instructions_returns_hash_map_with_all_given_keys() {
        let hash_map = HashMap::from([("a", 1), ("b", 2), ("c", 3)]);

        let set_transform_and_transition_instructions =
            get_set_transform_and_transition_instructions(&hash_map);

        match check_hash_map_key_diffs(&hash_map, &set_transform_and_transition_instructions) {
            Ok(()) => {
                hash_map.keys().for_each(|k| {
                    assert_eq!(
                        set_transform_and_transition_instructions.get(k).unwrap(),
                        &SetTransformAndTransition(hash_map.get(k).unwrap())
                    );
                });
            }
            Err(diffs) => {
                panic!("Given hash map and returned should have the same keys, but {:?} differences were reported", diffs);
            }
        }
    }

    #[test]
    fn get_remove_transform_instructions_returns_hash_map_with_all_given_keys() {
        let hash_map = HashMap::from([("a", 0), ("b", 2), ("c", 4)]);

        let remove_transform_instructions =
            get_remove_transform_and_set_transition_instructions(&hash_map);

        match check_hash_map_key_diffs(&hash_map, &remove_transform_instructions) {
            Ok(()) => {
                hash_map.keys().for_each(|k| {
                    assert_eq!(
                        &FlipRemoveTransformAndSetTransition(hash_map.get(k).unwrap()),
                        remove_transform_instructions.get(k).unwrap()
                    );
                });
            }
            Err(diffs) => {
                panic!("Given hash map and returned should have the same keys, but {:?} differences were reported", diffs);
            }
        }
    }

    #[test]
    fn get_clear_style_instructions_returns_hash_map_with_all_given_keys() {
        let hash_map = HashMap::from([("a", 0), ("b", 2), ("c", 4)]);

        let clear_style_instructions = get_clear_style_instructions(&hash_map);

        match check_hash_map_key_diffs(&hash_map, &clear_style_instructions) {
            Ok(()) => hash_map.keys().for_each(|k| {
                assert_eq!(
                    &ClearStyle(hash_map.get(k).unwrap()),
                    clear_style_instructions.get(k).unwrap()
                )
            }),
            Err(diffs) => {
                panic!("Given hash map and returned should have same keys, but {:?} differences were reported", diffs);
            }
        }
    }

    #[test]
    fn check_hash_maps_key_diffs_returns_error_when_keys_differ() {
        let first_hash_map = HashMap::from([("a", 0), ("b", 1)]);
        let second_hash_map = HashMap::from([("c", 0), ("b", 1)]);

        if let Ok(_) = check_hash_map_key_diffs(&first_hash_map, &second_hash_map) {
            panic!("The function should return an error if hash maps have different keys");
        }
    }

    #[test]
    fn check_hash_map_key_diffs_returns_ok_when_keys_are_same() {
        let first_hash_map = HashMap::from([("a", 0), ("b", 1)]);
        let second_hash_map = HashMap::from([("a", 10), ("b", 20)]);

        if let Err(_) = check_hash_map_key_diffs(&first_hash_map, &second_hash_map) {
            panic!("The function should not report errors if keys are same");
        }
    }

    #[test]
    fn check_hash_map_key_diffs_returns_error_with_original_keys_missing_in_new() {
        let first_hash_map = HashMap::from([("a", 1), ("b", 2), ("c", 3)]);
        let second_hash_map = HashMap::from([("a", 1)]);

        match check_hash_map_key_diffs(&first_hash_map, &second_hash_map) {
            Ok(_) => panic!(
                "As keys from original are missing in new, an error should have been returned"
            ),
            Err(HashMapDiffError {
                present_in_original_but_not_new,
                ..
            }) => {
                let expected = HashSet::from(["b", "c"]);

                assert_eq!(expected, present_in_original_but_not_new);
            }
        }
    }

    #[test]
    fn check_hash_map_key_diff_returns_error_with_new_keys_missing_in_original() {
        let first_hash_map = HashMap::from([("a", 1)]);
        let second_hash_map = HashMap::from([("a", 2), ("b", 3), ("c", 4)]);

        match check_hash_map_key_diffs(&first_hash_map, &second_hash_map) {
            Ok(_) => panic!(
                "As keys from new are missing in original, an error should have been returneed"
            ),
            Err(HashMapDiffError {
                present_in_new_but_not_original,
                ..
            }) => {
                let expected = HashSet::from(["b", "c"]);

                assert_eq!(expected, present_in_new_but_not_original);
            }
        }
    }

    #[test]
    fn begin_flip_takes_ids_to_nodes() {
        let hash_map = HashMap::from([("a", 0), ("b", 1), ("b", 2)]);

        let flip_nodes = BeginFlip::set_initial_nodes(hash_map.clone());

        assert_eq!(flip_nodes.nodes(), &hash_map);
    }

    #[test]
    fn flip_nodes_computes_positions_with_resolver() {
        let nodes = HashMap::from([("a", 1), ("b", 2), ("c", 3)]);

        let resolver = |_: &_, p: &_| Ok(p * 10);

        let flip_nodes = FlipNodes::new(nodes);

        let cloned_nodes = flip_nodes.clone();

        let flip_positions = cloned_nodes
            .compute_positions(resolver)
            .expect("Computing positions should not return an error");

        flip_positions.positions().keys().for_each(|k| {
            assert_eq!(
                *flip_positions.positions().get(k).unwrap(),
                flip_nodes.nodes().get(k).unwrap() * 10
            );
        });
    }

    #[test]
    fn compute_diffs_return_flip_diffs() {
        let nodes = HashMap::from([("a", 1), ("b", 2), ("c", 3)]);

        let original_begin_flip = BeginFlip::set_initial_nodes(nodes.clone());

        let original_flip_positions = original_begin_flip
            .compute_positions(|_, p| Ok(*p * 10))
            .expect("All elements should be had after setting initial nodes for original");

        let new_begin_flip = BeginFlip::set_initial_nodes(nodes.clone());

        let new_flip_positions = new_begin_flip
            .compute_positions(|_, p| Ok(*p * 20))
            .expect("All elements should be had after setting initial nodes for new");

        let flip_diffs =
            original_flip_positions.compute_diffs(new_flip_positions, |_, old, new| *old - *new);

        let expected = HashMap::from([("a", -10), ("b", -20), ("c", -30)]);

        nodes.keys().for_each(|k| {
            let actual_diff = flip_diffs.as_ref().unwrap().diffs().get(k).unwrap();
            let expected_diff = expected.get(k).unwrap();

            assert_eq!(actual_diff, expected_diff);
        });
    }

    #[test]
    fn set_transforms_and_transitions_returns_flip_transform_and_position() {
        let nodes = HashMap::from([("a", 1), ("b", 2), ("c", 3)]);
        let diffs = HashMap::from([("a", 10), ("b", 20), ("c", 30)]);

        let cloned_nodes = nodes.clone();

        let flip_diffs = FlipDiffs::new(cloned_nodes, diffs);

        let flip_transform_and_transition = flip_diffs
            .set_transforms(|_, _, _| Ok(()))
            .expect("Setting transforms and transitions should not fail");

        assert_eq!(&nodes, flip_transform_and_transition.nodes());
    }

    #[test]
    fn flip_nodes_allows_clearing_of_styles() {
        let nodes = HashMap::from([("a", 1), ("b", 2), ("c", 3)]);

        let flip_transform_and_transition = FlipNodes::new(nodes);

        let _ = flip_transform_and_transition.clear_styles(|_, _| Ok(()));
    }
}
