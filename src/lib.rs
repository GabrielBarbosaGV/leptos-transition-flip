use std::{collections::HashMap, hash::Hash, ops::Deref};

use leptos::{
    html::{Div, ElementDescriptor},
    NodeRef,
};
use web_sys::HtmlElement;

/// Should be used before actions that change the NodeRefs' positions. Takes HashMap of arbitrary
/// keys to NodeRefs that should be dislocated, as well as the NodeRef for a container Div which
/// must be targeted for DOM reflow. A reflow is the recomputation of an element's position and
/// dimensions, and must be done in order to FLIP. Returns a function that animates the given
/// elements when called.
///
/// ```
/// // Call before changing NodeRefs' positions
/// let flip = prepare_flip(ids_to_node_refs, container_div_node_ref);
///
/// // Perform action that will change the NodeRefs' positions in page, such as setting signals
/// // ...
///
/// // Perform FLIP animation
/// flip();
/// ```
pub fn prepare_flip<T, U, V>(
    mapping: &HashMap<T, NodeRef<U>>,
    reflow_target: NodeRef<Div>,
) -> Result<impl FnOnce() -> Result<(), FlipError<T>> + '_, PrepareFlipError<T>>
where
    T: ToOwned<Owned = T> + Hash + Eq,
    U: ElementDescriptor + Deref<Target = V> + Clone + 'static,
    V: Deref<Target = HtmlElement>,
{
    match get_positions_from_node_refs(&mapping) {
        Err(ts) => {
            let ts = ts.into_iter().map(|t| t.to_owned()).collect();

            Err(PrepareFlipError(ts))
        }
        Ok(ids_to_positions) => Ok(move || flip(mapping, &ids_to_positions, reflow_target)),
    }
}

fn get_positions_from_node_refs<T, U, V>(
    mapping: &HashMap<T, NodeRef<U>>,
) -> Result<HashMap<T, (f64, f64)>, Vec<&T>>
where
    T: ToOwned<Owned = T> + Hash + Eq,
    U: ElementDescriptor + Deref<Target = V> + Clone + 'static,
    V: Deref<Target = HtmlElement>,
{
    let mut ids_for_which_element_could_not_be_obtained = Vec::new();

    let mut ids_to_positions = HashMap::new();

    mapping.iter().for_each(|(k, v)| match v.get() {
        None => ids_for_which_element_could_not_be_obtained.push(k),
        Some(element) => {
            if ids_for_which_element_could_not_be_obtained.len() == 0 {
                let position = element.get_bounding_client_rect();

                ids_to_positions.insert(k.to_owned(), (position.x(), position.y()));
            }
        }
    });

    if ids_for_which_element_could_not_be_obtained.len() > 0 {
        Err(ids_for_which_element_could_not_be_obtained)
    } else {
        Ok(ids_to_positions)
    }
}

fn flip<T, U, V>(
    mapping: &HashMap<T, NodeRef<U>>,
    ids_to_positions: &HashMap<T, (f64, f64)>,
    reflow_target: NodeRef<Div>,
) -> Result<(), FlipError<T>>
where
    T: ToOwned<Owned = T> + Hash + Eq,
    U: ElementDescriptor + Deref<Target = V> + Clone + 'static,
    V: Deref<Target = HtmlElement>,
{
    let ids_to_new_positions = get_positions_from_node_refs(mapping).map_err(|ts| {
        let ts = ts.into_iter().map(|t| t.to_owned()).collect();

        FlipError::ObtainNewPositionsError(ts)
    })?;

    let ids_to_diffs = get_diffs_from_positions(ids_to_positions, &ids_to_new_positions);

    style_elements_with_diffs(kj, diffs)

    Ok(())
}

fn get_diffs_from_positions<T>(
    old: &HashMap<T, (f64, f64)>,
    new: &HashMap<T, (f64, f64)>,
) -> HashMap<T, (f64, f64)>
where
    T: ToOwned<Owned = T> + Hash + Eq,
{
    old.iter()
        .map(|(k, (old_x, old_y))| {
            let (new_x, new_y) = new.get(k).unwrap();

            (k.to_owned(), (old_x - new_x, old_y - new_y))
        })
        .collect()
}

fn style_elements_with_diffs<T, U, V>(
    mapping: &HashMap<T, NodeRef<U>>,
    diffs: &HashMap<T, (f64, f64)>,
) where
    T: ToOwned<Owned = T> + Hash + Eq,
    U: ElementDescriptor + Deref<Target = V> + Clone + 'static,
    V: Deref<Target = HtmlElement>,
{
    let mut ids_for_which_element_could_not_be_obtained = Vec::new();

    mapping.iter().for_each(|(k, v)| match v.get() {
        None => ids_for_which_element_could_not_be_obtained.push(k),
        Some(element) => {
            if ids_for_which_element_could_not_be_obtained.len() == 0 {
                let (x, y) = diffs.get(k).unwrap();

                element.style("transform", &format!("translate({}px, {}px)", x, y));
            }
        }
    });
}

/// Might occur when preparing a flip, where elements could not be obtained. The error contains the
/// list of identifiers for which the elements could not be obtained.
pub struct PrepareFlipError<T>(Vec<T>);

/// Might occur when flipping
pub enum FlipError<T> {
    /// Means that, for some identifiers, their positions could not be obtained. Wraps Vec<T> of
    /// problematic identifiers.
    ObtainNewPositionsError(Vec<T>),
}
