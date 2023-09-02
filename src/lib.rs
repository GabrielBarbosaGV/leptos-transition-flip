use std::{collections::HashMap, fmt::Display, hash::Hash, ops::Deref, time::Duration};

use leptos::{
    html::{Div, ElementDescriptor},
    set_timeout, NodeRef,
};
use web_sys::HtmlElement;

/// Should be used before actions that change the NodeRefs' positions. Takes HashMap of arbitrary
/// keys to NodeRefs that should be dislocated, the NodeRef for a container Div which
/// must be targeted for DOM reflow, as well as the duration the animation should have. A reflow
/// is the recomputation of an element's position and dimensions, and must be done in order to
/// FLIP. Returns a pair of functions. The first one animates the given elements when called, the
/// second one clears the transition styles, and should be called once the length of the animation
/// has passed.
///
/// ```
/// // Call before changing NodeRefs' positions
/// let (flip, clear) = prepare_flip(
///     ids_to_node_refs,
///     container_div_node_ref,
///     Duration::from_millis(600)
/// );
///
/// // Perform action that will change the NodeRefs' positions in page, such as setting signals
/// // ...
///
/// // Perform FLIP animation
/// flip().map_err(|err| format!(""));
///
/// // Await end and then clear transition style
/// set_timeout(|| {
///     clear();
/// }, Duration::from_millis(600));
/// ```
pub fn prepare_flip<T, U, V>(
    mapping: HashMap<T, NodeRef<U>>,
    reflow_target: NodeRef<Div>,
    duration: Duration,
) -> Result<
    (
        impl FnOnce() -> Result<(), FlipError<T>>,
        impl FnOnce() -> Result<(), RemoveTransitionError<T>>,
    ),
    PrepareFlipError<T>,
>
where
    T: ToOwned<Owned = T> + Hash + Eq + Clone,
    U: ElementDescriptor + Deref<Target = V> + Clone + 'static,
    V: Deref<Target = HtmlElement>,
{
    let flip_mapping = mapping.clone();
    let clear_mapping = mapping.clone();

    match get_positions_from_node_refs(&mapping) {
        Err(ts) => Err(PrepareFlipError(ts)),
        Ok(ids_to_positions) => Ok((
            move || flip(flip_mapping, &ids_to_positions, reflow_target, duration),
            move || remove_transition(clear_mapping),
        )),
    }
}

fn get_positions_from_node_refs<T, U, V>(
    mapping: &HashMap<T, NodeRef<U>>,
) -> Result<HashMap<T, (f64, f64)>, Vec<T>>
where
    T: ToOwned<Owned = T> + Hash + Eq,
    U: ElementDescriptor + Deref<Target = V> + Clone + 'static,
    V: Deref<Target = HtmlElement>,
{
    let mut ids_for_which_element_could_not_be_obtained = Vec::new();

    let mut ids_to_positions = HashMap::new();

    mapping.iter().for_each(|(k, v)| match v.get() {
        None => ids_for_which_element_could_not_be_obtained.push(k.to_owned()),
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
    mapping: HashMap<T, NodeRef<U>>,
    ids_to_positions: &HashMap<T, (f64, f64)>,
    reflow_target: NodeRef<Div>,
    duration: Duration,
) -> Result<(), FlipError<T>>
where
    T: ToOwned<Owned = T> + Hash + Eq,
    U: ElementDescriptor + Deref<Target = V> + Clone + 'static,
    V: Deref<Target = HtmlElement>,
{
    let ids_to_new_positions = get_positions_from_node_refs(&mapping)
        .map_err(|ts| FlipError::ObtainNewPositionsError(ts))?;

    let ids_to_diffs = get_diffs_from_positions(ids_to_positions, &ids_to_new_positions);

    style_elements_with_diffs(&mapping, &ids_to_diffs)
        .map_err(|ts| FlipError::StyleElementsWithOffsetError(ts))?;

    match reflow_target.get() {
        None => return Err(FlipError::GetReflowTargetError),
        Some(element) => {
            element.offset_width();
        }
    };

    style_elements_with_no_transform(&mapping, duration)
        .map_err(|ts| FlipError::StyleElementsWithNoTransformError(ts))?;

    set_timeout(|| {}, duration);

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
) -> Result<(), Vec<T>>
where
    T: ToOwned<Owned = T> + Hash + Eq,
    U: ElementDescriptor + Deref<Target = V> + Clone + 'static,
    V: Deref<Target = HtmlElement>,
{
    let mut ids_for_which_element_could_not_be_obtained = Vec::new();

    mapping.iter().for_each(|(k, v)| match v.get() {
        None => ids_for_which_element_could_not_be_obtained.push(k.to_owned()),
        Some(element) => {
            if ids_for_which_element_could_not_be_obtained.len() == 0 {
                let (x, y) = diffs.get(k).unwrap();

                element
                    .clone()
                    .style("transform", &format!("translate({x}px, {y}px)"));
            }
        }
    });

    if ids_for_which_element_could_not_be_obtained.len() > 0 {
        Err(ids_for_which_element_could_not_be_obtained)
    } else {
        Ok(())
    }
}

fn style_elements_with_no_transform<T, U, V>(
    mapping: &HashMap<T, NodeRef<U>>,
    duration: Duration,
) -> Result<(), Vec<T>>
where
    T: ToOwned<Owned = T> + Hash + Eq,
    U: ElementDescriptor + Deref<Target = V> + Clone + 'static,
    V: Deref<Target = HtmlElement>,
{
    let mut ids_for_which_element_could_not_be_obtained = Vec::new();

    mapping.iter().for_each(|(k, v)| match v.get() {
        None => ids_for_which_element_could_not_be_obtained.push(k.to_owned()),
        Some(element) => {
            if ids_for_which_element_could_not_be_obtained.len() == 0 {
                element.clone().style(
                    "transition",
                    &format!("transform {}s", duration.as_secs_f64()),
                );

                element.clone().style("transform", "");
            }
        }
    });

    if ids_for_which_element_could_not_be_obtained.len() > 0 {
        Err(ids_for_which_element_could_not_be_obtained)
    } else {
        Ok(())
    }
}

fn remove_transition<T, U, V>(
    mapping: HashMap<T, NodeRef<U>>,
) -> Result<(), RemoveTransitionError<T>>
where
    T: ToOwned<Owned = T> + Hash + Eq,
    U: ElementDescriptor + Deref<Target = V> + Clone + 'static,
    V: Deref<Target = HtmlElement>,
{
    let mut ids_for_which_element_could_not_be_obtained = Vec::new();

    mapping.iter().for_each(|(k, v)| match v.get() {
        None => ids_for_which_element_could_not_be_obtained.push(k.to_owned()),
        Some(element) => {
            if ids_for_which_element_could_not_be_obtained.len() == 0 {
                element.clone().style("transition", "");
            }
        }
    });

    if ids_for_which_element_could_not_be_obtained.len() > 0 {
        Err(RemoveTransitionError(
            ids_for_which_element_could_not_be_obtained,
        ))
    } else {
        Ok(())
    }
}

const EMPTY_ERROR_COLLECTION_EXPECT_MESSAGE: &str = "\
    At least one ID should be contained in error's collection for it to have occurred, \
    please report this error to https://github.com/GabrielBarbosaGV/leptos-transition-flip\
";

/// Might occur when preparing a flip, where elements could not be obtained. The error contains the
/// list of identifiers for which the elements could not be obtained.
#[derive(Debug)]
pub struct PrepareFlipError<T>(Vec<T>);

impl<T> Display for PrepareFlipError<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let PrepareFlipError(ids) = self;

        let ids = format_vec(ids).expect(EMPTY_ERROR_COLLECTION_EXPECT_MESSAGE);

        write!(f, "Elements could not be obtained for IDs {ids} when attempting to get their current positions")
    }
}

#[derive(Debug)]
/// Might occur when flipping
pub enum FlipError<T> {
    /// Means that, for some identifiers, their positions could not be obtained. Wraps Vec<T> of
    /// problematic identifiers.
    ObtainNewPositionsError(Vec<T>),

    /// Means that an error occured when trying to style the elements with new translate values.
    /// Wraps Vec<T> of problematic identifiers.
    StyleElementsWithOffsetError(Vec<T>),

    /// Means that getting the HTML element from the reflow target was no possible.
    GetReflowTargetError,

    /// Means an error occurred when styling elements with no transform, which is a necessary step
    /// for transitioning them to their final positions.
    StyleElementsWithNoTransformError(Vec<T>),
}

impl<T> Display for FlipError<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {

        match self {
            Self::ObtainNewPositionsError(ids) => {
                let ids = format_vec(ids).expect(EMPTY_ERROR_COLLECTION_EXPECT_MESSAGE);
                write!(f, "Elements could not be obtained for IDs {ids} when attempting to get their new positions")
            }
            Self::StyleElementsWithOffsetError(ids) => {
                let ids = format_vec(ids).expect(EMPTY_ERROR_COLLECTION_EXPECT_MESSAGE);
                write!(f, "Elements could not be obtained for IDs {ids} when attempting to style them with their offset positions")
            }
            Self::GetReflowTargetError => {
                write!(f, "Reflow target element could not be obtained")
            }
            Self::StyleElementsWithNoTransformError(ids) => {
                let ids = format_vec(ids).expect(EMPTY_ERROR_COLLECTION_EXPECT_MESSAGE);
                write!(f, "Elements could not be obtained for IDs {ids} when attempting to style them with no transform and a transition duration")
            }
        }
    }
}

// Might occur when clearing the transition styles
#[derive(Debug)]
pub struct RemoveTransitionError<T>(Vec<T>);

impl<T> Display for RemoveTransitionError<T> where T: Display {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let RemoveTransitionError(ids) = self;
        let ids = format_vec(ids).expect(EMPTY_ERROR_COLLECTION_EXPECT_MESSAGE);
        write!(f, "Elements could not be obtained for IDs {ids} when attempting to style them with no transition")
    }
}

fn format_vec<T>(vs: &Vec<T>) -> Option<String> where T: Display {
    let last_element = vs.last()?;

    let first_elements = &vs[0..vs.len() - 1];

    match first_elements.len() {
        0 => Some(format!("{last_element}")),
        _ => {
            let first_elements = first_elements
                .into_iter()
                .map(|elem| format!("{elem}"))
                .collect::<Vec<_>>()
                .join(", ");

            Some(format!("{first_elements} and {last_element}"))
        }
    }
}
