use leptos::{html::ElementDescriptor, IntoStyle, NodeRef};

use web_sys::HtmlElement;

use std::{collections::HashSet, fmt::Display, hash::Hash, ops::Deref};

use crate::{
    begin_flip::BeginFlip, flip_positions::FlipPositions, hash_map_diff_error::HashMapDiffError,
};

pub(crate) fn flip<T, U, V>(
    flip_positions: FlipPositions<T, NodeRef<U>, (f64, f64)>,
    reflow_target: NodeRef<U>,
    transition_style: impl IntoStyle + Clone,
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

                Ok((x, y))
            }
            None => Err(k.clone()),
        })
        .map_err(|e| FlipError::CouldNotGetHtmlElement(e))?;

    let flip_diffs = flip_positions
        .compute_diffs(new_flip_positions, |_, o, n| {
            let (original_x, original_y) = o;
            let (new_x, new_y) = n;

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
                let _ = html_element
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
                let _ = html_element
                    .clone()
                    .style("transition", transition_style.clone());
                let _ = html_element.clone().style("transform", "");

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
///
/// ```ignore
/// let (flip, clear) = prepare_flip(ids_to_nodes, reflow_target, "transform 0.6s")
///     .map_err(|e| format!("An error has occurred when attempting to prepare a flip: {e}"))?;
///
/// // Perform actions that change the nodes' positions in the DOM ...
///
/// if let Err(err) = flip() {
///     let err = match err {
///         FlipError::CouldNotGetHtmlElement(ids) => {
///             format!("Getting the HTML elements corresponding to the following IDs was not possible: [{ids}]")
///         },
///         FlipError::CouldNotGetReflowTarget => {
///             "\
///                 Getting the HTML elements for the reflow target NodeRef was not possible\
///             ".to_string()
///         },
///         FlipError::HashMapDiffError {
///             present_in_original_but_not_new,
///             present_in_new_but_not_original
///         } => format!(
///             "\
///                 Diffing the positions of the HTML elements was not possible, \
///                 as the [{present_in_original_but_not_new}] IDs were found in \
///                 the first HashMap, but not the second, and the converse was \
///                 the case for [{present_in_new_but_not_original}].\
///             "
///         )
///     }
///
///     return Err(err)
/// }
///
/// // ...
///
/// ```
#[derive(Debug, Clone)]
pub enum FlipError<T> {
    CouldNotGetHtmlElement(Vec<T>),
    CouldNotGetReflowTarget,
    HashMapDiffError {
        present_in_new_but_not_original: HashSet<T>,
        present_in_original_but_not_new: HashSet<T>,
    },
}

impl<T> PartialEq for FlipError<T>
where
    T: Eq + Hash,
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                FlipError::HashMapDiffError {
                    present_in_new_but_not_original: first_original,
                    present_in_original_but_not_new: second_original,
                },
                FlipError::HashMapDiffError {
                    present_in_new_but_not_original: first_new,
                    present_in_original_but_not_new: second_new,
                },
            ) => first_original == first_new && second_original == second_new,

            (a, b) => a == b,
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
                    "Could not get element(s) from node references(s) associated with [{}]",
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
                    are [{}], while the elements present in the second hash map but \
                    not the first are [{}]",
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
