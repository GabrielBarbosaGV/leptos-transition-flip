use crate::{
    begin_flip::BeginFlip,
    flip::{flip, FlipError},
};

use std::fmt::Display;

use leptos::{html::ElementDescriptor, IntoStyle, NodeRef};

use web_sys::HtmlElement;

use std::{collections::HashMap, hash::Hash, ops::Deref};

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
/// // ...
///
/// let (flip, clear) = prepare_flip(
///     ids_to_nodes,
///     reflow_target,
///     "transform 0.6s"
/// ).map_err(|e| format!("Prepare flip failed with error: {e}"))?;
///
/// // Perform actions that will change the node refs' elements' positions
/// // ...
///
/// flip().map_err(|e| format!("Flip failed with error: {e}"))?;
///
/// // ...
///
/// set_timeout(|| {
///     if let Err(e) = clear() {
///         println!("An error occurred when attempting to clear the elements' styles: {e}");
///     }
/// })
///
/// Ok(())
///
/// ```
pub fn prepare_flip<T, U, V>(
    ids_to_nodes: HashMap<T, NodeRef<U>>,
    reflow_target: NodeRef<U>,
    transition_style: impl IntoStyle + Clone,
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
                    let _ = html_element.clone().style("transition", "");

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
///
/// ```ignore
/// match prepare_flip(ids_to_nodes, reflow_target, "transform 0.6s") {
///     Ok((flip, clear)) => {
///         // Perform actions that will change the NodeRefs' elements' positions
///         // ...
///
///         flip().map_err(|e| format!("An error occurred when trying to perform a FLIP transition: {e}"))?;
///
///         set_timeout(|| {
///             match clear() {
///                 Ok(()) => (),
///                 Err(e) => println!(&format!("An error occurred when attempting to clear FLIP styles: {e}"))
///             }
///         }, Duration::from_millis(600));
///
///         Ok(())
///     },
///
///     Err(PrepareFlipError::CouldNotGetHtmlElement(ids)) => {
///         Err("Getting the HTML elements corresponding to the following IDs was not possible: [{ids}]")
///     }
/// }
/// ```
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
                    "Could not get element(s) from node reference(s) associated with [{}]",
                    ts.iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
}

/// Might occur when clearing the styles of elements from the given node references. The single
/// variant, CouldNotGetHtmlElement(`Vec<T>`), contains all IDs for which elements could not be
/// obtained from the given node references.
///
/// ```ignore
/// let (flip, clear) = prepare_flip(ids_to_nodes, reflow_target, "transform 0.6s")
///     .map_err(|e| format!("An error has occurred when attempting to prepare a flip: {e}"))?;
///
/// // ...
///
/// set_timeout(|| {
///     if let Err(ClearError::CouldNotGetHtmlElement(ids)) = clear() {
///         println!("Getting the HTML elements corresponding to the following IDs was not possible: [{ids}]");
///     }
/// })
/// ```
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
                    "Could not get element(s) for node reference(s) associated with [{}]",
                    ts.iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
}
