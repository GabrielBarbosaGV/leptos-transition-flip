use leptos::{
    html::{Div, ElementDescriptor},
    *,
};
use leptos_transition_flip::prepare_flip;
use std::{collections::HashMap, time::Duration};

// Struct to contain all the useful information. As we will need to identify each
// node_ref, we use an usize as a simple key. The ID needs to be
// Hash + Eq + Clone + Display. Display is necessary for showing errors.
#[derive(Clone)]
struct ListItem<V>
where
    V: ElementDescriptor + 'static,
{
    id: usize,
    text: String,
    node_ref: NodeRef<V>,
}

// Example component that does nothing beyond cosmetics. Two lists of elements
// that can transition among them.
#[component]
fn App() -> impl IntoView {
    // A node_ref is necessary for an ancestor component to be the DOM reflow target.
    // This node_ref needs to be reference a Div.
    let reflow_target = create_node_ref();

    let texts = vec!["One", "Two", "Three"];

    // List that houses all the transitioning items.
    let (items, _set_items) = create_signal(
        texts
            .into_iter()
            .enumerate()
            .map(|(index, text)| {
                create_signal(
                    ListItem {
                        id: index,
                        text: text.to_string(),
                        node_ref: create_node_ref::<Div>(),
                    },
                )
            })
            .collect::<Vec<_>>(),
    );

    // Left list that starts with all the elements.
    let (left_items, set_left_items) = create_signal(items());

    // Empty right list
    let (right_items, set_right_items) = create_signal(Vec::new());

    view! {
        <div node_ref=reflow_target class="w-[100vw] h-[100vh] bg-blue-200 flex justify-around items-center">
            <div class="flex justify-around items-center flex-col">
                <For
                    each=left_items
                    key=move |(item, _)| item().id
                    children=move |(item, _)| {
                        let node_ref = item().node_ref;

                        let move_item_right = move |_| {
                            let id = item().id;

                            // A HashMap is utilized to map the keys to node_refs.
                            let ids_to_node_refs = items()
                                    .into_iter()
                                    .map(|(item, _)| item())
                                    .map(|ListItem { id, node_ref, .. }| (id, node_ref))
                                    .collect::<HashMap<_, _>>();

                            // This is the leptos_transition_flip crate's function. It receives the
                            // aformentioned mapping, the node_ref of the reflow target, and,
                            // finally, the transition property to be set for the target HTML
                            // elements in FLIPping.
                            let (flip, clear) = match prepare_flip(
                                ids_to_node_refs,
                                reflow_target,
                                "transform 0.6s"
                            ) {
                                Ok(v) => v,
                                Err(e) => {
                                    println!("An error has occurred when preparing a FLIP: {:?}", e);
                                    return
                                }
                            };

                            // Get item that will be removed>
                            let item = left_items().into_iter().filter(|(i, _)| i().id == id).next().unwrap();

                            // Remove item from left list.
                            set_left_items.update(|items| {
                                let position = items.iter().position(|(i, _)| i().id == id).unwrap();

                                items.remove(position);
                            });

                            // Push item into right list.
                            set_right_items.update(|items| {
                                items.push(item);
                            });

                            // Do FLIP.
                            if let Err(err) = flip() {
                                println!("An error has occurred when attempting a FLIP: {:?}", err);
                            }

                            // Clear styles after timeout.
                            set_timeout(|| {
                                if let Err(err) = clear() {
                                    println!("An error has occurred when attempting to clear the elements' transition styles: {:?}", err);
                                }
                            }, Duration::from_millis(600));
                        };

                        view! {
                            <div node_ref=node_ref class="bg-green-200 my-2 w-[10vw] flex justify-center items-center" on:click=move_item_right>
                                <span>
                                    {move || item().text}
                                </span>
                            </div>
                        }
                    }
                />
            </div>

            <div class="flex flex-col justify-around items-center">
                <For
                    each=right_items
                    key=move |(item, _)| item().id
                    children=move |(item, _)| {
                        let node_ref = item().node_ref;

                        // Same process described above, but reversing the list positions.
                        let move_item_left = move |_| {
                            let id = item().id;

                            let ids_to_node_refs = items()
                                .into_iter()
                                .map(|(item, _)| item())
                                .map(|ListItem { id, node_ref, .. }| (id, node_ref))
                                .collect::<HashMap<_, _>>();

                            let (flip, clear) = match prepare_flip(ids_to_node_refs, reflow_target, "transform 0.6s") {
                                Ok(v) => v,
                                Err(e) => {
                                    println!("An error has occurred when preparing a FLIP: {:?}", e);
                                    return
                                }
                            };

                            let item = right_items().into_iter().filter(|(i, _)| i().id == id).next().unwrap();

                            set_right_items.update(|items| {
                                let position = items.into_iter().position(|(i, _)| i().id == id).unwrap();

                                items.remove(position);
                            });

                            set_left_items.update(|items| {
                                items.push(item);
                            });

                            if let Err(err) = flip() {
                                println!("An error has occurred when attempting a FLIP: {:?}", err);
                            }

                            set_timeout(|| {
                                if let Err(err) = clear() {
                                    println!("An error has occurred when attempting to clear the elements' transition styles: {:?}", err);
                                }
                            }, Duration::from_millis(600));
                        };

                        view! {
                            <div node_ref=node_ref class="bg-green-200 my-2 w-[10vw] flex justify-center items-center" on:click=move_item_left>
                                <span>
                                    {move || item().text}
                                </span>
                            </div>
                        }
                    }
                />
            </div>
        </div>
    }
}

fn main() {
    mount_to_body(|| view! { <App /> });
}
