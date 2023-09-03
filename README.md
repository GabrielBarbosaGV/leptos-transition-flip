# leptos-transition-flip
Enables translate-based FLIP animations for elements referenced by NodeRefs.

## Example
![FLIP example](./flip_example.gif)
(Quality and framerate extremely lowered to avoid large GIF size)

## How to use
The single function exposed by this crate is responsible for taking the initial positions
of the given HashMap of arbitrary IDs (T that must be Hash + Eq + Clone) to their
NodeRefs, a [DOM reflow](https://stackoverflow.com/questions/27637184/what-is-dom-reflow)
target, as well as a transition property to determine the smoothing of the movement of
the element element, such as "all 0.6s" (the CSS "transition" property), and return a
(flip, clear) tuple.

The prepare_flip function should be called when the elements are in their initial positions.
Then, flip should be called when immediately upon changing the positions of the NodeRefs'
elements. After the transition has ended, clear should be called to remove the transition
property from the given elements.

An example of usage is:

```rust
let (flip, clear) = prepare_flip(
    ids_to_node_refs,
    container_div_node_ref,
    "transform 0.6s".to_string()
).unwrap();

// Perform action that will change the NodeRefs' positions in page, such as settings signals
// ...

// Perform FLIP
let _ = flip();

set_timeout(|| { let _ = clear(); }, Duration::from_millis(600));
```
