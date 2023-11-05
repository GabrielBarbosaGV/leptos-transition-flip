# leptos-transition-flip
Enables translate-based FLIP animations for elements referenced by NodeRefs.

## Example
![FLIP example](./flip_example.gif)
(Quality and framerate extremely lowered to avoid large GIF size)

## How to use
The single function exposed by this crate is responsible for taking the initial positions
of the given HashMap of arbitrary IDs (T that must be Hash + Eq + Clone + Display) to
their NodeRefs,
a [DOM reflow](https://stackoverflow.com/questions/27637184/what-is-dom-reflow) target,
as well as a transition property to determine the smoothing of the movement of
the element element, such as "all 0.6s" (the CSS "transition" property), and return a
(flip, clear) tuple.

The prepare_flip function should be called when the elements are in their initial positions.
Then, flip should be called when immediately upon changing the positions of the NodeRefs'
elements. After the transition has ended, clear should be called to remove the applied
styles property from the given elements. Please note, however, that this last step is
optional, and an issue you might run into, if you use it after a fixed time interval, is the
cutoff of a second transition that might have been triggered before the end of the current
one. This happens due to the transition property being cleared as the second transition
element approaches its destination, resulting in a visible jump cut. To avoid this, you
might call the clear function only at the very end of all transitions.


An example of usage is:

```rust
let (flip, clear) = prepare_flip(
    ids_to_node_refs,
    container_div_node_ref,
    "transform 0.6s"
).unwrap();

// Perform action that will change the NodeRefs' positions in page, such as settings signals
// ...

// Perform FLIP
let _ = flip();

set_timeout(|| { let _ = clear(); }, Duration::from_millis(600));
```

And a full example project can be found [here](./example-project/).
