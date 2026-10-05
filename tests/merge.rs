// cargo nextest run --test grouped_saves -E 'test(merge::)'

#[path = "merge/nodeInstructions_from_the_merges_in_a_viewforest.rs"]
mod nodeInstructions_from_the_merges_in_a_viewforest;

#[path = "merge/merge_nodes.rs"]
mod merge_nodes;

#[path = "merge/merge_container_into_content.rs"]
mod merge_container_into_content;

#[path = "merge/merge_acquiree_in_sibling_subtree.rs"]
mod merge_acquiree_in_sibling_subtree;

#[path = "merge/merge_acquiree_as_independent_root.rs"]
mod merge_acquiree_as_independent_root;

#[path = "merge/merge_preserves_acquiree_child_bodies.rs"]
mod merge_preserves_acquiree_child_bodies;

#[path = "merge/merge_hides_intersection.rs"]
mod merge_hides_intersection;
