# Review Notes for Commit 146199b3: "Toward the ability to merge nodes"

## Overview
This commit implements node merging functionality for the skg knowledge graph system. When two nodes are merged, one node (the "acquirer") absorbs another node (the "acquiree"), preserving the acquiree's content in a separate "acquiree_text_preserver" node.

## File Structure Changes

### New Module: rust/merge.rs
- Main merge module with three submodules: typedb.rs, fs.rs, tantivy.rs
- Entry point function: `merge_nodes_in_graph` - coordinates merging across all three systems
- Helper function: `instructiontriples_from_the_merges_in_an_orgnode_forest` - generates SaveInstructions from merge requests

### Type System Changes (rust/types/skgnode.rs)

**Critical change**: Three relationship fields changed from `Vec<ID>` to `Option<Vec<ID>>`:
- `subscribes_to`: Vec<ID> → Option<Vec<ID>>
- `hides_from_its_subscriptions`: Vec<ID> → Option<Vec<ID>>
- `overrides_view_of`: Vec<ID> → Option<Vec<ID>>

**Rationale**: This allows distinguishing between:
- User explicitly set empty list: Some(vec![])
- User didn't specify (should use disk value): None

This is important for the "none_node_fields_are_noops" functionality where unspecified fields should not overwrite existing data.

### New Type: NodeRequest::Merge(ID)
Added to rust/types/orgnode.rs as a new variant of NodeRequest enum. Allows org nodes to request merging with another node by ID.

### Parser Changes (rust/serve/parse_headline_md_sexp.rs)
Extended to parse merge requests from metadata:
- Format: `(merge "target-id")`
- Example: `(skg (id "abc") (requests (merge "xyz")))`

## The Merge Algorithm

### High-Level Process

When merge request is found in orgnode:
1. **Generate SaveInstructions** (3 per merge):
   - acquiree_text_preserver: new node containing acquiree's title and body
   - updated_acquirer: acquirer with extra IDs and combined contents
   - deleted_acquiree: acquiree marked for deletion

2. **Execute in 3 phases** (order matters!):
   - Phase 1: TypeDB - update graph relationships and entities
   - Phase 2: Filesystem - write/update/delete .skg files
   - Phase 3: Tantivy - update search index

PITFALL: If phase 2 or 3 fails after phase 1 succeeds, system state is invalid.

### SaveInstruction Generation Details

#### acquiree_text_preserver
- New ID (UUID)
- Title: "MERGED-{acquiree.title}"
- Body: copy of acquiree.body
- Empty relationships (no contains, subscribes, etc.)

#### updated_acquirer
- IDs: acquirer.ids + all acquiree.ids (deduplicated)
- contains: [acquiree_text_preserver_id] + acquirer.contains + acquiree.contains
- subscribes_to: combined later in fs.rs
- hides_from_its_subscriptions: combined later in fs.rs (with filtering)
- overrides_view_of: combined later in fs.rs

#### deleted_acquiree
- Original acquiree node with toDelete flag set

## TypeDB Merge Algorithm (rust/merge/typedb.rs)

All merges batched in single transaction for atomicity.

For each merge, relationships are rerouted in this order:

### 1. Reroute contains (bilateral transfer)
- Acquiree as container → acquirer as container
- Acquiree as contained → acquirer as contained
- Both directions transferred

### 2. Reroute subscribes (bilateral transfer)
- Acquiree as subscriber → acquirer as subscriber
- Acquiree as subscribee → acquirer as subscribee
- Both directions transferred

### 3. Reroute overrides_view_of (conditional)
- Acquiree as replacement → acquirer as replacement (TRANSFER)
- Acquiree as replaced → DROP (do not recreate)

**Rationale**: If something was overriding the acquiree's view, that relationship becomes meaningless after merge.

### 4. Reroute hides_from_its_subscriptions (complex, conditional)
- Acquiree is hidden → DROP (do not recreate)
  - Rationale: Acquiree no longer exists as separate entity

- Acquiree is hider → CONDITIONAL TRANSFER
  - Query each hidden node ID
  - IF hidden_id NOT IN acquirer_final_contains:
    - Transfer relationship (acquirer hides that node)
  - ELSE (hidden_id IS in acquirer_final_contains):
    - DROP (can't hide your own content from subscriptions)

### 5. Reroute hyperlinks_to (incoming only)
- Acquiree as dest → acquirer as dest (TRANSFER incoming links)
- Acquiree as source → NO TRANSFER

**Rationale**: Outbound hyperlinks from acquiree's body go to acquiree_text_preserver, so those should remain pointing from the preserved text.

### 6. Delete acquiree's extra_ids
Must happen BEFORE deleting the node itself (entity must exist for relationship deletion).

### 7. Delete acquiree node
Simple TypeDB delete query.

### 8. Create acquiree_text_preserver
New node with its relationships (including hyperlinks from its body).

### 9. Create contains relationship
Acquirer contains acquiree_text_preserver.

### 10. Add all acquiree IDs as extra_ids of acquirer
Creates extra_id entities and has_extra_id relationships.
Must happen AFTER deleting acquiree's extra_ids to avoid conflicts.

## Filesystem Merge Algorithm (rust/merge/fs.rs)

Simpler than TypeDB because just writing files.

For each merge:
1. **Write acquiree_text_preserver to disk**
   - Path based on its new UUID

2. **Compute and write updated acquirer**
   - subscribes_to: acquirer + acquiree (combined)
   - hides_from_its_subscriptions: acquirer + acquiree, but FILTER out any IDs that are in acquirer_final_contains
   - overrides_view_of: acquirer + acquiree (combined)
   - contains: already set in updated_acquirer from merge.rs

3. **Delete acquiree file**
   - std::fs::remove_file on acquiree's .skg file

## Tantivy Merge Algorithm (rust/merge/tantivy.rs)

Delegates to existing `update_index_from_saveinstructions` function.

That function handles:
- Deleting all IDs from index (including acquiree)
- Re-adding non-deleted nodes (acquiree_text_preserver and updated_acquirer)
- Skipping deleted nodes (acquiree)

## Other Important Changes

### rust/save/none_node_fields_are_noops.rs
Now handles the three new Option fields:
- If user didn't specify (None), use value from disk
- This prevents unintentional overwrites

### rust/save/orgnodes_to_instructions/from_tree.rs
Updated mk_skgnode to use None instead of vec![] for the three relationship fields.

### rust/rebuild.rs
Added handling for NodeRequest::Merge:
- Does nothing during rebuild/view
- Merge requests only processed during save

### Tests

#### tests/merge/merge_nodes_in_graph.rs (687 lines!)
Comprehensive test suite covering:
- Basic merge operations
- Relationship rerouting for all relationship types
- Edge cases (self-loops, cycles, etc.)
- Verification that all three systems (TypeDB, FS, Tantivy) are consistent

Test fixtures in tests/merge/merge_nodes_in_graph/fixtures/:
- Various .skg files representing different relationship scenarios
- Nodes with different relationship configurations

## Key Design Decisions

1. **Three-component merge**: Node gets split into three SaveInstructions
   - Preserves acquiree's text content
   - Allows acquirer to absorb acquiree's identity
   - Clean deletion of original acquiree

2. **Relationship rerouting rules vary by type**:
   - contains: bilateral (both container and contained roles transfer)
   - subscribes: bilateral
   - overrides_view_of: asymmetric (only replacement role transfers, replaced role drops)
   - hides_from_its_subscriptions: complex conditional logic
   - hyperlinks_to: incoming only (outbound stay with preserved text)

3. **Order of operations matters**:
   - TypeDB first (most critical, atomic via transaction)
   - FS second (persistent storage)
   - Tantivy last (search index, can be rebuilt)

4. **Categorization pattern**:
   - SaveInstructions identified by acquiree_text_preserver having title starting with "MERGED-"
   - Then next two instructions are updated_acquirer and deleted_acquiree
   - This pattern repeated for multiple merges

## Questions/Potential Issues

1. **Atomicity**: If FS or Tantivy fails after TypeDB succeeds, system is inconsistent. No rollback mechanism apparent.

2. **Concurrency**: No locking mechanism visible. What if two clients try to merge simultaneously?

3. **Validation**: What prevents merging a node with itself? Or circular merge requests?

4. **Performance**: All merges in one transaction could be slow for many simultaneous merges.

5. **Error handling**: Errors are propagated up, but partial state changes in TypeDB may have occurred.

## Test Coverage Observations

The test file includes fixtures for:
- Basic containment relationships
- Subscribe relationships
- Override relationships
- Hide relationships
- Hyperlinks
- Complex combinations

This suggests thorough testing of the relationship rerouting logic.
