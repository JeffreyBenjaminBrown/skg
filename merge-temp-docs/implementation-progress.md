# Merge Feature - Implementation Progress

## How to use this document

Don't describe changes to come (that's for another document),
nor changes made (that can be gleaned from git diff).
Rather, just keep a list of types and functions of interest.
So if you build a function F and it uses a helper G,
and nothing else will use G, don't document G here,
but do document F.

Similarly with types: The types that exist are of interest,
be they new ones, modified old ones, or unchanged old ones --
but changes made to earlier types are no longer of interest.

## Implemented Functions

### 1. `saveinstructions_from_the_merges_in_an_orgnode_forest`

**File:** `rust/merge.rs:17-81` (now 17-82 after removing helper)

**Purpose:** Creates SaveInstructions for merge operations from an orgnode forest.

**What it does:**
- Walks the orgnode forest to find nodes with `Merge(ID)` requests
- For each merge request found, generates three SaveInstructions:
  1. **MERGED node**: A synthetic node with title "MERGED: <acquiree title>" and acquiree's body
  2. **Updated acquirer**: The acquiring node with merged IDs and combined contents
  3. **Acquiree deletion**: The acquired node marked for deletion (toDelete=true)

**Key details:**
- Reads both acquirer and acquiree data from disk (.skg files)
- Uses `path_from_pid` to locate files
- Generates new UUID for MERGED node
- Combines contents in order: [MERGED_ID] + acquirer's old + acquiree's old
- Combines IDs: acquirer's current IDs + acquiree_id

**TODO comment:** "This is slightly inefficient. It would be faster to collect a list of orgnodes with merge instructions during one of the other walks of the forest."

**Test coverage:** ✅ `tests/merge/saveinstructions_from_the_merges_in_an_orgnode_forest.rs`

## Type Changes Implemented

### NodeRequest Enum (Phase 1)

**File:** `rust/types/orgnode.rs:70-76`

**Addition:**
```rust
pub enum NodeRequest {
  ContainerwardView,
  SourcewardView,
  Merge(ID),  // NEW: Request to merge another node into this one
}
```

**Display implementation:** `rust/types/orgnode.rs:110-119`
- Outputs `(merge <id>)` for Merge variant

**FromStr implementation:** `rust/types/orgnode.rs:121-138`
- Parses "merge <id>" syntax (though list syntax is used in practice)

---

## Parser Changes Implemented (Phase 2)

### parse_requests_sexp

**File:** `rust/serve/parse_headline_md_sexp.rs:161-189`

**Changes:**
- Added handler for list syntax: `(merge <id>)`
- Existing atom-based parsing preserved for ContainerwardView and SourcewardView
- New case: `Sexp::List` with 2 elements where first is "merge"

---

## Other Changes

### rebuild.rs Pattern Matching

**File:** `rust/rebuild.rs:94-104`

**Change:** Added handler for `NodeRequest::Merge(_)` variant
- Merge requests are ignored during rebuild/view (they're handled during save)
- Prevents non-exhaustive pattern match error

---

## What's NOT Yet Implemented

The following phases from `merge-plan.md` still need implementation:

### Phase 1 (Partial) - Remaining Type Changes
- [ ] Add `merge: Option<ID>` field to `NodeSaveAction`
- [ ] Remove `Copy` trait from `NodeSaveAction`
- [ ] Add merge-related error variants to `Buffer_Cannot_Be_Saved`
- [ ] Update `format_buffer_validation_error` function

### Phase 3 - Validation
- [ ] `find_merge_errors` function
  - [ ] Merge+delete conflicts
  - [ ] Acquiree existence checking
  - [ ] Circular merge detection
  - [ ] Acquiree also merging check
- [ ] Make `find_buffer_errors_for_saving` async
- [ ] Update call sites to pass DB driver

### Phase 4 - Extract Merge Info (Partial)
- [x] Created `saveinstructions_from_the_merges_in_an_orgnode_forest` (this is a variant approach)
- [ ] Original plan: extract merge info in `orgnodes_to_save_instructions`

### Phase 5 - Implement merge_in_graph
- [ ] `merge_in_typedb` - relationship rerouting, extra_ids
- [ ] `merge_in_fs` - update acquirer file, delete acquiree file
- [ ] `merge_in_tantivy` - remove acquiree from index
- [ ] Helper functions:
  - [ ] `fetch_node_data` (from DB and FS)
  - [ ] `fetch_contains_ids` (from TypeDB)
  - [ ] `fetch_extra_ids` (from TypeDB)
  - [ ] `reroute_contains_relationships`
  - [ ] `reroute_hyperlinks_relationships`
  - [ ] `reroute_subscribes_relationships`
  - [ ] `reroute_hides_relationships`
  - [ ] `reroute_overrides_relationships`
  - [ ] `delete_acquiree_node`

### Phase 6 - Integration
- [ ] Call merge functions in save flow
- [ ] `remove_merged_nodes_from_forest` (so acquiree doesn't appear in rendered view)

### Phase 7 - Testing
- [x] Unit test for SaveInstruction generation
- [ ] Integration test for full merge flow
- [ ] Tests for each relationship type
- [ ] Cycle detection tests
- [ ] Validation error tests

---

## Current Architecture

**SaveInstruction Generation:**
```
orgnode forest (with merge requests)
    ↓
saveinstructions_from_the_merges_in_an_orgnode_forest()
    ↓
[MERGED node, Updated acquirer, Acquiree deletion] SaveInstructions
```

**Next Step:** These SaveInstructions need to be integrated into the save flow, which will:
1. Apply them via `update_graph` (normal save path)
2. Then perform additional merge operations (relationship rerouting, etc.)

---

## Test Files

### Unit Tests
- `tests/merge/saveinstructions_from_the_merges_in_an_orgnode_forest.rs`
  - Test: `test_single_merge`
  - Status: ✅ PASSING

### Test Fixtures
- `tests/merge/saveinstructions_from_the_merges_in_an_orgnode_forest/fixtures/1.skg` (acquirer)
- `tests/merge/saveinstructions_from_the_merges_in_an_orgnode_forest/fixtures/2.skg` (acquiree)

### Test Module
- `tests/merge.rs` - module registration file

---

## Key Insights from Implementation

1. **ego_tree traversal:** Must match on `Edge::Open(node_ref)`, not use `.value()` directly
2. **File reading:** Use `read_node(&path_from_pid(config, id))` - no need for a wrapper function since we already have the primary ID
3. **ID handling:** Mix of owned `ID` and `&ID` - need to be careful with borrows
4. **Merge timing:** Merge requests are processed during save, not during rebuild/view
5. **SaveInstruction approach:** Creating SaveInstructions for MERGED nodes allows them to flow through normal save infrastructure (will get TypeDB, FS, and Tantivy handling automatically)
6. **Existing utilities:** `file_io` module has `read_node_from_id` but it's async and does DB lookup via `pid_from_id` first - not needed when we already have the primary ID

---

## Function Signatures

### Main Function
```rust
pub async fn saveinstructions_from_the_merges_in_an_orgnode_forest(
    forest: &[Tree<OrgNode>],
    config: &SkgConfig,
    _driver: &TypeDBDriver,
) -> Result<Vec<SaveInstruction>, Box<dyn Error>>
```

### Helper Functions
```rust
fn create_merged_node(acquiree: &SkgNode) -> SkgNode
```

---

## Files Modified

### New Files
- `rust/merge.rs` (module for merge functionality)
- `tests/merge.rs` (test module)
- `tests/merge/saveinstructions_from_the_merges_in_an_orgnode_forest.rs`
- `tests/merge/saveinstructions_from_the_merges_in_an_orgnode_forest/fixtures/*.skg`

### Modified Files
- `rust/lib.rs` - added `pub mod merge;`
- `rust/types/orgnode.rs` - added Merge variant, updated Display/FromStr
- `rust/serve/parse_headline_md_sexp.rs` - updated parse_requests_sexp
- `rust/rebuild.rs` - added Merge pattern match case

---

## Next Steps for Implementation

Following the merge-plan.md phases in order:

1. **Complete Phase 1:** Add remaining type changes (NodeSaveAction.merge field, error variants)
2. **Implement Phase 3:** Validation logic (most complex part)
3. **Integrate SaveInstruction generation:** Hook into save flow
4. **Implement Phase 5:** The core merge logic (TypeDB, FS, Tantivy operations)
5. **Implement Phase 6:** Forest cleanup and integration
6. **Expand Phase 7:** Comprehensive testing
