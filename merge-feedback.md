# Merge Implementation - Questions and Observations

**Date:** 2025-10-27
**Status:** ✅ ALL CRITICAL QUESTIONS ANSWERED - Ready for implementation

## Summary

All critical questions have been resolved:
1. ✅ SkgNode structure identified (rust/types/skgnode.rs)
2. ✅ Title/body storage confirmed (FS-only)
3. ✅ Relationship rerouting rules specified for all 5 types
4. ✅ Filesystem path utilities found (path_from_pid in util.rs)
5. ✅ ego_tree API confirmed (child_mut.detach() in complete_contents.rs)
6. ✅ Copy trait issue explained (String prevents Copy for ID)
7. ✅ MERGED node handling resolved (via SaveInstructions)

**Implementation can proceed!** See merge-plan.md:1 for full implementation guide.

---

## Critical Questions - ANSWERED ✓

### 1. SkgNode Structure ✓ RESOLVED

**Answer:** Found in `rust/types/skgnode.rs:6-32`

```rust
pub struct SkgNode {
  pub title: String,
  pub aliases: Option<Vec<String>>,
  pub ids: Vec<ID>,  // Must be nonempty; can be > 1 due to merges
  pub body: Option<String>,
  pub contains: Vec<ID>,  // The content list!
  pub subscribes_to: Vec<ID>,
  pub hides_from_its_subscriptions: Vec<ID>,
  pub overrides_view_of: Vec<ID>,
}
```

**Key insight:** The field is `contains`, not `content`. This is the list of contained node IDs.

### 2. Title and Body Storage ✓ RESOLVED

**Answer:** Just FS. (User confirmed)

Title and body are stored ONLY in the filesystem (.skg files), not in TypeDB.

**Implication:** `fetch_node_data` needs to:
1. Read acquiree's .skg file to get title/body
2. Query TypeDB for `contains` relationships to get content IDs
3. Query TypeDB for extra_ids

**File format:** SkgNode is serialized/deserialized using serde (see skgnode.rs:1)
- Function `read_skg_files` likely handles reading
- Can deserialize directly to SkgNode struct

### 3. Relationship Rerouting ✓ RULES CLARIFIED

**User provided specific rules for each relationship type:**

#### contains (container/contained)
Transfer wholesale - already described in original plan.

#### hyperlinks_to (source/dest)
**Transfer wholesale:**
- If acquiree was source → acquirer becomes source
- If acquiree was dest → acquirer becomes dest

#### subscribes (subscriber/subscribee)
**Transfer wholesale:**
- If acquiree was subscriber → acquirer becomes subscriber
- If acquiree was subscribee → acquirer becomes subscribee

#### hides_from_its_subscriptions (hider/hidden)
**Conditional logic:**
- If acquiree is `hidden` → **DROP the relationship**
- If acquiree is `hider` and N is `hidden`:
  - If N is NOT in acquirer's contents → transfer (acquirer becomes hider)
  - If N IS in acquirer's contents → **DROP the relationship**

**Why drop when N is in contents?** Because hiding from your own content doesn't make sense.

#### overrides_view_of (replacement/replaced)
**Conditional logic:**
- If acquiree is `replacement` → transfer (acquirer becomes replacement)
- If acquiree is `replaced` → **DROP the relationship**

**Why drop when replaced?** The acquiree no longer exists to be replaced.

**Implementation strategy:** Each relationship type needs its own handler function with specific logic.

### 4. Filesystem File Organization ✓ RESOLVED

**Answer:** Found in `rust/util.rs:4-9` and `rust/file_io/update_fs.rs:62-63`

**Function:** `path_from_pid(config: &SkgConfig, pid: ID) -> String`

**Usage example from update_fs.rs:**
```rust
let file_path = path_from_pid(&config, pid);
```

This function constructs the full path from config.skg_folder and the ID.

**File operations already exist:**
- `write_node(&node, &Path::new(&path_from_pid(&config, pid)))` - writes a node
- `fs::remove_file(&path_from_pid(&config, pid))` - deletes a node
- Reading: likely via `read_skg_files` or similar in file_io module

**For merge implementation:**
```rust
// Read acquiree file
let acquiree_path = path_from_pid(config, acquiree_id);
let acquiree_node = read_node_from_file(&acquiree_path)?;

// Write updated acquirer
let acquirer_path = path_from_pid(config, acquirer_id);
write_node(&updated_acquirer, &Path::new(&acquirer_path))?;

// Delete acquiree file
fs::remove_file(&path_from_pid(config, acquiree_id))?;
```

---

### 5. Validation Function Signature Change

**Impact:** Making `find_buffer_errors_for_saving` async

**Current (validate_tree.rs:16):**
```rust
pub fn find_buffer_errors_for_saving (
  trees: &[Tree<OrgNode>]
) -> Vec<Buffer_Cannot_Be_Saved>
```

**Needed:**
```rust
pub async fn find_buffer_errors_for_saving (
  trees: &[Tree<OrgNode>],
  db_name: &str,
  driver: &TypeDBDriver
) -> Vec<Buffer_Cannot_Be_Saved>
```

**Reason:** Merge validation needs to query TypeDB to check if acquiree exists.

**Call site to update:** `rust/save.rs:56-60`

Current:
```rust
let validation_errors : Vec<Buffer_Cannot_Be_Saved> =
  find_buffer_errors_for_saving ( & orgnode_forest );
```

Needs to become:
```rust
let validation_errors : Vec<Buffer_Cannot_Be_Saved> =
  find_buffer_errors_for_saving ( & orgnode_forest, & config . db_name, driver )
  . await . map_err ( SaveError::DatabaseError ) ?;
```

---

### 6. Forest Node Removal Details ✓ RESOLVED

**Answer:** Found in `rust/rebuild/complete_contents.rs:189-216`

The `ego_tree` library provides the following API:
- `tree.root_mut()` returns `NodeMut`
- `node_mut.children()` returns iterator of child `NodeRef`s
- `node_mut.detach()` removes a node from its parent

**Working example from codebase:**
The function `replace_subtree_with_node` (complete_contents.rs:189-216) demonstrates the pattern:

```rust
// Collect child IDs before mutating
let child_ids: Vec<NodeId> = {
  let node_ref: ego_tree::NodeRef<OrgNode> =
    tree.get(old_node_id).ok_or("Old node not found")?;
  node_ref.children()
    .map(|c| c.id())
    .collect()
};

// Remove all children
for child_id in child_ids {
  let mut child_mut: NodeMut<OrgNode> =
    tree.get_mut(child_id).ok_or("Child not found")?;
  child_mut.detach();  // <-- This removes the node
}
```

**For merge implementation:**
- Collect IDs of acquiree nodes to remove
- Use `forest.retain()` to remove top-level trees
- Use `child_mut.detach()` to remove nodes from within trees
- Recurse through all children to handle nested acquirees

---

## Technical Observations

### 1. NodeSaveAction Loses Copy Trait

Adding `merge: Option<ID>` means `NodeSaveAction` can no longer derive `Copy`.

**Root cause:** ID is defined as `pub struct ID(pub String)` in rust/types/misc.rs:19, and `String` is not `Copy` because it's heap-allocated. Therefore:
- `ID` cannot derive `Copy`
- `Option<ID>` cannot be `Copy`
- `NodeSaveAction` containing `Option<ID>` cannot be `Copy`

Current (save.rs:12):
```rust
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct NodeSaveAction { ... }
```

Must become:
```rust
#[derive(Clone, Debug, PartialEq)]
pub struct NodeSaveAction { ... }
```

**Alternative:** If `Copy` is essential, ID's internal representation could be changed to `u64` or similar stack-allocated type. However, this would require changing the entire codebase's ID representation strategy.

**Impact:** All code that copies `NodeSaveAction` must be reviewed. Most likely just needs `.clone()` added in a few places.

---

### 2. Two-Write Strategy Clarity

You mentioned wanting acquirer written twice for code clarity:
1. First: Apply user's direct edits (via `update_graph`)
2. Second: Apply merge (via `merge_in_graph`)

**This is reflected in the plan:**
- `update_graph` runs first (processes SaveInstructions including acquirer edits)
- `merge_in_graph` runs second (modifies acquirer with merge data)

**File writes:**
- First write: acquirer's file updated with user edits (update_fs_from_saveinstructions)
- Second write: acquirer's file updated again with merged content (merge_in_fs)

The cost is minimal (one extra file write) and the code separation is clean.

---

### 3. Synthetic MERGED Node via SaveInstructions ✓ RESOLVED

**Decision:** MERGED node SHOULD be created via SaveInstructions for consistency with the rest of the codebase.

**User's reasoning:** "Just as update_graph does, merge_in_graph should first create all new nodes, next perform the edits on the others, and last perform deletes. That way the edits are not handling things that don't exist yet."

**Implementation approach:**
1. During `orgnodes_to_save_instructions` or early in `merge_in_graph`:
   - For each merge instruction, create MERGED node
   - Add SaveInstruction for MERGED node to the instructions list

2. Order of operations in `merge_in_graph`:
   - **First:** Create all new nodes (MERGED nodes via SaveInstructions)
   - **Next:** Perform edits (acquirer updates, relationship rerouting)
   - **Last:** Perform deletes (acquiree nodes)

3. This ensures:
   - MERGED node goes through normal `update_graph` flow
   - MERGED node gets TypeDB, FS, and Tantivy handling
   - All edits can reference nodes that already exist
   - Consistent with existing codebase patterns

**Benefits:**
- Consistency with existing save infrastructure
- Automatic Tantivy indexing
- Simpler error handling
- Less special-case code

---

### 4. Circular Merge Detection Algorithm

For detecting cycles like A→B→C→A, here's the algorithm:

```rust
fn detect_merge_cycle(
  merge_map: &HashMap<ID, ID> // acquirer -> acquiree
) -> Option<Vec<ID>> {
  let mut visited = HashSet::new();
  let mut rec_stack = HashSet::new();
  let mut path = Vec::new();

  for start_id in merge_map.keys() {
    if let Some(cycle) = dfs(
      start_id,
      merge_map,
      &mut visited,
      &mut rec_stack,
      &mut path
    ) {
      return Some(cycle);
    }
  }
  None
}

fn dfs(
  current: &ID,
  merge_map: &HashMap<ID, ID>,
  visited: &mut HashSet<ID>,
  rec_stack: &mut HashSet<ID>,
  path: &mut Vec<ID>,
) -> Option<Vec<ID>> {
  visited.insert(current.clone());
  rec_stack.insert(current.clone());
  path.push(current.clone());

  if let Some(next) = merge_map.get(current) {
    if rec_stack.contains(next) {
      // Found cycle - extract it from path
      let cycle_start = path.iter().position(|id| id == next).unwrap();
      return Some(path[cycle_start..].to_vec());
    }

    if !visited.contains(next) {
      if let Some(cycle) = dfs(next, merge_map, visited, rec_stack, path) {
        return Some(cycle);
      }
    }
  }

  rec_stack.remove(current);
  path.pop();
  None
}
```

This will catch cycles and return the IDs involved in the cycle for the error message.

---

### 5. Order of Merges - Does It Matter?

**Scenario:** Buffer has two merges: A→B and C→D

**Question:** Should they be processed in any particular order?

**Answer:** No, as long as there are no dependencies:
- A and C are different nodes
- B and D are different nodes
- No cycles (validated earlier)

They can be processed in any order or even in parallel (though sequential is simpler).

**Special case to watch:** If someone tries A→B and B→C in the same save (which should be caught by "acquiree also merging" validation).

---

## Suggestions for Testing

### Unit Tests

1. **Cycle detection:**
   - A→B→C→A should error
   - A→B, C→D (independent) should pass
   - A→B→C (chain without cycle) should pass

2. **Conflict detection:**
   - Node with both `merge` and `toDelete` should error
   - Acquirer marked for deletion should error
   - Acquiree marked for deletion should error

3. **Filesystem operations:**
   - Read .skg file
   - Write merged .skg file
   - Delete .skg file

4. **Relationship rerouting:**
   - Test each relationship type independently
   - Verify counts before/after

### Integration Test Structure

```
tests/integration/node-merge/
  data/
    skg-data/
      <node-a-id>.skg    # Acquirer
      <node-b-id>.skg    # Acquiree
  test-config.toml
  test-emacs.el          # Issues merge request and saves
  run-test.sh
```

**Test flow:**
1. Start with two nodes: A (title: "Acquirer") and B (title: "Acquiree")
2. A has content: [X, Y]
3. B has content: [Z]
4. Add `(merge B-ID)` to A's metadata
5. Save buffer
6. Verify in returned view:
   - A exists with 3 children: MERGED, X, Y, Z
   - MERGED child has title "MERGED: Acquiree" and B's body
   - B does not appear
7. Verify in database:
   - A has extra_id = B's ID
   - Query for B returns A (via extra_id)
   - A's contains relationships include all of: MERGED, X, Y, Z
8. Verify in filesystem:
   - A's file has correct IDs and content
   - B's file is deleted
   - MERGED node's file exists

---

## Potential Edge Cases

### 1. Acquirer Has No Content

If acquirer has no children, after merge it should have:
- MERGED node
- Acquiree's children

This is handled by the formula: `[MERGED] + [] + [acquiree content]`

### 2. Acquiree Has No Content

If acquiree has no children:
- MERGED node still created (with acquiree's title/body)
- Acquirer gains MERGED as first child
- Acquirer's other children remain

Formula: `[MERGED] + [acquirer content] + []`

### 3. Both Have No Content

Both have no children:
- Acquirer gains MERGED node as only child
- MERGED node contains acquiree's title/body
- This is valid

### 4. Multiple Merges Into Same Acquirer

**Scenario:** A→B and A→C (A wants to merge both B and C)

**Question:** Is this allowed?

Current plan assumes one merge per acquirer. If multiple are allowed, need to:
- Process them sequentially
- Each adds its own MERGED node
- Order matters (MERGED-B, then acquirer's content, then B's content, then MERGED-C, then C's content)

**Recommendation:** Disallow for first version. Add validation error if node has multiple `Merge` requests in its `nodeRequests` set.

### 5. Acquiree Is in Current Buffer

If acquiree appears in the current buffer orgnode forest:
- Validation sees both nodes
- Can validate acquiree doesn't have `toDelete` or `Merge` of its own
- After `update_graph`, both are updated with user edits
- After `merge_in_graph`, acquiree is merged into acquirer
- Before rendering, acquiree is removed from forest (via `remove_merged_nodes_from_forest`)

This should work correctly!

### 6. Acquiree Has Existing Extra IDs

From user's feedback: "Moreover, it might be that the acquiree itself has extra_ids! So we should adjust those relationships too -- they should be transferred to the acquirer."

**Handled in plan:** `merge_in_typedb` transfers acquiree's extra_ids:
```rust
for extra_id in &acquiree_data.extra_ids {
  add_extra_id(&tx, acquirer_id, extra_id).await?;
}
```

This means if:
- Acquiree has extra_ids [E1, E2]
- After merge, acquirer has extra_ids [B-id, E1, E2]

All three IDs now resolve to acquirer. ✓

---

## Implementation Complexity Estimate

**Straightforward:**
- Type changes (Phase 1)
- Parsing changes (Phase 2)
- Validation framework (Phase 3)
- Extract merge info (Phase 4)
- Integration (Phase 6)
- Testing (Phase 7)

**Medium Complexity:**
- Validation logic (cycles, conflicts)
- Forest node removal
- Filesystem operations

**High Complexity:** ⚠️
- **Relationship rerouting** (Phase 5.3)
  - Must handle 5 relationship types
  - Each has bidirectional cases
  - TypeDB query construction for each
  - Must preserve all relationship data
  - This is the most error-prone part

**Estimate:**
- Phases 1-4, 6-7: 1-2 days
- Phase 5 (except 5.3): 1 day
- Phase 5.3 (relationships): 2-3 days
- **Total: 4-6 days** for experienced developer

---

## Recommendations

### Start Simple
1. Implement Phases 1-4 first (types, parsing, validation, extraction)
2. Test validation thoroughly before moving to merge logic
3. Implement merge logic for one relationship type (e.g., `contains`)
4. Test that thoroughly
5. Extend to other relationship types

### Consider Breaking Into Smaller PRs
1. PR 1: Types and parsing
2. PR 2: Validation
3. PR 3: Merge logic (TypeDB only, one relationship type)
4. PR 4: Extend to all relationship types
5. PR 5: Filesystem merge
6. PR 6: Tantivy integration
7. PR 7: Integration tests

### Error Handling
Add detailed logging throughout merge operations:
- Log each relationship found
- Log each relationship deleted
- Log each relationship created
- This will be invaluable for debugging

### Documentation
Document the merge behavior in user-facing docs:
- What happens during a merge
- What the MERGED node is
- How extra_ids work
- Limitations (e.g., one merge per node)
