# Phase 5: path_from_pid_and_source Call Site Analysis

**STATUS: ✅ COMPLETED - 2025-11-12**

All recommendations in this document have been implemented:
- `path_from_pid_and_source` now accepts source parameter
- `pid_and_source_from_id` helper function created for TypeDB queries
- All call sites updated accordingly
- TODO in reconcile_dup_instructions.rs resolved with source validation

This document remains for historical reference.

---

## Question
Do callers of `path_from_pid_and_source` have access to the source nickname, or do they need to query TypeDB?

## Answer
**Both scenarios exist.** We need two different approaches:

1. **For callers with the node object**: Add `source` parameter to `path_from_pid_and_source`
2. **For callers with only an ID**: Create helper that queries TypeDB for source

---

## Category A: Callers Have the Node Object ✓ Can Pass node.source

These callers already have a `SkgNode` object with a `source` field:

### [rust/merge/fs.rs:26-36](../rust/merge/fs.rs#L26) - Writing merge results
**Context:** Has `acquiree_text_preserver`, `updated_acquirer`, and `acquiree` node objects
**Solution:** Pass `node.source` to `path_from_pid_and_source`
```rust
path_from_pid_and_source(&config, &acquiree_text_preserver.source, acquiree_text_preserver.ids[0].clone())
path_from_pid_and_source(&config, &updated_acquirer.source, updated_acquirer.ids[0].clone())
path_from_pid_and_source(&config, &acquiree.source, acquiree.ids[0].clone())
```

### [rust/media/file_io/multiple_nodes.rs:211](../rust/media/file_io/multiple_nodes.rs#L211) - write_all_nodes_to_fs
**Context:** Iterating over `nodes`, has `node` object
**Solution:** Pass `node.source` to `path_from_pid_and_source`
```rust
path_from_pid_and_source(&config, &node.source, pid)
```

### [rust/media/file_io/multiple_nodes.rs:227](../rust/media/file_io/multiple_nodes.rs#L227) - delete_all_nodes_from_fs
**Context:** Iterating over `nodes`, has `node` object
**Solution:** Pass `node.source` to `path_from_pid_and_source`
```rust
path_from_pid_and_source(&config, &node.source, pid)
```

---

## Category B: Callers Only Have ID ⚠ Need TypeDB Lookup

These callers only have an ID and need to find which source contains it:

### [rust/media/file_io/one_node.rs:24](../rust/media/file_io/one_node.rs#L24) - read_node_from_id
**Context:** Has PID from TypeDB query, needs to read node
**Current approach:**
1. Query TypeDB for PID using `pid_from_id`
2. Call `path_from_pid_and_source(config, pid)`
3. Read node from path

**Solution:** Modify `pid_from_id` to also return source, OR create new helper function

### [rust/media/file_io/one_node.rs:59](../rust/media/file_io/one_node.rs#L59) - fetch_aliases_from_file
**Context:** Has ID, needs to read node
**Solution:** Same as above

### [rust/merge/mergeInstructionTriple.rs:56,60](../rust/merge/mergeInstructionTriple.rs#L56) - Reading nodes for merge
**Context:** Has acquirer_id and acquiree_id, needs to read both nodes
**Current:**
```rust
read_node(&path_from_pid_and_source(config, acquirer_id.clone()))?
read_node(&path_from_pid_and_source(config, acquiree_id.clone()))?
```
**Solution:** Need to query TypeDB for source before constructing path

### [rust/rebuild/complete_aliascol.rs:37](../rust/rebuild/complete_aliascol.rs#L37) - Reading parent node
**Context:** Has parent_id, needs to read node
**Solution:** Query TypeDB for source

### [rust/rebuild/complete_contents.rs:237](../rust/rebuild/complete_contents.rs#L237) - Reading node
**Context:** Has node_pid, needs to read node
**Solution:** Query TypeDB for source

### [rust/mk_org_text/content_view.rs:188,211](../rust/mk_org_text/content_view.rs#L188) - Content view (2 places)
**Context:** Has PID/ID, needs to read node
**Solution:** Query TypeDB for source

### [rust/save/orgnodes_to_instructions/none_node_fields_are_noops.rs:38](../rust/save/orgnodes_to_instructions/none_node_fields_are_noops.rs#L38)
**Context:** Has primary_id, needs to read node
**Solution:** Query TypeDB for source

---

## Category C: Special Case - Already Has Source ✓ Easy Fix

### [rust/save/orgnodes_to_instructions/reconcile_dup_instructions.rs:100-113](../rust/save/orgnodes_to_instructions/reconcile_dup_instructions.rs#L100)
**Context:** Already computed `reconciled_source_value` (line 100-102)
**Current:** Has TODO comment saying to use it
**Solution:** Just use the already-computed source value!
```rust
// Line 100-102: Already has reconciled_source_value
// Line 112-113: Pass it to read_node_from_id
read_node_from_id_with_source(config, driver, &primary_id, &reconciled_source_value).await?
```

---

## Recommended Implementation Strategy

### Step 1: Modify path_from_pid_and_source signature
```rust
pub fn path_from_pid_and_source(
  config: &SkgConfig,
  source: &str,      // NEW parameter
  pid: ID,
) -> String
```

### Step 2: Create TypeDB helper to get source

**See [phase5-batching-analysis.md](phase5-batching-analysis.md) for detailed performance analysis.**

Create single-ID lookup helper (batching can be added later if needed):
```rust
pub async fn pid_and_source_from_id(
  db_name: &str,
  driver: &TypeDBDriver,
  node_id: &ID
) -> Result<Option<(ID, String)>, Box<dyn Error>>
```

**Rationale:** Most Category B call sites process single IDs. Start simple, optimize later if profiling shows batching is beneficial.

### Step 3: Update call sites by category
- **Category A (3 call sites):** Add `&node.source` parameter
- **Category B (8 call sites):** Use helper that queries TypeDB for source
- **Category C (1 call site):** Pass the already-computed `reconciled_source_value`

---

## Summary

**Total call sites:** 12
- **3** already have the node → simple parameter addition
- **8** need TypeDB lookup → use new helper function
- **1** already computed source → use it

**Key insight:** We need BOTH approaches:
1. `path_from_pid_and_source(config, source, pid)` - for when you have the source
2. Helper functions that query TypeDB for source - for when you only have an ID
