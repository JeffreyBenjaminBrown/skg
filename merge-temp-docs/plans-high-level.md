# Merge Feature - High-Level Plans

**Date:** 2025-10-27

---

## Goal

Allow users to merge two nodes by adding merge metadata to the acquirer node.

## Terminology

- **Acquirer**: The node that remains after the merge (has the `merge` instruction)
- **Acquiree**: The node that will be subsumed into the acquirer (specified by ID in the merge instruction)

## User Experience

User adds metadata to acquirer: `(skg (code (requests (merge <ID>))))`

When they save the buffer, the merge happens automatically:
1. Acquirer gains the acquiree's ID as an extra ID
2. Acquirer gains a "MERGED" child node containing the acquiree's title/body
3. Acquirer's contents become: [MERGED] + acquirer's old contents + acquiree's old contents
4. Acquiree is deleted
5. All relationships pointing to acquiree are rerouted to acquirer (with type-specific rules)

## Implementation Phases

### Phase 1: Type Changes
- Add `Merge(ID)` to `NodeRequest` enum ✅
- Add `merge: Option<ID>` to `NodeSaveAction`
- Add merge-related error variants to `Buffer_Cannot_Be_Saved`
- Remove `Copy` trait from `NodeSaveAction` (since `Option<ID>` is not Copy)

### Phase 2: Parsing
- Parse `(merge <id>)` syntax in metadata ✅

### Phase 3: Validation
- Check for merge+delete conflicts
- Verify acquiree exists in DB
- Detect circular merge chains (A→B→C→A)
- Verify acquiree isn't also trying to merge
- Make `find_buffer_errors_for_saving` async (needs DB access)

### Phase 4: SaveInstruction Generation
- Create SaveInstructions for merges ✅ (via `saveinstructions_from_the_merges_in_an_orgnode_forest`)

### Phase 5: Core Merge Operations
- Reroute relationships in TypeDB
- Update filesystem (acquirer file, delete acquiree file)
- Update Tantivy index

### Phase 6: Integration
- Call merge functions in save flow
- Remove acquiree nodes from orgnode forest before rendering

### Phase 7: Testing
- Unit tests ✅ (SaveInstruction generation)
- Integration tests
- Relationship-specific tests

---

## Relationship Rerouting Rules

### contains (container/contained)
Transfer wholesale - if acquiree was container → acquirer becomes container, if contained → acquirer becomes contained

### hyperlinks_to (source/dest)
Transfer wholesale - if acquiree was source → acquirer becomes source, if dest → acquirer becomes dest

### subscribes (subscriber/subscribee)
Transfer wholesale - if acquiree was subscriber → acquirer becomes subscriber, if subscribee → acquirer becomes subscribee

### hides_from_its_subscriptions (hider/hidden)
**Conditional:**
- If acquiree is `hidden` → **DROP the relationship** (acquiree no longer exists)
- If acquiree is `hider` and N is `hidden`:
  - If N is NOT in acquirer's contents → transfer (acquirer becomes hider)
  - If N IS in acquirer's contents → **DROP** (can't hide from your own content)

### overrides_view_of (replacement/replaced)
**Conditional:**
- If acquiree is `replacement` → transfer (acquirer becomes replacement)
- If acquiree is `replaced` → **DROP** (acquiree no longer exists to be replaced)

---

## Data Storage

- **Title and body**: Filesystem only (.skg files)
- **Relationships**: TypeDB only
- **IDs**: Both (primary ID in filesystem, extra_ids in TypeDB via has_extra_id relationship)
- **Contents list**: Both (SkgNode.contains field in filesystem, TypeDB contains relationships)

---

## Architecture

### Current (Implemented)
```
orgnode forest (with merge requests)
    ↓
saveinstructions_from_the_merges_in_an_orgnode_forest()
    ↓
[MERGED node, Updated acquirer, Acquiree deletion] SaveInstructions
```

### Planned Full Flow
```
buffer_to_save_instructions()
    ↓
validation (including merge validation)
    ↓
SaveInstructions generated (including merge instructions)
    ↓
update_graph() - applies SaveInstructions
    ↓
merge_in_graph() - performs relationship rerouting, etc.
    ↓
remove_merged_nodes_from_forest() - so acquiree doesn't appear in view
    ↓
render to org-mode text
```

---

## Edge Cases to Handle

1. **Acquirer has no content**: Result = [MERGED] + [] + acquiree's content
2. **Acquiree has no content**: Result = [MERGED] + acquirer's content + []
3. **Both have no content**: Result = [MERGED] only
4. **Multiple merges in same save**: Process sequentially
5. **Acquiree is in current buffer**: Works correctly - removed before rendering
6. **Acquiree has extra_ids**: Transfer all extra_ids to acquirer

---

## Key Design Decisions

1. **MERGED node via SaveInstructions**: Allows normal save infrastructure to handle it (TypeDB, FS, Tantivy)
2. **Order of operations**: Create (MERGED nodes) → Edit (reroute relationships) → Delete (acquirees)
3. **Two-write strategy**: Acquirer written once for user edits, once for merge (code clarity)
4. **ego_tree API**: Use `child_mut.detach()` to remove nodes from forest
5. **ID handling**: String-based IDs prevent Copy trait, must use Clone
