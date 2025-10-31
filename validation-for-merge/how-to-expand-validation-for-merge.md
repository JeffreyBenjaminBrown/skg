# Merge Validation Analysis

## Review of Existing Validation Document

The `how-save-buffer-works.md` document comprehensively covers the existing validation infrastructure:

1. **find_inconsistent_instructions**: Checks for contradictory deletion instructions and multiple defining containers
2. **validate_node_and_children**: Recursively validates structural rules for AliasCol/Alias nodes, parent-child relationships, and duplicate content

All validation occurs in `find_buffer_errors_for_saving` after PID resolution but before converting to SaveInstructions.

---

## Merge Validation Rules from Planning Docs

The planning documents identified these validation rules for merges:

### 1. **Merge + Delete Conflict** ✓ (from plans)
- **Rule**: A node cannot have both `toDelete=true` AND a `Merge` request
- **Error**: `Buffer_Cannot_Be_Saved::Merge_And_Delete_Conflict(OrgNode)`
- **Rationale**: Contradictory intent - can't both delete and merge

### 2. **Acquiree Marked for Deletion** ✓ (from plans)
- **Rule**: The merge target (acquiree) cannot be marked for deletion anywhere in the buffer
- **Error**: `Buffer_Cannot_Be_Saved::Acquiree_Has_Delete(ID)`
- **Rationale**: Can't merge into something that's being deleted

### 3. **Acquiree Doesn't Exist** ✓ (from plans)
- **Rule**: The merge target must exist in the database
- **Error**: `Buffer_Cannot_Be_Saved::Acquiree_Not_Found(ID)`
- **Rationale**: Can't merge with non-existent node
- **Note**: Requires database query

### 4. **Circular Merge Chains** ✓ (from plans)
- **Rule**: No cycles in merge dependencies (A→B→C→A)
- **Error**: `Buffer_Cannot_Be_Saved::Circular_Merge_Detected(Vec<ID>)`
- **Rationale**: Impossible to resolve which merges into what

### 5. **Acquiree Also Merging** ✓ (from plans)
- **Rule**: The acquiree cannot itself have a merge request
- **Error**: `Buffer_Cannot_Be_Saved::Acquiree_Also_Merging(ID)`
- **Rationale**: Simplifies implementation (no transitive merges)
- **Note**: This also prevents merge chains like A→B→C (even without cycles)

### 6. **Multiple Merge Requests on Same Node** ✓ (from other-ideas.md)
- **Rule**: A node's `nodeRequests` set cannot contain multiple `Merge(ID)` requests
- **Error**: `Buffer_Cannot_Be_Saved::Multiple_Merge_Requests(OrgNode, Vec<ID>)`
- **Rationale**: Unclear semantics - which merge happens first?
- **Note**: Disallow for now, could be supported later with explicit ordering

---

## Additional Validation Rules from De-Novo Analysis

### 7. **indefinitive + merge** ⚠️ NEW
- **Rule**: A node with `indefinitive=true` cannot request a merge
- **Error**: `Buffer_Cannot_Be_Saved::Indefinitive_Node_Cannot_Merge(OrgNode)`
- **Rationale**:
  - `indefinitive` means "this is just a partial view, don't use it to define the node's contents"
  - Merge is a very definite, destructive operation
  - Allowing indefinitive nodes to merge creates ambiguity about what's actually being merged
- **Severity**: MEDIUM - semantically unclear what it means

### 8. **repeat + merge** ⚠️ NEW
- **Rule**: A node with `repeat=true` cannot request a merge
- **Error**: `Buffer_Cannot_Be_Saved::Repeated_Node_Cannot_Merge(OrgNode)`
- **Rationale**:
  - `repeat` indicates the node appears elsewhere in the buffer
  - Which instance is actually performing the merge?
  - Would create confusion about which view's metadata is authoritative
- **Severity**: HIGH - creates ambiguity about which instance is acting
- **Note**: This is automatically prevented because `change_repeated_to_indefinitive` converts repeat→indefinitive before validation, and rule #7 catches it

### 9. **Alias/AliasCol nodes cannot merge** ⚠️ NEW
- **Rule**: A node with `relToParent ∈ {Alias, AliasCol}` cannot request a merge
- **Error**: `Buffer_Cannot_Be_Saved::Alias_Cannot_Merge(OrgNode)`
- **Rationale**:
  - Alias/AliasCol nodes have special structural meanings
  - They're not "real" nodes in the graph, just metadata
  - Merging them doesn't make semantic sense
- **Severity**: HIGH - violates the alias abstraction

### 10. **Merge acquirer must have ID** ⚠️ NEW
- **Rule**: A node requesting a merge must have an ID (the acquirer ID)
- **Error**: `Buffer_Cannot_Be_Saved::Merge_Requester_Missing_ID(OrgNode)`
- **Rationale**:
  - The merge operation needs to know which node is the acquirer
  - Without an ID, there's no node to transfer relationships to
- **Severity**: CRITICAL - merge cannot proceed without it

### 11. **Self-merge** ⚠️ NEW (probably redundant)
- **Rule**: A node cannot merge with itself (merge target ≠ node's own ID)
- **Error**: `Buffer_Cannot_Be_Saved::Self_Merge_Detected(ID)`
- **Rationale**: Nonsensical operation
- **Severity**: MEDIUM
- **Note**: Likely caught by circular detection (#4), but explicit check is clearer

### 12. **Multiple instances of same node with different merge targets** ⚠️ NEW
- **Rule**: If multiple nodes in the buffer have the same ID (e.g., A1 and A2 both have ID "A"), they cannot have different merge requests
- **Error**: `Buffer_Cannot_Be_Saved::Conflicting_Merge_Requests_For_Same_ID(ID, Vec<ID>)`
- **Rationale**:
  - Which merge should actually execute?
  - Similar to the existing "Multiple Defining Containers" error
- **Severity**: HIGH - creates ambiguity
- **Note**: This is related to but distinct from #6 - that's about one node having multiple requests, this is about multiple views of same node having different requests

---

## Interactions with Existing Validation

### Existing: AmbiguousDeletion
- **Existing rule**: Multiple nodes with same ID cannot have conflicting `toDelete` values
- **Merge interaction**: Works well - rule #2 (acquiree marked for deletion) integrates naturally

### Existing: Multiple_DefiningContainers
- **Existing rule**: Multiple nodes with same ID cannot both have `indefinitive=false` and `repeat=false`
- **Merge interaction**:
  - Good - prevents ambiguous merge sources via rule #12
  - A definitive node can merge, an indefinitive node cannot (rule #7)

### Existing: DuplicatedContent
- **Existing rule**: A definitive node cannot have duplicate content children with same ID
- **Merge interaction**:
  - After merge, acquirer's contents = [MERGED node] + old acquirer contents + acquiree contents
  - Could theoretically create duplicates if acquirer and acquiree both contained the same node
  - **NEEDS NEW RULE #13 below**

### 13. **Post-merge duplicate content** ⚠️ NEW
- **Rule**: After merging, the acquirer cannot end up with duplicate content
- **Check**: `acquirer.contains ∪ acquiree.contains` must have no duplicates
- **Error**: `Buffer_Cannot_Be_Saved::Merge_Would_Create_Duplicate_Content(acquirer_id, duplicate_id)`
- **Rationale**: Violates the existing content uniqueness constraint
- **Severity**: MEDIUM - violates data integrity
- **Note**: This is a validation that requires looking at disk state (acquiree's contents)

---

## Summary of Validation Rules

### From Planning Docs (all included)
1. Merge + Delete Conflict ✓
2. Acquiree Marked for Deletion ✓
3. Acquiree Doesn't Exist (requires DB query) ✓
4. Circular Merge Chains ✓
5. Acquiree Also Merging ✓
6. Multiple Merge Requests on Same Node ✓

### Newly Identified
7. Indefinitive Node Cannot Merge ⚠️ NEW
8. Repeated Node Cannot Merge ⚠️ NEW (auto-prevented by #7)
9. Alias/AliasCol Cannot Merge ⚠️ NEW
10. Merge Requester Must Have ID ⚠️ NEW
11. Self-Merge ⚠️ NEW (redundant with #4)
12. Conflicting Merge Requests For Same ID ⚠️ NEW
13. Post-Merge Duplicate Content ⚠️ NEW

### Priority Recommendations

**CRITICAL (must implement):**
- #10: Merge requester must have ID

**HIGH (should implement):**
- #7: Indefinitive node cannot merge
- #9: Alias/AliasCol cannot merge
- #12: Conflicting merge requests for same ID

**MEDIUM (nice to have):**
- #11: Self-merge (likely redundant with circular detection)
- #13: Post-merge duplicate content (edge case, but good to catch)

**LOW (already handled):**
- #8: Repeated node (handled by #7 after repeat→indefinitive conversion)

---

## Implementation Notes

### Location
All merge validation should go in a new function:
```rust
pub async fn find_merge_errors(
  trees: &[Tree<OrgNode>],
  db_name: &str,
  driver: &TypeDBDriver
) -> Vec<Buffer_Cannot_Be_Saved>
```

This should be called from `find_buffer_errors_for_saving` after the existing validations.

### Database Access
Rules requiring database queries:
- #3: Check if acquiree exists
- #13: Check acquiree's contents for duplicate detection (optional - could also read from filesystem)

This makes `find_buffer_errors_for_saving` async.

### Order of Checks
1. Quick structural checks (#6, #7, #8, #9, #10, #11) - no DB access
2. Build merge dependency graph for circular detection (#4)
3. Cross-reference with deletion instructions (#1, #2)
4. Check for conflicting merge requests for same ID (#12)
5. Database queries (#3)
6. Optional: Check for post-merge duplicates (#13) if implementing

### Error Type Additions
Need to add these variants to `Buffer_Cannot_Be_Saved` enum:
```rust
Merge_And_Delete_Conflict(OrgNode),
Acquiree_Has_Delete(ID),
Acquiree_Not_Found(ID),
Circular_Merge_Detected(Vec<ID>),
Acquiree_Also_Merging(ID),
Multiple_Merge_Requests(OrgNode, Vec<ID>),
Indefinitive_Node_Cannot_Merge(OrgNode),
Alias_Cannot_Merge(OrgNode),
Merge_Requester_Missing_ID(OrgNode),
Self_Merge_Detected(ID),
Conflicting_Merge_Requests_For_Same_ID(ID, Vec<ID>),
Merge_Would_Create_Duplicate_Content(ID, ID),  // (acquirer_id, duplicate_id)
```
