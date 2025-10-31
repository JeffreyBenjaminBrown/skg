# Validation in handle_save_buffer_request

## Overview

The `handle_save_buffer_request` function (rust/serve/save_buffer.rs:53) processes buffer save requests from Emacs. It calls `update_from_and_rerender_buffer`, which in turn calls `buffer_to_save_instructions` (rust/save.rs:36). This is where all validation occurs.

## Call Flow

```
handle_save_buffer_request (serve/save_buffer.rs:53)
  └─> update_from_and_rerender_buffer (serve/save_buffer.rs:155)
      └─> buffer_to_save_instructions (save.rs:36)
          ├─> org_to_uninterpreted_nodes (parse buffer text)
          ├─> change_repeated_to_indefinitive (preprocessing)
          ├─> add_missing_info_to_trees (fill in PIDs from database)
          ├─> find_buffer_errors_for_saving ★ VALIDATION POINT 1 ★
          ├─> orgnodes_to_save_instructions
          │   ├─> orgnodes_to_dirty_save_instructions (convert to instructions)
          │   └─> reconcile_dup_instructions ★ VALIDATION POINT 2 ★
          │       └─> reconcile_dup_instructions_for_one_id
          │           └─> to_delete_if_consistent ★ VALIDATION POINT 3 ★
          └─> clobber_none_fields_with_data_from_disk
```

---

## Validation Point 1: find_buffer_errors_for_saving

**Location:** `rust/save/buffer_to_orgnodes/validate_tree.rs:16`

**Called from:** `buffer_to_save_instructions` (save.rs:57)

**Purpose:** Main validation orchestrator that looks for invalid structure in the org buffer when a user tries to save it.

**Important Assumption:** IDs have been replaced with PIDs by `add_missing_info_to_trees`. Otherwise two org nodes might refer to the same skg node yet appear not to.

### Sub-validations

#### 1.1: find_inconsistent_instructions

**Location:** `rust/save/buffer_to_orgnodes/validate_tree/contradictory_instructions.rs:32`

**Checks for:**
- **Ambiguous Deletions:** A node with `toDelete=true` but another node with the same ID has `toDelete=false`
- **Multiple Defining Containers:** Multiple nodes with the same ID where both have `repeat=false` and `indefinitive=false` (both claim to define the node's contents)

**Returns:** `(Vec<ID>, Vec<ID>)` - inconsistent deletion IDs and problematic defining IDs

**Error types produced:**
- `Buffer_Cannot_Be_Saved::AmbiguousDeletion(ID)`
- `Buffer_Cannot_Be_Saved::Multiple_DefiningContainers(ID)`

#### 1.2: validate_node_and_children (recursive)

**Location:** `rust/save/buffer_to_orgnodes/validate_tree.rs:40`

**Checks performed recursively on each node:**

1. **AliasCol validation:**
   - AliasCol nodes must not have a body
   - Error: `Buffer_Cannot_Be_Saved::Body_of_AliasCol(OrgNode)`

2. **Alias validation:**
   - Alias nodes must not have a body
   - Error: `Buffer_Cannot_Be_Saved::Body_of_Alias(OrgNode)`
   - Alias nodes must have an AliasCol parent
   - Error: `Buffer_Cannot_Be_Saved::Alias_with_no_AliasCol_Parent(OrgNode)`

3. **Parent-child relationship validation:**
   - Children of AliasCol nodes must not have IDs
   - Error: `Buffer_Cannot_Be_Saved::Child_of_AliasCol_with_ID(OrgNode)`
   - Alias nodes must not have children at all
   - Error: `Buffer_Cannot_Be_Saved::Child_of_Alias(OrgNode)`

4. **Multiple AliasCol validation:**
   - A node can have at most one child with `relToParent=AliasCol`
   - Error: `Buffer_Cannot_Be_Saved::Multiple_AliasCols_in_Children(OrgNode)`

5. **Duplicated content validation:**
   - If a node is definitive (`indefinitive=false`), it cannot have two `relToParent=Content` children with the same ID
   - Error: `Buffer_Cannot_Be_Saved::DuplicatedContent(ID)`

### Result

If any validation errors are found, `buffer_to_save_instructions` returns:
```rust
Err(SaveError::BufferValidationErrors(Vec<Buffer_Cannot_Be_Saved>))
```

---

## Validation Point 2: reconcile_dup_instructions

**Location:** `rust/save/orgnodes_to_instructions/reconcile_dup_instructions.rs:17`

**Called from:** `orgnodes_to_save_instructions` (save/orgnodes_to_instructions/from_tree.rs:9)

**Purpose:** Groups SaveInstructions by ID and reconciles duplicates into single instructions.

**Process:**
1. Groups instructions by primary ID
2. For each group, calls `reconcile_dup_instructions_for_one_id`

---

## Validation Point 3: to_delete_if_consistent

**Location:** `rust/save/orgnodes_to_instructions/reconcile_dup_instructions.rs:162`

**Called from:** `reconcile_dup_instructions_for_one_id` (same file:44)

**Purpose:** Validates that all SaveInstructions for the same ID have consistent `toDelete` values.

**Check performed:**
- Collects all `toDelete` values for instructions with the same ID
- If more than one distinct value exists, returns error

**Error:**
```rust
Err("Inconsistent toDelete values for same ID")
```

**Note:** This should theoretically never fail because `find_inconsistent_instructions` (Validation Point 1.1) already checks for this at the OrgNode level. This is a defensive second check at the SaveInstruction level.

---

## Error Type Hierarchy

```rust
SaveError (types/save.rs:4)
  ├─> ParseError(String)
  ├─> DatabaseError(Box<dyn Error>)
  ├─> IoError(io::Error)
  └─> BufferValidationErrors(Vec<Buffer_Cannot_Be_Saved>)
        ├─> Body_of_AliasCol(OrgNode)
        ├─> Child_of_AliasCol_with_ID(OrgNode)
        ├─> Body_of_Alias(OrgNode)
        ├─> Child_of_Alias(OrgNode)
        ├─> Alias_with_no_AliasCol_Parent(OrgNode)
        ├─> Multiple_AliasCols_in_Children(OrgNode)
        ├─> Multiple_DefiningContainers(ID)
        ├─> AmbiguousDeletion(ID)
        └─> DuplicatedContent(ID)
```

---

## Summary of All Validation Functions

| Function | Location | Level | What it validates |
|----------|----------|-------|-------------------|
| `find_buffer_errors_for_saving` | validate_tree.rs:16 | OrgNode forest | Orchestrates all OrgNode-level validation |
| `find_inconsistent_instructions` | contradictory_instructions.rs:32 | OrgNode forest | Contradictory deletion/defining instructions across nodes |
| `validate_node_and_children` | validate_tree.rs:40 | OrgNode (recursive) | Structural errors: AliasCol/Alias rules, parent-child relationships, duplicate content |
| `to_delete_if_consistent` | reconcile_dup_instructions.rs:162 | SaveInstruction group | Consistent toDelete values when reconciling |

---

## Key Design Principles

1. **Validation happens early:** Most validation occurs after parsing and PID resolution, before converting to SaveInstructions.

2. **Fail fast:** If `find_buffer_errors_for_saving` finds any errors, the entire save operation aborts before touching any storage systems.

3. **Defensive depth:** `to_delete_if_consistent` provides a second check for consistency that should already be guaranteed by earlier validation.

4. **Clear error types:** The `Buffer_Cannot_Be_Saved` enum provides specific, actionable error messages for each type of validation failure.

5. **ID normalization prerequisite:** Critical that `add_missing_info_to_trees` runs before validation to replace all IDs with PIDs, ensuring nodes referring to the same entity are recognized as such.
