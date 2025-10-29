# Implementation Plan: merge_nodes_in_graph Function

**Date:** 2025-10-28
**Purpose:** Detailed implementation plan for the `merge_nodes_in_graph` function

---

## Function Signature

```rust
pub async fn merge_nodes_in_graph(
  instructions: Vec<SaveInstruction>,
  config: SkgConfig,
  tantivy_index: &tantivy::Index,
  driver: &TypeDBDriver,
) -> Result<(), Box<dyn Error>>
```

**Location:** `rust/merge.rs`

**Called by:** Tests only (for now - integration with save flow comes later)

---

## Overview

`merge_nodes_in_graph` takes merge SaveInstructions (created by `saveinstructions_from_the_merges_in_an_orgnode_forest`) and applies them to the three storage systems in order:
1. TypeDB
2. Filesystem
3. Tantivy

The merge SaveInstructions consist of three entries per merge:
1. **MERGED node** - new node with acquiree's title/body (indefinitive=false, toDelete=false)
2. **Updated acquirer** - node with new IDs and contains (indefinitive=false, toDelete=false)
3. **Acquiree** - marked for deletion (indefinitive=false, toDelete=true)

---

## Key Design Decisions

### 1. Relationship Field Updates Happen Here (Not in SaveInstruction Generation)

Per user feedback on point 1:
> It shouldn't! That should happen in 'merge_nodes_in_graph'.

**Rationale:**
- `saveinstructions_from_the_merges_in_an_orgnode_forest` should be minimal - just create the basic SaveInstructions
- The complex logic of relationship rerouting and field updates happens in `merge_nodes_in_graph`
- This keeps the SaveInstruction generation simple and puts all merge logic in one place

**Implication:** The SaveInstructions passed to `merge_nodes_in_graph` have:
- Updated acquirer with new `ids` and `contains`
- **BUT NOT** updated `subscribes_to`, `hides_from_its_subscriptions`, or `overrides_view_of`
- These fields need to be updated in `merge_nodes_in_typedb` AND `merge_nodes_in_fs`

### 2. Two-Pass Approach (Not Redundant)

Per user feedback on point 3:
> It would be more efficient to do them all in one pass rather than two, but no, they won't be redundant, and we don't need to optimize for speed yet -- clarity and correctness are more important at this stage.

**Design:**
- First pass: Create/update/delete basic node data
- Second pass: Update relationships and relationship metadata fields

**Rationale:** Clarity and correctness over performance at this stage.

### 3. Integration Comes Later

Per user feedback on point 4:
> We'll use it later. We're just writing that function for now, not calling it (except in tests).

**Current scope:** Implement and test `merge_nodes_in_graph` in isolation. Integration with the save flow is a future task.

### 4. Reuse Extra_ID Creation Logic

Per user feedback on point 6:
> See how this is handled when nodes are initially read from disk by the server. Presumably you can reuse a lot of that code. If appropriate, to avoid duplicating logic, factor out common logic into helper functions.

**Action:** Review `rust/typedb/init.rs` or wherever nodes are initially loaded to see how `has_extra_id` relationships are created. Potentially factor out:
- `create_extra_id_entity_and_relationship(tx, node_id, extra_id_value)` helper
- Or similar utility function

---

## Implementation Plan

### Phase 1: merge_nodes_in_typedb

This is the most complex part. It needs to:
1. Create extra_id entities and has_extra_id relationships
2. Reroute relationships according to merge rules
3. Update relationship metadata fields (subscribes_to, hides, overrides) on acquirer node
4. Create/update/delete nodes via SaveInstructions

#### Step 1.1: Handle Basic Node Operations

```rust
async fn merge_nodes_in_typedb(
  db_name: &str,
  driver: &TypeDBDriver,
  instructions: &[SaveInstruction],
) -> Result<(), Box<dyn Error>> {

  // Filter to find merge-related instructions
  let (merged_nodes, updated_acquirers, deleted_acquirees) =
    categorize_merge_instructions(instructions);

  if merged_nodes.is_empty() {
    return Ok(()); // No merges to process
  }

  // Process each merge
  for i in 0..merged_nodes.len() {
    let merged_node = &merged_nodes[i].0;
    let updated_acquirer = &updated_acquirers[i].0;
    let acquiree = &deleted_acquirees[i].0;

    let acquirer_id = &updated_acquirer.ids[0];
    let acquiree_id = &acquiree.ids[0];

    // Open transaction for this merge
    let tx = driver.transaction(db_name, TransactionType::Write).await?;

    // 1. Create MERGED node
    create_node_in_transaction(&tx, merged_node).await?;

    // 2. Add extra_ids to acquirer
    add_extra_ids_to_acquirer(&tx, acquirer_id, &acquiree.ids).await?;

    // 3. Fetch acquirer's final contains for relationship filtering
    let acquirer_final_contains = compute_final_contains(updated_acquirer);

    // 4. Reroute relationships
    reroute_all_relationships(
      &tx,
      acquirer_id,
      acquiree_id,
      &acquirer_final_contains,
      &acquiree,
      &updated_acquirer,
    ).await?;

    // 5. Update acquirer node with new relationship fields
    update_acquirer_with_relationship_fields(
      &tx,
      acquirer_id,
      &acquiree,
      &updated_acquirer,
      &acquirer_final_contains,
    ).await?;

    // 6. Delete acquiree node
    delete_node_by_id(&tx, acquiree_id).await?;

    tx.commit().await?;
  }

  Ok(())
}
```

#### Step 1.2: Add Extra_IDs

**Research needed:** Look at how extra_ids are created in initial node loading.

```rust
async fn add_extra_ids_to_acquirer(
  tx: &Transaction,
  acquirer_id: &ID,
  acquiree_ids: &[ID], // All IDs from acquiree (primary + extras)
) -> Result<(), Box<dyn Error>> {

  for extra_id_value in acquiree_ids {
    // TypeQL to create extra_id entity and relationship:
    let query = format!(
      r#"match
           $acquirer isa node, has id "{}";
         insert
           $extra_id isa extra_id, has id "{}";
           $rel (node: $acquirer, extra_id: $extra_id) isa has_extra_id;"#,
      acquirer_id.0,
      extra_id_value.0
    );

    tx.query(query).await?;
  }

  Ok(())
}
```

**TODO:** Factor this out if similar code exists elsewhere.

#### Step 1.3: Reroute Relationships

```rust
async fn reroute_all_relationships(
  tx: &Transaction,
  acquirer_id: &ID,
  acquiree_id: &ID,
  acquirer_final_contains: &HashSet<ID>,
  acquiree: &SkgNode,
  updated_acquirer: &SkgNode,
) -> Result<(), Box<dyn Error>> {

  // Order matters: do these sequentially
  reroute_contains(tx, acquirer_id, acquiree_id).await?;
  reroute_subscribes(tx, acquirer_id, acquiree_id).await?;
  reroute_overrides(tx, acquirer_id, acquiree_id).await?;
  reroute_hides(tx, acquirer_id, acquiree_id, acquirer_final_contains).await?;

  // Note: hyperlinks_to handled automatically via text parsing!

  Ok(())
}
```

**Implementation details for each relationship type:**

##### contains Relationships (Bilateral Transfer)

```rust
async fn reroute_contains(
  tx: &Transaction,
  acquirer_id: &ID,
  acquiree_id: &ID,
) -> Result<(), Box<dyn Error>> {

  // 1. Acquiree was container → acquirer is now container
  let query1 = format!(
    r#"match
         $acquiree isa node, has id "{}";
         $acquirer isa node, has id "{}";
         $rel (container: $acquiree, contained: $c) isa contains;
       delete $rel;
       insert
         $new_rel (container: $acquirer, contained: $c) isa contains;"#,
    acquiree_id.0,
    acquirer_id.0
  );
  tx.query(query1).await?;

  // 2. Acquiree was contained → acquirer is now contained
  let query2 = format!(
    r#"match
         $acquiree isa node, has id "{}";
         $acquirer isa node, has id "{}";
         $rel (container: $container, contained: $acquiree) isa contains;
       delete $rel;
       insert
         $new_rel (container: $container, contained: $acquirer) isa contains;"#,
    acquiree_id.0,
    acquirer_id.0
  );
  tx.query(query2).await?;

  Ok(())
}
```

##### subscribes Relationships (Bilateral Transfer)

Similar pattern to contains - transfer both directions.

##### overrides_view_of Relationships (Conditional)

```rust
async fn reroute_overrides(
  tx: &Transaction,
  acquirer_id: &ID,
  acquiree_id: &ID,
) -> Result<(), Box<dyn Error>> {

  // 1. Acquiree was replacement → acquirer is now replacement (TRANSFER)
  let query1 = format!(
    r#"match
         $acquiree isa node, has id "{}";
         $acquirer isa node, has id "{}";
         $rel (replacement: $acquiree, replaced: $r) isa overrides_view_of;
       delete $rel;
       insert
         $new_rel (replacement: $acquirer, replaced: $r) isa overrides_view_of;"#,
    acquiree_id.0,
    acquirer_id.0
  );
  tx.query(query1).await?;

  // 2. Acquiree was replaced → DROP (do not recreate)
  let query2 = format!(
    r#"match
         $acquiree isa node, has id "{}";
         $rel (replacement: $r, replaced: $acquiree) isa overrides_view_of;
       delete $rel;"#,
    acquiree_id.0
  );
  tx.query(query2).await?;

  Ok(())
}
```

##### hides_from_its_subscriptions Relationships (Most Complex)

```rust
async fn reroute_hides(
  tx: &Transaction,
  acquirer_id: &ID,
  acquiree_id: &ID,
  acquirer_final_contains: &HashSet<ID>,
) -> Result<(), Box<dyn Error>> {

  // 1. Acquiree is hidden → DROP (do not recreate)
  let query1 = format!(
    r#"match
         $acquiree isa node, has id "{}";
         $rel (hider: $h, hidden: $acquiree) isa hides_from_its_subscriptions;
       delete $rel;"#,
    acquiree_id.0
  );
  tx.query(query1).await?;

  // 2. Acquiree is hider → transfer ONLY if hidden node NOT in acquirer's contains
  // First, query all hidden nodes
  let query_hidden = format!(
    r#"match
         $acquiree isa node, has id "{}";
         $rel (hider: $acquiree, hidden: $hidden) isa hides_from_its_subscriptions;
         $hidden has id $hidden_id;
       select $hidden_id;"#,
    acquiree_id.0
  );

  let answer = tx.query(query_hidden).await?;
  let mut stream = answer.into_rows();

  while let Some(row_result) = stream.next().await {
    let row = row_result?;
    if let Some(concept) = row.get("hidden_id")? {
      let hidden_id_str = extract_payload_from_typedb_string_rep(&concept.to_string());
      let hidden_id = ID(hidden_id_str);

      // Check if this ID is in acquirer's final contains
      if !acquirer_final_contains.contains(&hidden_id) {
        // Transfer this relationship
        let transfer_query = format!(
          r#"match
               $acquirer isa node, has id "{}";
               $hidden isa node, has id "{}";
               $old_rel (hider: $acquiree, hidden: $hidden) isa hides_from_its_subscriptions;
               $acquiree isa node, has id "{}";
             delete $old_rel;
             insert
               $new_rel (hider: $acquirer, hidden: $hidden) isa hides_from_its_subscriptions;"#,
          acquirer_id.0,
          hidden_id.0,
          acquiree_id.0
        );
        tx.query(transfer_query).await?;
      } else {
        // Just delete (do not recreate - can't hide your own content)
        let delete_query = format!(
          r#"match
               $hidden isa node, has id "{}";
               $old_rel (hider: $acquiree, hidden: $hidden) isa hides_from_its_subscriptions;
               $acquiree isa node, has id "{}";
             delete $old_rel;"#,
          hidden_id.0,
          acquiree_id.0
        );
        tx.query(delete_query).await?;
      }
    }
  }

  Ok(())
}
```

#### Step 1.4: Update Acquirer's Relationship Fields

After rerouting relationships in TypeDB, we need to update the acquirer node's relationship metadata fields to reflect the new state.

```rust
async fn update_acquirer_with_relationship_fields(
  tx: &Transaction,
  acquirer_id: &ID,
  acquiree: &SkgNode,
  updated_acquirer: &SkgNode,
  acquirer_final_contains: &HashSet<ID>,
) -> Result<(), Box<dyn Error>> {

  // Read current acquirer from disk to get its original relationship fields
  // (These are in updated_acquirer but might not have the merge updates yet)

  // Compute new subscribes_to
  let mut new_subscribes_to = updated_acquirer.subscribes_to.clone()
    .unwrap_or_default();
  new_subscribes_to.extend(
    acquiree.subscribes_to.clone().unwrap_or_default()
  );

  // Compute new hides_from_its_subscriptions (with filtering)
  let mut new_hides: Vec<ID> = Vec::new();
  for list in [&updated_acquirer.hides_from_its_subscriptions,
               &acquiree.hides_from_its_subscriptions] {
    if let Some(hides_list) = list {
      for hidden_id in hides_list {
        if !acquirer_final_contains.contains(hidden_id)
           && !new_hides.contains(hidden_id) {
          new_hides.push(hidden_id.clone());
        }
      }
    }
  }

  // Compute new overrides_view_of
  let mut new_overrides = updated_acquirer.overrides_view_of.clone()
    .unwrap_or_default();
  new_overrides.extend(
    acquiree.overrides_view_of.clone().unwrap_or_default()
  );

  // TODO: Update these fields on the acquirer node in TypeDB
  // This might require deleting old values and inserting new ones,
  // or might be handled by a separate function that updates node metadata

  // NOTE: This is tricky because these aren't simple attributes -
  // they're represented as relationships in TypeDB.
  // Actually, we already updated the relationships above!
  // So this function might not be needed in TypeDB.
  // The filesystem update (below) will handle writing these to the .skg file.

  Ok(())
}
```

**IMPORTANT REALIZATION:** The relationship fields in TypeDB are represented as **relationships**, not as node attributes. So we don't need to "update" them in TypeDB - we already did that in `reroute_all_relationships`. This function is only needed for the filesystem update.

---

### Phase 2: merge_nodes_in_fs

Update .skg files on disk to reflect the merge.

```rust
fn merge_nodes_in_fs(
  instructions: Vec<SaveInstruction>,
  config: SkgConfig,
) -> Result<(), Box<dyn Error>> {

  // Categorize instructions
  let (merged_nodes, updated_acquirers, deleted_acquirees) =
    categorize_merge_instructions(&instructions);

  if merged_nodes.is_empty() {
    return Ok(());
  }

  // Process each merge
  for i in 0..merged_nodes.len() {
    let merged_node = &merged_nodes[i].0;
    let updated_acquirer = &updated_acquirers[i].0;
    let acquiree = &deleted_acquirees[i].0;

    // 1. Write MERGED node to disk
    let merged_path = path_from_pid(&config, merged_node.ids[0].clone());
    write_node(merged_node, &merged_path)?;

    // 2. Compute final relationship fields for acquirer
    let acquirer_final_contains: HashSet<ID> =
      updated_acquirer.contains.iter().cloned().collect();

    let mut acquirer_to_write = updated_acquirer.clone();

    // Combine subscribes_to
    acquirer_to_write.subscribes_to = Some(
      updated_acquirer.subscribes_to.clone().unwrap_or_default()
        .into_iter()
        .chain(acquiree.subscribes_to.clone().unwrap_or_default())
        .collect()
    );

    // Combine hides_from_its_subscriptions (with filtering)
    let mut combined_hides = Vec::new();
    for list in [&updated_acquirer.hides_from_its_subscriptions,
                 &acquiree.hides_from_its_subscriptions] {
      if let Some(hides_list) = list {
        for hidden_id in hides_list {
          if !acquirer_final_contains.contains(hidden_id)
             && !combined_hides.contains(hidden_id) {
            combined_hides.push(hidden_id.clone());
          }
        }
      }
    }
    acquirer_to_write.hides_from_its_subscriptions = Some(combined_hides);

    // Combine overrides_view_of
    acquirer_to_write.overrides_view_of = Some(
      updated_acquirer.overrides_view_of.clone().unwrap_or_default()
        .into_iter()
        .chain(acquiree.overrides_view_of.clone().unwrap_or_default())
        .collect()
    );

    // 3. Write updated acquirer to disk
    let acquirer_path = path_from_pid(&config, acquirer_to_write.ids[0].clone());
    write_node(&acquirer_to_write, &acquirer_path)?;

    // 4. Delete acquiree from disk
    let acquiree_path = path_from_pid(&config, acquiree.ids[0].clone());
    fs::remove_file(&acquiree_path)?;
  }

  Ok(())
}
```

---

### Phase 3: merge_nodes_in_tantivy

Update search index to reflect the merge.

```rust
fn merge_nodes_in_tantivy(
  instructions: &[SaveInstruction],
  index: &tantivy::Index,
) -> Result<(), Box<dyn Error>> {

  let (merged_nodes, updated_acquirers, deleted_acquirees) =
    categorize_merge_instructions(instructions);

  if merged_nodes.is_empty() {
    return Ok(());
  }

  let mut index_writer = index.writer(50_000_000)?;

  // Process each merge
  for i in 0..merged_nodes.len() {
    let merged_node = &merged_nodes[i].0;
    let updated_acquirer = &updated_acquirers[i].0;
    let acquiree = &deleted_acquirees[i].0;

    // 1. Add MERGED node to index
    index_tantivy_node(&mut index_writer, merged_node)?;

    // 2. Update acquirer in index (might have new aliases)
    // (Delete old, add new)
    remove_from_tantivy_by_id(&mut index_writer, &updated_acquirer.ids[0])?;
    index_tantivy_node(&mut index_writer, updated_acquirer)?;

    // 3. Remove acquiree from index
    remove_from_tantivy_by_id(&mut index_writer, &acquiree.ids[0])?;
  }

  index_writer.commit()?;

  Ok(())
}
```

**TODO:** Check if these helper functions exist (`index_tantivy_node`, `remove_from_tantivy_by_id`) or need to be created. Look at existing Tantivy code in `rust/tantivy.rs`.

---

## Helper Functions

### categorize_merge_instructions

```rust
fn categorize_merge_instructions(
  instructions: &[SaveInstruction]
) -> (Vec<&SaveInstruction>, Vec<&SaveInstruction>, Vec<&SaveInstruction>) {

  // Merge instructions come in sets of 3:
  // 1. MERGED node (new UUID, title starts with "MERGED:")
  // 2. Updated acquirer (has new IDs appended, new contains with MERGED first)
  // 3. Acquiree (toDelete=true)

  let mut merged_nodes = Vec::new();
  let mut updated_acquirers = Vec::new();
  let mut deleted_acquirees = Vec::new();

  let mut i = 0;
  while i < instructions.len() {
    let instr = &instructions[i];

    if instr.0.title.starts_with("MERGED: ") {
      // This is a MERGED node
      merged_nodes.push(instr);

      // Next should be updated acquirer
      if i + 1 < instructions.len() {
        updated_acquirers.push(&instructions[i + 1]);
      }

      // Next should be deleted acquiree
      if i + 2 < instructions.len() {
        deleted_acquirees.push(&instructions[i + 2]);
      }

      i += 3;
    } else {
      i += 1;
    }
  }

  (merged_nodes, updated_acquirers, deleted_acquirees)
}
```

**NOTE:** This is a simple heuristic. Might need refinement if merge instructions can be interleaved with other SaveInstructions.

---

## Testing Strategy

1. **Unit test each helper function** in isolation
2. **Test merge_nodes_in_typedb** with the existing test fixtures
3. **Test merge_nodes_in_fs** with the existing test fixtures
4. **Test merge_nodes_in_tantivy** with the existing test fixtures
5. **Integration test** the full `merge_nodes_in_graph` function

The existing tests in `tests/merge/merge_nodes_in_graph.rs` provide comprehensive verification.

---

## Open Questions

1. **Hyperlink handling** - Need user confirmation that hyperlinks are handled automatically when MERGED node is created (see discussion above)

2. **SaveInstruction ordering** - Are merge SaveInstructions always in sets of 3 (MERGED, acquirer, acquiree)? Or could they be interleaved?

3. **Multiple merges** - If multiple merges happen in one save, are they independent? (Tests suggest yes)

4. **Transaction boundaries** - Should each merge be its own transaction, or all merges in one transaction?

---

## Implementation Checklist

- [ ] Research how extra_ids are created in initial node loading
- [ ] Implement `add_extra_ids_to_acquirer`
- [ ] Implement `reroute_contains`
- [ ] Implement `reroute_subscribes`
- [ ] Implement `reroute_overrides`
- [ ] Implement `reroute_hides` (most complex)
- [ ] Implement `merge_nodes_in_fs` with relationship field updates
- [ ] Implement `merge_nodes_in_tantivy`
- [ ] Verify hyperlink handling works automatically
- [ ] Run test suite: `cargo test merge::merge_nodes_in_graph`
- [ ] Address any test failures
