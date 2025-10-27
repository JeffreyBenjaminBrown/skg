# Merge Implementation Plan

**Date:** 2025-10-27
**Goal:** Allow users to merge two nodes by adding merge metadata to the acquirer

## Terminology

- **Acquirer**: The node that remains after the merge (has the `merge` instruction)
- **Acquiree**: The node that will be subsumed into the acquirer (specified by ID in the merge instruction)

## User Experience

User adds metadata to acquirer: `(skg (code (requests (merge <ID>))))`
When they save the buffer, the merge happens automatically.

---

## Phase 1: Type Changes

### 1.1 Add Merge to NodeRequest Enum

**File:** `rust/types/orgnode.rs:70-75`

```rust
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NodeRequest {
  ContainerwardView,
  SourcewardView,
  Merge(ID),  // NEW
}
```

**Also update:**
- `fmt::Display` implementation (line 109-119)
- `FromStr` implementation (line 121-131)

### 1.2 Add merge Field to NodeSaveAction

**File:** `rust/types/save.rs:12-18`

```rust
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct NodeSaveAction {
  pub indefinitive: bool,
  pub toDelete: bool,
  pub merge: Option<ID>,  // NEW
}
```

**Note:** `Copy` trait will need to be removed since `Option<ID>` is not Copy.

### 1.3 Add Merge-Related Error Variants

**File:** `rust/types/save.rs:34-44`

Add new variants to `Buffer_Cannot_Be_Saved`:
```rust
pub enum Buffer_Cannot_Be_Saved {
  // ... existing variants ...
  Merge_And_Delete_Conflict(OrgNode),           // NEW: node has both merge and delete
  Acquiree_Has_Delete(ID),                       // NEW: merge target is marked for deletion
  Acquiree_Not_Found(ID),                        // NEW: merge target doesn't exist
  Circular_Merge_Detected(Vec<ID>),              // NEW: A→B→C→A cycle
  Acquiree_Also_Merging(ID),                     // NEW: acquiree has its own merge instruction
}
```

**Also update:** `format_buffer_validation_error` function (line 128-160)

---

## Phase 2: Parsing Changes

### 2.1 Update Metadata Parser

**File:** The parser that converts `(merge <ID>)` s-expressions to `NodeRequest::Merge(ID)`

- Find the existing parser for NodeRequest (likely in parse_headline_md_sexp or similar)
- Add case for parsing `"merge"` with an ID argument
- Create `NodeRequest::Merge(ID)` from the parsed ID string

---

## Phase 3: Validation (find_buffer_errors_for_saving)

### 3.1 Extend Validation Function

**File:** `rust/save/buffer_to_orgnodes/validate_tree/contradictory_instructions.rs`

Create new validation function `find_merge_errors`:

```rust
/// Returns errors related to merge instructions:
/// - Nodes that have both merge and delete
/// - Acquirees that are marked for deletion
/// - Acquirees that don't exist (need DB query)
/// - Circular merge chains (A→B→C→A)
/// - Acquirees that also have merge instructions
pub fn find_merge_errors(
  trees: &[Tree<OrgNode>],
  db_name: &str,
  driver: &TypeDBDriver
) -> Vec<Buffer_Cannot_Be_Saved>
```

**Logic:**

1. **Collect all merge requests** from the forest
   - Build `HashMap<ID, Vec<ID>>` where key is acquirer, value is acquirees

2. **Check for merge+delete conflicts**
   - If node has `toDelete=true` AND has a `Merge` request → error

3. **Check if acquirees exist in database**
   - Query TypeDB for each acquiree ID
   - If not found → error

4. **Check if acquirees are marked for deletion**
   - Check if acquiree ID appears anywhere in forest with `toDelete=true`
   - If yes → error

5. **Check for circular merges**
   - Build dependency graph from merge requests
   - Use cycle detection algorithm (DFS with visited/stack tracking)
   - If cycle found → error with full cycle path

6. **Check if acquirees also have merge instructions**
   - Check if acquiree ID appears anywhere in forest with a Merge request
   - If yes → error

### 3.2 Call New Validation from find_buffer_errors_for_saving

**File:** `rust/save/buffer_to_orgnodes/validate_tree.rs:16-37`

Add async call to `find_merge_errors` and append results to errors vector:

```rust
pub async fn find_buffer_errors_for_saving (
  trees: &[Tree<OrgNode>],
  db_name: &str,  // NEW parameter
  driver: &TypeDBDriver  // NEW parameter
) -> Vec<Buffer_Cannot_Be_Saved> {
  let mut errors: Vec<Buffer_Cannot_Be_Saved> = Vec::new();

  // ... existing validation ...

  // NEW: Merge validation
  let merge_errors = find_merge_errors(trees, db_name, driver).await;
  errors.extend(merge_errors);

  errors
}
```

**Note:** This makes `find_buffer_errors_for_saving` async and requires DB access. Update all call sites:
- `rust/save.rs:56-60` in `buffer_to_save_instructions`

---

## Phase 4: Extract Merge Information (orgnodes_to_save_instructions)

### 4.1 Extract Merge Requests When Building SaveInstructions

**File:** `rust/save/orgnodes_to_instructions.rs`

When building `NodeSaveAction` for each node, check `nodeRequests` for `Merge(ID)`:

```rust
// In the function that creates NodeSaveAction from OrgNode
let merge_target: Option<ID> = node
  .metadata
  .code
  .nodeRequests
  .iter()
  .find_map(|req| match req {
    NodeRequest::Merge(id) => Some(id.clone()),
    _ => None,
  });

let action = NodeSaveAction {
  indefinitive: node.metadata.code.indefinitive,
  toDelete: node.metadata.code.toDelete,
  merge: merge_target,  // NEW
};
```

---

## Phase 5: Implement merge_in_graph Function

### 5.1 Create New Module

**File:** `rust/merge.rs` (NEW FILE)

```rust
use crate::types::{SaveInstruction, SkgConfig, TantivyIndex, ID, SkgNode, NodeSaveAction};
use crate::typedb::nodes::delete_nodes_from_pids;
use std::error::Error;
use typedb_driver::TypeDBDriver;
use std::collections::HashMap;

/// Performs merges after update_graph has run.
/// Processes all merge instructions in the correct order:
/// 1. Create new nodes (MERGED nodes)
/// 2. Perform edits (acquirer updates, relationship rerouting)
/// 3. Delete old nodes (acquirees)
pub async fn merge_in_graph(
  mut instructions: Vec<SaveInstruction>,
  config: &SkgConfig,
  tantivy_index: &TantivyIndex,
  driver: &TypeDBDriver,
) -> Result<(), Box<dyn Error>> {

  // 1. Filter merge instructions
  let merge_instructions: Vec<(SkgNode, ID)> = instructions
    .iter()
    .filter_map(|(node, action)| {
      action.merge.as_ref().map(|merge_id| (node.clone(), merge_id.clone()))
    })
    .collect();

  if merge_instructions.is_empty() {
    return Ok(());
  }

  println!("Processing {} merge(s)...", merge_instructions.len());

  // 2. FIRST: Create all MERGED nodes via SaveInstructions
  let mut merged_nodes: HashMap<ID, SkgNode> = HashMap::new();

  for (acquirer_node, acquiree_id) in &merge_instructions {
    // Fetch acquiree data from database and filesystem
    let acquiree_data = fetch_node_data(
      &config.db_name, driver, config, &acquiree_id
    ).await?;

    // Create MERGED node
    let merged_node = create_merged_node(&acquiree_data);
    let merged_id = merged_node.ids[0].clone();

    // Add SaveInstruction for MERGED node
    let merged_action = NodeSaveAction {
      indefinitive: false,
      toDelete: false,
      merge: None,
    };
    instructions.push((merged_node.clone(), merged_action));
    merged_nodes.insert(acquiree_id.clone(), merged_node);
  }

  // Create MERGED nodes in TypeDB, FS, and Tantivy via update_graph
  update_graph(
    instructions.clone(),
    config.clone(),
    tantivy_index,
    driver
  ).await?;

  // 3. NEXT: Perform edits (relationship rerouting, acquirer updates)
  for (acquirer_node, acquiree_id) in merge_instructions {
    let acquirer_id = &acquirer_node.ids[0];

    println!("Merging {} into {}...", acquiree_id.0, acquirer_id.0);

    let acquiree_data = fetch_node_data(
      &config.db_name, driver, config, &acquiree_id
    ).await?;

    let merged_node = merged_nodes.get(&acquiree_id).unwrap();

    // Update TypeDB (relationships + acquirer's extra_ids)
    merge_in_typedb(
      &config.db_name,
      driver,
      acquirer_id,
      &acquiree_id,
      &acquiree_data,
    ).await?;

    // Update Filesystem (acquirer's contains list + extra_ids)
    merge_in_fs(
      config,
      acquirer_id,
      &acquiree_id,
      &acquiree_data,
      &merged_node.ids[0],
    )?;
  }

  // 4. LAST: Delete acquiree nodes
  let acquiree_ids: Vec<ID> = merge_instructions
    .iter()
    .map(|(_, acquiree_id)| acquiree_id.clone())
    .collect();

  for acquiree_id in &acquiree_ids {
    delete_acquiree_node(&config.db_name, driver, config, acquiree_id).await?;
  }

  println!("All merges completed successfully.");
  Ok(())
}

/// Data fetched from database and filesystem about a node
struct NodeData {
  title: String,
  body: Option<String>,
  contains_ids: Vec<ID>,  // IDs of contained nodes (from TypeDB contains relationships)
  extra_ids: Vec<ID>,     // IDs from has_extra_id relationships
}

fn create_merged_node(acquiree_data: &NodeData) -> SkgNode {
  SkgNode {
    title: format!("MERGED: {}", acquiree_data.title),
    aliases: None,
    ids: vec![ID(uuid::Uuid::new_v4().to_string())],
    body: acquiree_data.body.clone(),
    contains: Vec::new(), // MERGED node has no children
    subscribes_to: Vec::new(),
    hides_from_its_subscriptions: Vec::new(),
    overrides_view_of: Vec::new(),
  }
}
```

### 5.2 Fetch Acquiree Data from Database

```rust
async fn fetch_node_data(
  db_name: &str,
  driver: &TypeDBDriver,
  config: &SkgConfig,
  node_id: &ID,
) -> Result<NodeData, Box<dyn Error>> {
  // Title and body are FS-only (confirmed by user)
  // Contains and extra_ids are in TypeDB

  // 1. Read the .skg file to get title and body
  let node_path = path_from_pid(config, node_id.clone());
  let skg_node = read_node(&node_path)?;  // TODO: Find exact function name
  let title = skg_node.title;
  let body = skg_node.body;

  // 2. Get contains IDs from TypeDB
  let contains_ids = fetch_contains_ids(db_name, driver, node_id).await?;

  // 3. Get extra_ids from TypeDB
  let extra_ids = fetch_extra_ids(db_name, driver, node_id).await?;

  Ok(NodeData {
    title,
    body,
    contains_ids,
    extra_ids,
  })
}

async fn fetch_content_ids(
  db_name: &str,
  driver: &TypeDBDriver,
  container_id: &ID,
) -> Result<Vec<ID>, Box<dyn Error>> {
  // Query: match $container has id "...";
  //        $rel (container: $container, contained: $content) isa contains;
  //        $content has id $content_id;
  //        select $content_id;
  // Extract IDs from results
  todo!()
}

async fn fetch_extra_ids(
  db_name: &str,
  driver: &TypeDBDriver,
  node_id: &ID,
) -> Result<Vec<ID>, Box<dyn Error>> {
  // Query: match $node has id "...";
  //        $rel (node: $node, extra_id: $extra) isa has_extra_id;
  //        $extra has id $extra_id_value;
  //        select $extra_id_value;
  // Similar to delete_nodes_from_pids logic at nodes.rs:187-207
  todo!()
}
```

### 5.3 Merge in TypeDB

```rust
async fn merge_in_typedb(
  db_name: &str,
  driver: &TypeDBDriver,
  acquirer_id: &ID,
  acquiree_id: &ID,
  acquiree_data: &NodeData,
) -> Result<(), Box<dyn Error>> {
  println!("  Updating TypeDB for merge...");

  let tx = driver.transaction(db_name, TransactionType::Write).await?;

  // 1. Add acquiree's ID as extra_id to acquirer
  add_extra_id(&tx, acquirer_id, acquiree_id).await?;

  // 2. Transfer acquiree's extra_ids to acquirer
  for extra_id in &acquiree_data.extra_ids {
    add_extra_id(&tx, acquirer_id, extra_id).await?;
  }

  // 3. Reroute all relationships FROM acquiree TO acquirer
  // Need acquirer's contents for hides_from_its_subscriptions logic
  let acquirer_contains = fetch_contains_ids(db_name, driver, acquirer_id).await?;
  reroute_relationships(&tx, acquirer_id, acquiree_id, &acquirer_contains).await?;

  // 4. Delete acquiree node (but not yet - see note below)
  delete_node_by_iid(&tx, acquiree_id).await?;

  tx.commit().await?;
  println!("    TypeDB merge complete.");
  Ok(())
}

async fn add_extra_id(
  tx: &Transaction,
  node_id: &ID,
  extra_id: &ID,
) -> Result<(), Box<dyn Error>> {
  // Query: match $node has id "node_id";
  //        insert $extra (node: $node, extra_id: $new_extra) isa has_extra_id;
  //               $new_extra isa extra_id, has id "extra_id";
  todo!()
}

async fn reroute_relationships(
  tx: &Transaction,
  acquirer_id: &ID,
  acquiree_id: &ID,
  acquirer_contains: &[ID],
) -> Result<(), Box<dyn Error>> {
  // CRITICAL: This is complex because TypeDB relationships use internal IIDs
  //
  // Relationship types from schema.tql:
  // - contains (container/contained) - transfer wholesale
  // - hyperlinks_to (source/dest) - transfer wholesale
  // - subscribes (subscriber/subscribee) - transfer wholesale
  // - hides_from_its_subscriptions (hider/hidden) - conditional logic
  // - overrides_view_of (replacement/replaced) - conditional logic
  // - has_extra_id (node/extra_id) - handled separately in add_extra_id

  reroute_contains_relationships(tx, acquirer_id, acquiree_id).await?;
  reroute_hyperlinks_relationships(tx, acquirer_id, acquiree_id).await?;
  reroute_subscribes_relationships(tx, acquirer_id, acquiree_id).await?;
  reroute_hides_relationships(tx, acquirer_id, acquiree_id, acquirer_contains).await?;
  reroute_overrides_relationships(tx, acquirer_id, acquiree_id).await?;

  Ok(())
}

async fn reroute_contains_relationships(
  tx: &Transaction,
  acquirer_id: &ID,
  acquiree_id: &ID,
) -> Result<(), Box<dyn Error>> {
  // Transfer wholesale: wherever acquiree was container or contained, acquirer now is

  // 1. Acquiree was container
  //    match $acquiree has id "acquiree_id";
  //          $rel (container: $acquiree, contained: $c) isa contains;
  //          $c has id $contained_id;
  //    For each: delete $rel, insert (container: $acquirer, contained: $c)

  // 2. Acquiree was contained
  //    match $acquiree has id "acquiree_id";
  //          $rel (container: $container, contained: $acquiree) isa contains;
  //          $container has id $container_id;
  //    For each: delete $rel, insert (container: $container, contained: $acquirer)

  todo!()
}

async fn reroute_hyperlinks_relationships(
  tx: &Transaction,
  acquirer_id: &ID,
  acquiree_id: &ID,
) -> Result<(), Box<dyn Error>> {
  // Transfer wholesale: wherever acquiree was source or dest, acquirer now is

  // 1. Acquiree was source
  //    match $acquiree has id "acquiree_id";
  //          $rel (source: $acquiree, dest: $d) isa hyperlinks_to;
  //          $d has id $dest_id;
  //    For each: delete $rel, insert (source: $acquirer, dest: $d)

  // 2. Acquiree was dest
  //    match $acquiree has id "acquiree_id";
  //          $rel (source: $s, dest: $acquiree) isa hyperlinks_to;
  //          $s has id $source_id;
  //    For each: delete $rel, insert (source: $s, dest: $acquirer)

  todo!()
}

async fn reroute_subscribes_relationships(
  tx: &Transaction,
  acquirer_id: &ID,
  acquiree_id: &ID,
) -> Result<(), Box<dyn Error>> {
  // Transfer wholesale: wherever acquiree was subscriber or subscribee, acquirer now is

  // 1. Acquiree was subscriber
  //    match $acquiree has id "acquiree_id";
  //          $rel (subscriber: $acquiree, subscribee: $s) isa subscribes;
  //          $s has id $subscribee_id;
  //    For each: delete $rel, insert (subscriber: $acquirer, subscribee: $s)

  // 2. Acquiree was subscribee
  //    match $acquiree has id "acquiree_id";
  //          $rel (subscriber: $s, subscribee: $acquiree) isa subscribes;
  //          $s has id $subscriber_id;
  //    For each: delete $rel, insert (subscriber: $s, subscribee: $acquirer)

  todo!()
}

async fn reroute_hides_relationships(
  tx: &Transaction,
  acquirer_id: &ID,
  acquiree_id: &ID,
  acquirer_contains: &[ID],  // List of nodes contained by acquirer
) -> Result<(), Box<dyn Error>> {
  // Conditional logic based on role

  // 1. Acquiree is HIDDEN → DROP the relationship
  //    match $acquiree has id "acquiree_id";
  //          $rel (hider: $h, hidden: $acquiree) isa hides_from_its_subscriptions;
  //    For each: delete $rel (do NOT recreate)

  // 2. Acquiree is HIDER → transfer ONLY if hidden node is NOT in acquirer's contains
  //    match $acquiree has id "acquiree_id";
  //          $rel (hider: $acquiree, hidden: $hidden) isa hides_from_its_subscriptions;
  //          $hidden has id $hidden_id;
  //    For each:
  //      if $hidden_id NOT in acquirer_contains:
  //        delete $rel, insert (hider: $acquirer, hidden: $hidden)
  //      else:
  //        delete $rel (do NOT recreate - can't hide from your own content)

  todo!()
}

async fn reroute_overrides_relationships(
  tx: &Transaction,
  acquirer_id: &ID,
  acquiree_id: &ID,
) -> Result<(), Box<dyn Error>> {
  // Conditional logic based on role

  // 1. Acquiree is REPLACEMENT → transfer
  //    match $acquiree has id "acquiree_id";
  //          $rel (replacement: $acquiree, replaced: $r) isa overrides_view_of;
  //          $r has id $replaced_id;
  //    For each: delete $rel, insert (replacement: $acquirer, replaced: $r)

  // 2. Acquiree is REPLACED → DROP the relationship
  //    match $acquiree has id "acquiree_id";
  //          $rel (replacement: $r, replaced: $acquiree) isa overrides_view_of;
  //    For each: delete $rel (do NOT recreate - acquiree no longer exists to be replaced)

  todo!()
}
```

### 5.4 Merge in Filesystem

```rust
fn merge_in_fs(
  config: &SkgConfig,
  acquirer_id: &ID,
  acquiree_id: &ID,
  acquiree_data: &NodeData,
  merged_node_id: &ID,  // ID of the MERGED node created earlier
) -> Result<(), Box<dyn Error>> {
  println!("  Updating filesystem for merge...");

  // NOTE: MERGED node was already created via SaveInstructions in step 2
  // We only need to update the acquirer's file here

  // 1. Read acquirer's file using existing utility
  let acquirer_path = path_from_pid(config, acquirer_id.clone());
  let mut acquirer_skg = read_node(&acquirer_path)?;  // TODO: Find exact function name

  // 2. Add acquiree's ID to acquirer's extra IDs
  acquirer_skg.ids.push(acquiree_id.clone());
  acquirer_skg.ids.extend(acquiree_data.extra_ids.clone());

  // 3. Update acquirer's contains list:
  //    [MERGED node] + acquirer's old contains + acquiree's contains
  let mut new_contains = vec![merged_node_id.clone()];
  new_contains.extend(acquirer_skg.contains.clone());
  new_contains.extend(acquiree_data.contains_ids.clone());
  acquirer_skg.contains = new_contains;

  // 4. Write updated acquirer using existing utility
  write_node(&acquirer_skg, &Path::new(&acquirer_path))?;

  println!("    Filesystem merge complete.");
  Ok(())
}

async fn delete_acquiree_node(
  db_name: &str,
  driver: &TypeDBDriver,
  config: &SkgConfig,
  acquiree_id: &ID,
) -> Result<(), Box<dyn Error>> {
  println!("  Deleting acquiree node {}...", acquiree_id.0);

  // Delete from TypeDB
  let tx = driver.transaction(db_name, TransactionType::Write).await?;
  delete_node_by_iid(&tx, acquiree_id).await?;
  tx.commit().await?;

  // Delete from filesystem
  let acquiree_path = path_from_pid(config, acquiree_id.clone());
  if Path::new(&acquiree_path).exists() {
    std::fs::remove_file(&acquiree_path)?;
  }

  println!("    Acquiree deleted.");
  Ok(())
}
```

### 5.5 Merge in Tantivy

**Note:** With the new approach of creating MERGED nodes via SaveInstructions, Tantivy indexing is handled automatically by `update_graph`. The only explicit action needed is removing the acquiree from the index.

```rust
async fn delete_acquiree_node(
  db_name: &str,
  driver: &TypeDBDriver,
  config: &SkgConfig,
  tantivy_index: &TantivyIndex,
  acquiree_id: &ID,
) -> Result<(), Box<dyn Error>> {
  println!("  Deleting acquiree node {}...", acquiree_id.0);

  // Delete from TypeDB
  let tx = driver.transaction(db_name, TransactionType::Write).await?;
  delete_node_by_iid(&tx, acquiree_id).await?;
  tx.commit().await?;

  // Delete from filesystem
  let acquiree_path = path_from_pid(config, acquiree_id.clone());
  if Path::new(&acquiree_path).exists() {
    std::fs::remove_file(&acquiree_path)?;
  }

  // Remove from Tantivy index
  remove_from_index(tantivy_index, acquiree_id)?;

  println!("    Acquiree deleted.");
  Ok(())
}
```

**Summary of Tantivy operations:**
- MERGED node: Indexed automatically by `update_graph` (step 2)
- Acquirer node: Re-indexed when file is rewritten in `merge_in_fs`
- Acquiree node: Explicitly removed from index during deletion (step 4)

---

## Phase 6: Integration into Save Flow

### 6.1 Call merge_in_graph After update_graph

**File:** `rust/serve/save_buffer.rs:155-192`

```rust
async fn update_from_and_rerender_buffer (
  org_buffer_text : &str,
  typedb_driver   : &TypeDBDriver,
  config          : &SkgConfig,
  tantivy_index   : &TantivyIndex
) -> Result<SaveResponse, Box<dyn Error>> {

  let (mut orgnode_forest, save_instructions)
    : (Vec<Tree<OrgNode>>, Vec<SaveInstruction>)
    = buffer_to_save_instructions (
      org_buffer_text, config, typedb_driver )
    . await . map_err (
      |e| Box::new(e) as Box<dyn Error> ) ?;
  if orgnode_forest.is_empty() { return Err (
    "No valid org nodes found in org_buffer_text" . into()); }

  // Update graph (TypeDB, FS, Tantivy)
  update_graph (
    save_instructions.clone(),  // Clone so we can use again
    config.clone(),
    tantivy_index,
    typedb_driver ) . await ?;

  // NEW: Perform merges
  merge_in_graph(
    save_instructions,
    config,
    tantivy_index,
    typedb_driver
  ).await?;

  let mut errors : Vec < String > = Vec::new ();

  { // modify the orgnode forest before re-rendering it
    // NEW: Remove acquirees from forest
    remove_merged_nodes_from_forest(&mut orgnode_forest, &save_instructions);

    completeOrgnodeForest (
      &mut orgnode_forest,
      config,
      typedb_driver,
      &mut errors ) . await ?;
    set_metadata_relationships_in_forest (
      &mut orgnode_forest,
      config,
      typedb_driver ) . await ?; }

  let content : String =
    render_forest_to_org ( & orgnode_forest );

  Ok ( SaveResponse { content, errors } ) }
```

### 6.2 Remove Merged Nodes from Forest

```rust
/// Remove acquiree nodes from the orgnode forest so they don't appear in the re-rendered view
fn remove_merged_nodes_from_forest(
  forest: &mut Vec<Tree<OrgNode>>,
  instructions: &[SaveInstruction],
) {
  // 1. Collect all acquiree IDs
  let acquiree_ids: HashSet<ID> = instructions
    .iter()
    .filter_map(|(_, action)| action.merge.as_ref())
    .cloned()
    .collect();

  if acquiree_ids.is_empty() {
    return;
  }

  // 2. Remove trees whose root has an acquiree ID
  forest.retain(|tree| {
    if let Some(id) = &tree.root().value().metadata.id {
      !acquiree_ids.contains(id)
    } else {
      true
    }
  });

  // 3. Recursively remove acquiree nodes from within trees
  for tree in forest.iter_mut() {
    remove_nodes_recursively(tree, &acquiree_ids);
  }
}

fn remove_nodes_recursively(
  tree: &mut Tree<OrgNode>,
  acquiree_ids: &HashSet<ID>,
) {
  // Using the ego_tree API demonstrated in complete_contents.rs:189-216
  // We need to collect node IDs first, then remove them using detach()

  // Collect all node IDs that should be removed
  let node_ids_to_remove: Vec<NodeId> = tree
    .root()
    .descendants()
    .filter_map(|node_ref| {
      if let Some(id) = &node_ref.value().metadata.id {
        if acquiree_ids.contains(id) {
          Some(node_ref.id())
        } else {
          None
        }
      } else {
        None
      }
    })
    .collect();

  // Remove each node using detach()
  for node_id in node_ids_to_remove {
    if let Some(mut node_mut) = tree.get_mut(node_id) {
      node_mut.detach();
    }
  }
}
```

---

## Phase 7: Testing

### 7.1 Unit Tests

Create tests for:
- Merge validation (circular detection, conflicts, etc.)
- NodeData fetching
- Relationship rerouting logic
- Forest node removal

### 7.2 Integration Tests

Create test in `tests/integration/node-merge/`:
- Setup: Create two nodes A and B
- Action: Add `(merge B)` to A's metadata
- Save buffer
- Verify:
  - A has B's ID as extra_id
  - A's contents = [MERGED node, A's old contents, B's old contents]
  - B is deleted from database
  - B's file is deleted
  - B doesn't appear in returned view
  - All relationships now point to A

---

## Open Questions / Implementation Notes

### SkgNode Structure
Need to verify the exact structure of `SkgNode` - does it have:
- `title: String`?
- `body: Option<String>`?
- `content: Vec<ID>`?

### Title and Body Storage
- Are title/body stored in TypeDB or only in filesystem?
- Schema shows only `id` attribute, suggesting title/body are FS-only
- Fetch strategy needs to account for this

### Relationship Rerouting Complexity
- TypeDB uses internal IIDs for relationships
- Can't just update the ID attribute of a node in a relationship
- Must delete old relationship and create new one
- Need to handle all 5 relationship types from schema.tql

### Order of Operations for "Write Twice"
User wants acquirer written twice:
1. First write: Apply user's direct edits to acquirer (via update_graph)
2. Second write: Apply merge changes (via merge_in_graph)

This is reflected in the plan: update_graph runs first, then merge_in_graph.

### NodeSaveAction Not Being Copy
Removing `Copy` trait from `NodeSaveAction` when adding `merge: Option<ID>` field requires checking all usages for potential issues with moves.

### Validation Timing
Validation happens early in `buffer_to_save_instructions`, which is good. But merge validation needs DB access to check if acquiree exists, making validation async.

---

## Implementation Order

1. **Types** (Phase 1) - Get the data structures in place
2. **Parsing** (Phase 2) - Handle the new metadata syntax
3. **Validation** (Phase 3) - Catch errors early
4. **Extract merge info** (Phase 4) - Capture merge requests in SaveInstructions
5. **Core merge logic** (Phase 5) - Implement the actual merging
6. **Integration** (Phase 6) - Wire it into the save flow
7. **Testing** (Phase 7) - Verify it works
