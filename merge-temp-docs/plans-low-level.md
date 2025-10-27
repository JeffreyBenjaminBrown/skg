# Merge Feature - Low-Level Implementation Plans

For high-level overview, see [plans-high-level.md](plans-high-level.md).

**Note:** This document contains detailed implementation code. See implementation-progress.md for what's actually been implemented.

---

## Phase 1: Type Changes (Partially Complete)

### NodeRequest Enum ✅ DONE
**File:** `rust/types/orgnode.rs`

```rust
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NodeRequest {
  ContainerwardView,
  SourcewardView,
  Merge(ID),  // ✅ Added
}
```

**Display implementation:**
```rust
NodeRequest::Merge(id) => write!(f, "(merge {})", id.0),
```

**FromStr implementation:**
```rust
if let Some(id_str) = s.strip_prefix("merge ") {
  Ok(NodeRequest::Merge(ID::from(id_str)))
}
```

### NodeSaveAction - TODO
**File:** `rust/types/save.rs`

```rust
#[derive(Clone, Debug, PartialEq)]  // Remove Copy since Option<ID> is not Copy
pub struct NodeSaveAction {
  pub indefinitive: bool,
  pub toDelete: bool,
  pub merge: Option<ID>,  // TODO: Add this field
}
```

### Error Variants - TODO
**File:** `rust/types/save.rs`

```rust
pub enum Buffer_Cannot_Be_Saved {
  // ...existing variants...
  Merge_And_Delete_Conflict(OrgNode),
  Acquiree_Has_Delete(ID),
  Acquiree_Not_Found(ID),
  Circular_Merge_Detected(Vec<ID>),
  Acquiree_Also_Merging(ID),
}
```

Also update `format_buffer_validation_error` function.

---

## Phase 2: Parsing (Complete)

### parse_requests_sexp ✅ DONE
**File:** `rust/serve/parse_headline_md_sexp.rs`

```rust
Sexp::List(list_items) if list_items.len() == 2 => {
  let command: String = atom_to_string(&list_items[0])?;
  if command == "merge" {
    let id_str: String = atom_to_string(&list_items[1])?;
    requests.insert(NodeRequest::Merge(ID::from(id_str)));
  }
}
```

---

## Phase 3: Validation - TODO

### find_merge_errors Function
**File:** `rust/save/buffer_to_orgnodes/validate_tree/contradictory_instructions.rs` (new file or add to existing)

```rust
pub async fn find_merge_errors(
  trees: &[Tree<OrgNode>],
  db_name: &str,
  driver: &TypeDBDriver
) -> Vec<Buffer_Cannot_Be_Saved> {
  let mut errors = Vec::new();

  // 1. Collect merge requests
  let mut merge_map: HashMap<ID, ID> = HashMap::new();  // acquirer -> acquiree
  // ...walk trees and collect...

  // 2. Check for merge+delete conflicts
  // ...

  // 3. Check acquirees exist in DB
  for acquiree_id in merge_map.values() {
    let exists = check_node_exists(db_name, driver, acquiree_id).await?;
    if !exists {
      errors.push(Buffer_Cannot_Be_Saved::Acquiree_Not_Found(acquiree_id.clone()));
    }
  }

  // 4. Check acquirees aren't marked for deletion
  // ...

  // 5. Detect cycles
  if let Some(cycle) = detect_merge_cycle(&merge_map) {
    errors.push(Buffer_Cannot_Be_Saved::Circular_Merge_Detected(cycle));
  }

  // 6. Check acquirees don't also have merge instructions
  // ...

  errors
}
```

### Cycle Detection Algorithm
```rust
fn detect_merge_cycle(merge_map: &HashMap<ID, ID>) -> Option<Vec<ID>> {
  let mut visited = HashSet::new();
  let mut rec_stack = HashSet::new();
  let mut path = Vec::new();

  for start_id in merge_map.keys() {
    if let Some(cycle) = dfs(start_id, merge_map, &mut visited, &mut rec_stack, &mut path) {
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

### Update find_buffer_errors_for_saving
**File:** `rust/save/buffer_to_orgnodes/validate_tree.rs`

```rust
pub async fn find_buffer_errors_for_saving(
  trees: &[Tree<OrgNode>],
  db_name: &str,          // NEW
  driver: &TypeDBDriver   // NEW
) -> Vec<Buffer_Cannot_Be_Saved> {
  let mut errors = Vec::new();

  // ...existing validation...

  // NEW: Merge validation
  let merge_errors = find_merge_errors(trees, db_name, driver).await;
  errors.extend(merge_errors);

  errors
}
```

**Call site update in `rust/save.rs`:**
```rust
let validation_errors: Vec<Buffer_Cannot_Be_Saved> =
  find_buffer_errors_for_saving(&orgnode_forest, &config.db_name, driver)
    .await.map_err(SaveError::DatabaseError)?;
```

---

## Phase 4: Extract Merge Info - TODO

### In orgnodes_to_save_instructions
**File:** `rust/save/orgnodes_to_instructions.rs`

```rust
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
  merge: merge_target,
};
```

---

## Phase 5: Core Merge Operations - TODO

### merge_in_graph Main Function
**File:** `rust/merge.rs` (add to existing)

```rust
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

  // 2. FIRST: Create MERGED nodes via SaveInstructions
  let mut merged_nodes: HashMap<ID, SkgNode> = HashMap::new();
  for (acquirer_node, acquiree_id) in &merge_instructions {
    let acquiree_data = fetch_node_data(&config.db_name, driver, config, acquiree_id).await?;
    let merged_node = create_merged_node(&acquiree_data);

    instructions.push((
      merged_node.clone(),
      NodeSaveAction { indefinitive: false, toDelete: false, merge: None }
    ));
    merged_nodes.insert(acquiree_id.clone(), merged_node);
  }

  update_graph(instructions.clone(), config.clone(), tantivy_index, driver).await?;

  // 3. NEXT: Perform edits (reroute relationships, update acquirer)
  for (acquirer_node, acquiree_id) in merge_instructions {
    let acquirer_id = &acquirer_node.ids[0];
    let acquiree_data = fetch_node_data(&config.db_name, driver, config, &acquiree_id).await?;
    let merged_node = merged_nodes.get(&acquiree_id).unwrap();

    merge_in_typedb(&config.db_name, driver, acquirer_id, &acquiree_id, &acquiree_data).await?;
    merge_in_fs(config, acquirer_id, &acquiree_id, &acquiree_data, &merged_node.ids[0])?;
  }

  // 4. LAST: Delete acquiree nodes
  let acquiree_ids: Vec<ID> = merge_instructions.iter().map(|(_, id)| id.clone()).collect();
  for acquiree_id in &acquiree_ids {
    delete_acquiree_node(&config.db_name, driver, config, tantivy_index, acquiree_id).await?;
  }

  Ok(())
}
```

### fetch_node_data
```rust
struct NodeData {
  title: String,
  body: Option<String>,
  contains_ids: Vec<ID>,
  extra_ids: Vec<ID>,
}

async fn fetch_node_data(
  db_name: &str,
  driver: &TypeDBDriver,
  config: &SkgConfig,
  node_id: &ID,
) -> Result<NodeData, Box<dyn Error>> {
  // Read .skg file
  let skg_node = read_node(&path_from_pid(config, node_id.clone()))?;

  // Query TypeDB
  let contains_ids = fetch_contains_ids(db_name, driver, node_id).await?;
  let extra_ids = fetch_extra_ids(db_name, driver, node_id).await?;

  Ok(NodeData {
    title: skg_node.title,
    body: skg_node.body,
    contains_ids,
    extra_ids,
  })
}
```

### TypeDB Queries
```rust
async fn fetch_contains_ids(db_name: &str, driver: &TypeDBDriver, container_id: &ID) -> Result<Vec<ID>, Box<dyn Error>> {
  // Query: match $container has id "...";
  //        $rel (container: $container, contained: $content) isa contains;
  //        $content has id $content_id;
  //        select $content_id;
  todo!()
}

async fn fetch_extra_ids(db_name: &str, driver: &TypeDBDriver, node_id: &ID) -> Result<Vec<ID>, Box<dyn Error>> {
  // Query: match $node has id "...";
  //        $rel (node: $node, extra_id: $extra) isa has_extra_id;
  //        $extra has id $extra_id_value;
  //        select $extra_id_value;
  todo!()
}
```

### merge_in_typedb
```rust
async fn merge_in_typedb(
  db_name: &str,
  driver: &TypeDBDriver,
  acquirer_id: &ID,
  acquiree_id: &ID,
  acquiree_data: &NodeData,
) -> Result<(), Box<dyn Error>> {
  let tx = driver.transaction(db_name, TransactionType::Write).await?;

  // Add acquiree's ID as extra_id
  add_extra_id(&tx, acquirer_id, acquiree_id).await?;

  // Transfer acquiree's extra_ids
  for extra_id in &acquiree_data.extra_ids {
    add_extra_id(&tx, acquirer_id, extra_id).await?;
  }

  // Reroute relationships
  let acquirer_contains = fetch_contains_ids(db_name, driver, acquirer_id).await?;
  reroute_relationships(&tx, acquirer_id, acquiree_id, &acquirer_contains).await?;

  tx.commit().await?;
  Ok(())
}
```

### Relationship Rerouting Functions

**reroute_contains_relationships:**
```rust
async fn reroute_contains_relationships(
  tx: &Transaction,
  acquirer_id: &ID,
  acquiree_id: &ID,
) -> Result<(), Box<dyn Error>> {
  // 1. Acquiree was container
  // Query: match $acquiree has id "acquiree_id";
  //        $rel (container: $acquiree, contained: $c) isa contains;
  // For each: delete $rel, insert new (container: $acquirer, contained: $c)

  // 2. Acquiree was contained
  // Query: match $acquiree has id "acquiree_id";
  //        $rel (container: $container, contained: $acquiree) isa contains;
  // For each: delete $rel, insert new (container: $container, contained: $acquirer)

  todo!()
}
```

**reroute_hyperlinks_relationships:**
```rust
async fn reroute_hyperlinks_relationships(
  tx: &Transaction,
  acquirer_id: &ID,
  acquiree_id: &ID,
) -> Result<(), Box<dyn Error>> {
  // Transfer wholesale (both directions)
  todo!()
}
```

**reroute_subscribes_relationships:**
```rust
async fn reroute_subscribes_relationships(
  tx: &Transaction,
  acquirer_id: &ID,
  acquiree_id: &ID,
) -> Result<(), Box<dyn Error>> {
  // Transfer wholesale (both directions)
  todo!()
}
```

**reroute_hides_relationships:**
```rust
async fn reroute_hides_relationships(
  tx: &Transaction,
  acquirer_id: &ID,
  acquiree_id: &ID,
  acquirer_contains: &[ID],
) -> Result<(), Box<dyn Error>> {
  // 1. If acquiree is HIDDEN → DROP
  // Query: match $rel (hider: $h, hidden: $acquiree);
  // For each: delete $rel (don't recreate)

  // 2. If acquiree is HIDER → conditional transfer
  // Query: match $rel (hider: $acquiree, hidden: $hidden);
  //        $hidden has id $hidden_id;
  // For each:
  //   if $hidden_id NOT in acquirer_contains:
  //     delete $rel, insert (hider: $acquirer, hidden: $hidden)
  //   else:
  //     delete $rel (don't recreate)

  todo!()
}
```

**reroute_overrides_relationships:**
```rust
async fn reroute_overrides_relationships(
  tx: &Transaction,
  acquirer_id: &ID,
  acquiree_id: &ID,
) -> Result<(), Box<dyn Error>> {
  // 1. If acquiree is REPLACEMENT → transfer
  // Query: match $rel (replacement: $acquiree, replaced: $r);
  // For each: delete $rel, insert (replacement: $acquirer, replaced: $r)

  // 2. If acquiree is REPLACED → DROP
  // Query: match $rel (replacement: $r, replaced: $acquiree);
  // For each: delete $rel (don't recreate)

  todo!()
}
```

### merge_in_fs
```rust
fn merge_in_fs(
  config: &SkgConfig,
  acquirer_id: &ID,
  acquiree_id: &ID,
  acquiree_data: &NodeData,
  merged_node_id: &ID,
) -> Result<(), Box<dyn Error>> {
  // MERGED node already created via SaveInstructions

  // Read acquirer's file
  let acquirer_path = path_from_pid(config, acquirer_id.clone());
  let mut acquirer_skg = read_node(&acquirer_path)?;

  // Update IDs
  acquirer_skg.ids.push(acquiree_id.clone());
  acquirer_skg.ids.extend(acquiree_data.extra_ids.clone());

  // Update contains: [MERGED] + acquirer's old + acquiree's old
  let mut new_contains = vec![merged_node_id.clone()];
  new_contains.extend(acquirer_skg.contains.clone());
  new_contains.extend(acquiree_data.contains_ids.clone());
  acquirer_skg.contains = new_contains;

  // Write updated acquirer
  write_node(&acquirer_skg, &Path::new(&acquirer_path))?;

  Ok(())
}
```

### delete_acquiree_node
```rust
async fn delete_acquiree_node(
  db_name: &str,
  driver: &TypeDBDriver,
  config: &SkgConfig,
  tantivy_index: &TantivyIndex,
  acquiree_id: &ID,
) -> Result<(), Box<dyn Error>> {
  // Delete from TypeDB
  let tx = driver.transaction(db_name, TransactionType::Write).await?;
  delete_node_by_iid(&tx, acquiree_id).await?;
  tx.commit().await?;

  // Delete from filesystem
  let acquiree_path = path_from_pid(config, acquiree_id.clone());
  if Path::new(&acquiree_path).exists() {
    fs::remove_file(&acquiree_path)?;
  }

  // Remove from Tantivy index
  remove_from_index(tantivy_index, acquiree_id)?;

  Ok(())
}
```

---

## Phase 6: Integration - TODO

### Call merge_in_graph from save flow
**File:** `rust/serve/save_buffer.rs`

```rust
async fn update_from_and_rerender_buffer(
  org_buffer_text: &str,
  typedb_driver: &TypeDBDriver,
  config: &SkgConfig,
  tantivy_index: &TantivyIndex
) -> Result<SaveResponse, Box<dyn Error>> {
  let (mut orgnode_forest, save_instructions) =
    buffer_to_save_instructions(org_buffer_text, config, typedb_driver).await?;

  // Apply user's edits
  update_graph(save_instructions.clone(), config.clone(), tantivy_index, typedb_driver).await?;

  // Apply merges
  merge_in_graph(save_instructions, config, tantivy_index, typedb_driver).await?;

  // Remove acquirees from forest before rendering
  remove_merged_nodes_from_forest(&mut orgnode_forest, &save_instructions);

  completeOrgnodeForest(&mut orgnode_forest, config, typedb_driver, &mut errors).await?;
  set_metadata_relationships_in_forest(&mut orgnode_forest, config, typedb_driver).await?;

  let content: String = render_forest_to_org(&orgnode_forest);
  Ok(SaveResponse { content, errors })
}
```

### remove_merged_nodes_from_forest
```rust
fn remove_merged_nodes_from_forest(
  forest: &mut Vec<Tree<OrgNode>>,
  instructions: &[SaveInstruction],
) {
  let acquiree_ids: HashSet<ID> = instructions
    .iter()
    .filter_map(|(_, action)| action.merge.as_ref())
    .cloned()
    .collect();

  if acquiree_ids.is_empty() {
    return;
  }

  // Remove trees whose root is an acquiree
  forest.retain(|tree| {
    if let Some(id) = &tree.root().value().metadata.id {
      !acquiree_ids.contains(id)
    } else {
      true
    }
  });

  // Remove acquiree nodes from within trees
  for tree in forest.iter_mut() {
    remove_nodes_recursively(tree, &acquiree_ids);
  }
}

fn remove_nodes_recursively(tree: &mut Tree<OrgNode>, acquiree_ids: &HashSet<ID>) {
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

  for node_id in node_ids_to_remove {
    if let Some(mut node_mut) = tree.get_mut(node_id) {
      node_mut.detach();  // ego_tree API
    }
  }
}
```

---

## Phase 7: Testing - Partially Complete

### Current Tests ✅
- `tests/merge/saveinstructions_from_the_merges_in_an_orgnode_forest.rs` - unit test for SaveInstruction generation

### TODO: Integration Test Structure
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
6. Verify results (acquirer updated, MERGED node exists, acquiree deleted, relationships rerouted)
