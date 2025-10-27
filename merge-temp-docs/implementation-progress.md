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

---

## Types

### NodeRequest
**File:** `rust/types/orgnode.rs`

```rust
pub enum NodeRequest {
  ContainerwardView,
  SourcewardView,
  Merge(ID),
}
```

### SkgNode
**File:** `rust/types/skgnode.rs`

```rust
pub struct SkgNode {
  pub title: String,
  pub aliases: Option<Vec<String>>,
  pub ids: Vec<ID>,
  pub body: Option<String>,
  pub contains: Vec<ID>,
  pub subscribes_to: Vec<ID>,
  pub hides_from_its_subscriptions: Vec<ID>,
  pub overrides_view_of: Vec<ID>,
}
```

**Note:** The field is `contains`, not `content`.

### SaveInstruction
**File:** `rust/types/orgnode.rs`

```rust
pub type SaveInstruction = (SkgNode, NodeSaveAction);
```

### NodeSaveAction
**File:** `rust/types/save.rs`

```rust
pub struct NodeSaveAction {
  pub indefinitive: bool,
  pub toDelete: bool,
}
```

**Note:** Currently does NOT have `merge: Option<ID>` field (planned for later).

---

## Functions

### saveinstructions_from_the_merges_in_an_orgnode_forest
**File:** `rust/merge.rs`

```rust
pub async fn saveinstructions_from_the_merges_in_an_orgnode_forest(
  forest: &[Tree<OrgNode>],
  config: &SkgConfig,
  _driver: &TypeDBDriver,
) -> Result<Vec<SaveInstruction>, Box<dyn Error>>
```

**Purpose:** Generates SaveInstructions for merges found in the orgnode forest.

**Returns:** For each merge request, three SaveInstructions:
1. MERGED node (synthetic node with acquiree's title/body)
2. Updated acquirer (combined IDs and contents)
3. Acquiree marked toDelete

**Implementation details:**
- Reads .skg files directly using `read_node(&path_from_pid(config, id))`
- Combines contents: [MERGED_ID] + acquirer's old + acquiree's old
- Combines IDs: acquirer's IDs + acquiree_id

### parse_requests_sexp
**File:** `rust/serve/parse_headline_md_sexp.rs`

Parses `(requests (merge <id>))` syntax in org metadata.

### path_from_pid
**File:** `rust/util.rs`

```rust
pub fn path_from_pid(config: &SkgConfig, pid: ID) -> String
```

Constructs the file path for a node's .skg file.

### read_node
**File:** `rust/file_io/one_node.rs`

```rust
pub fn read_node<P: AsRef<Path>>(file_path: P) -> io::Result<SkgNode>
```

Reads and deserializes a .skg file (YAML format).

### read_node_from_id
**File:** `rust/file_io/one_node.rs`

```rust
pub async fn read_node_from_id(
  config: &SkgConfig,
  driver: &TypeDBDriver,
  node_id: &ID
) -> Result<SkgNode, Box<dyn Error>>
```

**Note:** This does a DB lookup via `pid_from_id` first. Not needed when you already have the primary ID.

---

## Test Files

- `tests/merge.rs` - test module registration
- `tests/merge/saveinstructions_from_the_merges_in_an_orgnode_forest.rs` - unit test
- `tests/merge/saveinstructions_from_the_merges_in_an_orgnode_forest/fixtures/` - test .skg files
