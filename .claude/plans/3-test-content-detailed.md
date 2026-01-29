
# Test Plan: Git Diff View - Content Changes

## Goal

Write a test for the git diff view feature, specifically testing how **ordinary content (contains) changes** are treated. This is "belated TDD" — the test describes expected behavior that is not yet fully implemented.

## Test Scenario (from `git-diff-view/test-spec.org`)

### Initial state (committed in git HEAD):
```
Node 1 contains [11, 12]
  Node 11 contains [gets-removed]
  Node 12 contains [moves]
```

### Worktree state (uncommitted changes):
```
Node 1 contains [11, 12]
  Node 11 contains [moves]    ← "moves" moved here from 12
  Node 12 contains []         ← empty now
Node "new" exists (top-level) ← completely new
```
Note: `gets-removed` is deleted from disk entirely.

### Expected ordinary view (no diff mode):
```
* 1
** 11
*** moves
** 12
* new
```

### Expected git diff view:
```
* 1
** 11
*** (skg .. (diff removed)) gets-removed     ← deleted from disk
*** (skg .. (diff new-here)) moves           ← moved TO here
** 12
*** (skg .. (diff removed-here)) moves       ← phantom: was here in git
* (skg ... (diff new)) new                   ← entirely new file
```

## Test Implementation

### File: `tests/git_diff_view.rs`

### Test Setup

1. **Create temp directory** with git repo
2. **Create .skg fixture files** for the "git HEAD" state:
   - `1.skg`: contains [11, 12]
   - `11.skg`: contains [gets-removed]
   - `12.skg`: contains [moves]
   - `gets-removed.skg`: exists
   - `moves.skg`: exists
3. **Commit** these files
4. **Modify worktree** to new state:
   - Edit `11.skg`: contains [moves]
   - Edit `12.skg`: contains []
   - Delete `gets-removed.skg`
   - Create `new.skg`
5. **Do NOT commit** — this creates the diff

### Test Execution

1. Call `multi_root_view()` with:
   - `root_ids = [ID("1"), ID("new")]`
   - `diff_mode_enabled = true`

2. **Verify** the output string contains expected diff markers:
   - `gets-removed` has `(diff removed)`
   - `moves` under 11 has `(diff new-here)`
   - `moves` under 12 has `(diff removed-here)` (phantom node)
   - `new` has `(diff new)`

### Test Helper Functions Needed

- `setup_git_repo_with_skg_files()` — creates repo and commits initial state
- `write_skg_file()` — helper to write .skg YAML files
- **S-expression parsing for assertions:**
  - Parse each headline's `(skg ...)` metadata as a formal S-expression
  - Extract the `(diff ...)` sub-expression if present
  - Verify expected diff values, ignoring other metadata fields like `(id ...)`, `(source ...)`, `(graphStats ...)`

### Assertion Strategy

Use existing `parse_metadata_to_orgnodemd()` from `server/serve/parse_metadata_sexp.rs`:

```rust
use skg::serve::parse_metadata_sexp::parse_metadata_to_orgnodemd;
use skg::types::git::NodeDiffStatus;

// For each headline in the output:
// 1. Extract the (skg ...) metadata string
// 2. Parse using existing infrastructure
// 3. Check truenode_diff field

fn assert_headline_has_diff(
  output: &str,
  title: &str,
  expected_diff: Option<NodeDiffStatus>
) {
  // Find line containing the title
  // Extract (skg ...) metadata
  // Parse with parse_metadata_to_orgnodemd()
  // Assert metadata.truenode_diff == expected_diff
}
```

The parser already handles:
- `(node ... (diff new-here) ...)` → `truenode_diff: Some(NodeDiffStatus::NewHere)`
- `(skg (diff removed) alias)` → `scaffold_diff: Some(FieldDiffStatus::Removed)`

Even better: use `headline_to_triple()` from `uninterpreted.rs`:
```rust
use skg::from_text::buffer_to_orgnodes::uninterpreted::headline_to_triple;

// Returns (level, Option<OrgnodeMetadata>, title)
// OrgnodeMetadata.truenode_diff has the diff status
```

## Critical Files

### For the test:
- `tests/git_diff_view.rs` (new file)
- `tests/git_diff_view/` (new fixtures directory, created programmatically in test)

### Existing test patterns to follow:
- `tests/git_ops.rs` — git repo setup pattern with `tempfile::TempDir`
- `tests/content_view.rs` — `run_with_test_db` pattern for TypeDB integration

## Test Structure

```rust
// tests/git_diff_view.rs

use git2::Repository;
use tempfile::TempDir;
use std::fs;

#[test]
fn test_content_diff_with_moved_and_deleted_nodes()
  -> Result<(), Box<dyn Error>>
{
  // 1. Create temp dir
  let temp_dir = TempDir::new()?;
  let fixtures_path = temp_dir.path();

  // 2. Init git repo
  let repo = Repository::init(fixtures_path)?;
  configure_git_user(&repo);

  // 3. Write "git HEAD" state .skg files:
  //    1.skg: contains [11, 12]
  //    11.skg: contains [gets-removed]
  //    12.skg: contains [moves]
  //    gets-removed.skg: exists
  //    moves.skg: exists
  write_initial_skg_files(fixtures_path);

  // 4. Commit all files
  commit_all(&repo, "Initial commit");

  // 5. Modify to "worktree" state:
  //    - Edit 11.skg: contains [moves]
  //    - Edit 12.skg: contains []
  //    - Delete gets-removed.skg
  //    - Create new.skg
  modify_to_worktree_state(fixtures_path);

  // 6. Setup TypeDB from current worktree state
  //    (run_with_test_db won't work here - need custom setup
  //     that uses fixtures_path as the source)

  // 7. Call multi_root_view with:
  //    - root_ids = [ID("1"), ID("new")]
  //    - diff_mode_enabled = true

  // 8. Assert output contains:
  //    - gets-removed with (diff removed)
  //    - moves under 11 with (diff new-here)
  //    - moves under 12 with (diff removed-here)
  //    - new with (diff new)
}
```

### Custom Test Setup (not `run_with_test_db`)

Since we need to:
1. Control git history (commit initial state)
2. Then modify worktree without committing
3. Populate TypeDB from the modified worktree

We need a custom async setup similar to `setup_test_tantivy_and_typedb_dbs` but that:
- Takes the temp fixtures path (already modified)
- Configures `SkgConfig.sources` to point there
- Populates TypeDB from current disk state

## What This Test Will Expose

When run, this test will **fail** because the implementation is incomplete:

1. `NodeChanges` lacks `contains_diff: ListDiff<ID>` field
2. `compare_skgnodes()` doesn't compute contains diff
3. `apply_diff_to_forest()` doesn't:
   - Insert phantom `RemovedHere` nodes
   - Mark moved nodes as `NewHere`
   - Insert `Removed` nodes for deleted content

The test documents expected behavior; implementation follows.

## Verification

Run the test:
```bash
cargo nextest run --test git_diff_view
```

Initially: Test should fail (expected behavior not implemented)
After implementation: Test should pass

## Summary of Test Assertions

| Node | Location | Expected Diff | Reason |
|------|----------|---------------|--------|
| `1` | root | None | Existed, still exists |
| `11` | under 1 | None | Existed, still exists |
| `gets-removed` | under 11 | `Removed` | Deleted from disk entirely |
| `moves` | under 11 | `NewHere` | Moved here from under 12 |
| `12` | under 1 | None | Existed, still exists |
| `moves` | under 12 | `RemovedHere` | Phantom: was here in git, moved away |
| `new` | root | `New` | Completely new file |

---

## Part 2: Save Behavior Tests

After loading the git diff view, test what happens when you edit and save.

### Test: Delete *either* 'removed' node and save

**Setup:** Start with the git diff view from Part 1
**Action:** Remove either:
- `gets-removed` (the `(diff removed)` node under 11), OR
- `moves` under 12 (the `(diff removed-here)` phantom)

**Expected:**
- Nothing changes on disk
- The deleted node **respawns** in the returned buffer with its original diff marker

### Test: Delete the 'new-here' node and save

**Setup:** Start with the git diff view from Part 1
**Action:** Remove `moves` under 11 (the one with `(diff new-here)`) from the buffer, then save

**Expected:**
- On disk: `11.skg` now has `contains: []` (moves removed from 11's contains)
- In returned buffer:
  - `moves` no longer appears under 11
  - **BUT** `moves` still appears under 12 with `(diff removed-here)` (because it was there in git HEAD)

### Test: Add a new child 'newer' under 12 and save

**Setup:** Start with the git diff view from Part 1
**Action:** Add a new node `newer` as a child of 12, then save

**Expected:**
- On disk: `newer.skg` created, `12.skg` updated with `contains: [newer]`
- In returned buffer, 12's branch should look like:
  ```
  ** 12
  *** (skg .. (diff removed-here)) moves
  *** (skg .. (diff new)) newer
  ```
  (order of children may vary, but exactly those two)

### Save Test Assertions Summary

| Test | Action | Disk Change | Returned Buffer |
|------|--------|-------------|-----------------|
| Delete removed | Remove `gets-removed` from buffer | None | Respawns with `(diff removed)` |
| Delete removed-here | Remove phantom `moves` under 12 | None | Respawns with `(diff removed-here)` |
| Delete new-here | Remove `moves` under 11 | `11.skg` updated | Gone from 11; still under 12 as `removed-here` |
| Add new child | Add `newer` under 12 | `newer.skg` + `12.skg` | 12 has two children: `moves` (removed-here) + `newer` (new) |

---

## Files to Create

1. `tests/git_diff_view.rs` - the test file
2. (Optional) `tests/git_diff_view/mod.rs` - if helper functions get large
