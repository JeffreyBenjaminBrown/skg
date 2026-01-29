# Git Diff View Implementation Plan

## Overview

Add a "git diff view" feature that shows users what has changed since the last git commit within Skg content buffers. When enabled, diff metadata is added to orgnodes, and removed nodes are inserted into the view.

Reference: `git-diff-view/big-plan.org` for detailed design decisions.

## Phase 1: Foundation - Types and git2 Integration — DONE

### 1.1 Add git2 crate dependency — DONE
- **File:** `server/Cargo.toml`
- Add `git2 = "0.18"` (or latest stable)

### 1.2 Add diff enums — DONE
- **File:** `server/types/orgnode.rs`
- Add after existing enums:
```rust
pub enum NodeDiff {
    New,        // Node did not exist in HEAD
    NewHere,    // Node existed but not in this relationship
    Removed,    // Node existed in HEAD, no longer on disk
    RemovedHere,// Node exists on disk but not in this relationship
    NotInGit,   // Source is not a git repo
}

pub enum FieldDiff {
    New,        // Value was not in list in HEAD
    Removed,    // Value was in list in HEAD, now gone
    NotInGit,   // Source is not a git repo
}
```

### 1.3 Add diff field to TrueNode — DONE
- **File:** `server/types/orgnode.rs`
- Add to `TrueNode` struct: `pub diff: Option<NodeDiff>`
- Update `Default` impl
- Update constructor functions: `mk_definitive_orgnode`, `mk_indefinitive_orgnode`, `mk_orgnode`

### 1.4 Add new Scaffold variants — DONE
- **File:** `server/types/orgnode.rs`
- Add to `Scaffold` enum:
  - `TextChanged` - indicates title/body changed
  - `IDCol` - collects ID changes (diff-mode only)
  - `ID(String)` - individual ID within IDCol
- Add to `ScaffoldKind` enum
- Update `REPRS_IN_CLIENT`, `kind()`, `title()`, `interp_str()`, `to_scaffold()`

### 1.5 Add diff field to Alias and ID scaffolds — DONE
- **File:** `server/types/orgnode.rs`
- Modify `Scaffold::Alias` to `Alias { text: String, diff: Option<FieldDiff> }`
- Add `Scaffold::ID { value: String, diff: Option<FieldDiff> }`

## Phase 2: Metadata Serialization — DONE

### 2.1 Update org_to_text for diff metadata — DONE
- **File:** `server/org_to_text.rs`
- In `truenode_metadata_to_string()`:
  - If `diff` is Some, add `(diff new)`, `(diff removed-here)`, etc.
- In `scaffold_metadata_to_string()`:
  - Handle TextChanged, IDCol, ID scaffolds
  - For Alias and ID with diff, add `(diff new)` etc.

### 2.2 Update parse_metadata_sexp for diff metadata — DONE
- **File:** `server/serve/parse_metadata_sexp.rs`
- Parse `(diff ...)` in both TrueNode and Scaffold contexts
- Store in appropriate field (even if discarded on save)

## Phase 3: Connection State and Request Handling — DONE

### 3.1 Add ConnectionState struct — DONE
- **File:** `server/serve.rs`
- Create struct:
```rust
struct ConnectionState {
    diff_mode_enabled: bool,
}
```
- Initialize in `handle_emacs()` thread
- Pass to handlers

### 3.2 Add diff mode toggle request — DONE
- **File:** `server/serve.rs` in `handle_emacs()`
- Parse `((request . "git diff mode") (value . 'on))` and `'off` variants
- Update `ConnectionState.diff_mode_enabled`
- Return success/failure response

### 3.3 Update handler signatures — DONE
- **Files:** `server/serve/handlers/single_root_view.rs`, `server/serve/handlers/save_buffer.rs`
- Add `diff_mode_enabled: bool` parameter to handlers
- Handlers now use this parameter to apply diff markers when enabled

## Phase 4: Git Operations Module — DONE

### 4.1 Create git operations module — DONE
- **File:** `server/git_ops.rs` (new)
- Functions:
  - `is_git_repo(source_path: &Path) -> bool`
  - `head_is_merge_commit(repo: &Repository) -> Result<bool, Error>`
  - `get_changed_skg_files(repo: &Repository) -> Result<Vec<PathDiffStatus>, Error>`
  - `get_file_content_at_head(repo: &Repository, path: &Path) -> Result<Option<String>, Error>`
- `PathDiffStatus` struct: `{ path: PathBuf, status: DiffStatus }` where status is Added/Modified/Deleted

### 4.2 Create diff computation module — DONE
- **File:** `server/diff.rs` (new)
- Functions:
  - `compute_diff_for_source(source: &str, source_path: &Path) -> Result<SourceDiff, Error>`
  - `diff_skg_nodes(old: &SkgNode, new: &SkgNode) -> NodeChanges`
  - `diff_lists<T>(old: &[T], new: &[T]) -> Vec<ListPathDiffStatus<T>>` (LCS-based)
- `SourceDiff` contains: changed files, their old/new SkgNode parses, computed diffs
- `NodeChanges` contains: text_changed, contains_diff, subscribes_diff, aliases_diff, ids_diff

## Phase 5: Rendering Pipeline Integration — DONE

### 5.1 Integrate diff into view rendering — DONE
- **File:** `server/to_org/render/content_view.rs`
- Modify `single_root_view()` and `multi_root_view()`:
  - If `diff_mode_enabled`:
    1. After `render_initial_forest_bfs()`, call new `apply_diff_to_forest()`
    2. This function walks the tree and:
       - Adds diff markers to existing nodes
       - Inserts removed/removed-here nodes (indefinitive)
       - Adds TextChanged scaffolds where needed
       - Adds IDCol scaffolds where IDs changed

### 5.2 Create diff application module — DONE
- **File:** `server/to_org/render/diff.rs` (new)
- Function `apply_diff_to_forest()`:
  - Takes forest, SourceDiff map, returns modified forest
  - For each TrueNode with changes:
    - Set `diff` field appropriately
    - For content changes: interleave removed-here/new-here per diff algorithm
    - For text changes: prepend TextChanged scaffold child
    - For ID changes: prepend IDCol with ID scaffold children
    - For alias changes: update AliasCol children with diff markers

### 5.3 Handle indefinitive for removed nodes — DONE
- Ensure nodes with `diff: Some(Removed)` or `diff: Some(RemovedHere)` are always indefinitive
- Modify `render_initial_forest_bfs()` or the diff application to enforce this
- **NOTE:** Implemented in `create_removed_node()` and `create_removed_here_node()` in diff.rs

## Phase 6: Save Pipeline Integration — DONE

### 6.1 Parse and retain diff metadata temporarily — DONE
- **File:** `server/serve/parse_metadata_sexp.rs`
- In `parse_metadata_to_orgnodemd()`:
  - Parse `(diff ...)` and store it in the diff field
  - This metadata is needed to identify which branches to strip

### 6.2 Strip removed node branches before save — DONE
- **File:** `server/serve/handlers/save_buffer.rs`
- In `update_from_and_rerender_buffer()`:
  - After parsing buffer, before `update_graph_minus_merges()`:
  - Walk tree and remove entire branches rooted at nodes marked removed/removed-here
  - EXCEPT: root nodes (ForestRoot children) and parent_ignores=true nodes
  - After stripping branches, clear diff metadata from remaining nodes
- Functions `remove_all_branches_marked_removed()` and `clear_diff_metadata()` are called before saving

### 6.3 Pre-save merge commit check — DONE
- **File:** `server/serve/handlers/save_buffer.rs`
- Before processing save:
  - For each source in the buffer, check `head_is_merge_commit()`
  - If any is true, abort entire save with error message
- Function `validate_no_merge_commits()` is called at the start when diff_mode_enabled

### 6.4 Recompute diff after save — DONE
- After successful save, if diff_mode_enabled:
  - The re-rendered buffer already comes from `update_from_and_rerender_buffer()`
  - Diff computation runs on the fresh view via `apply_diff_to_forest()`

## Phase 7: Definitive View from Git — DONE

### 7.1 Handle definitive view request for removed nodes — DONE
- **File:** `server/to_org/expand/definitive.rs`
- When expanding a node with `diff: Some(Removed)`:
  - Load content from git via `get_file_content_at_head()`
  - Parse as SkgNode using `from_git_replace_title_body()`
  - Render children via `extendDefinitiveSubtree_fromGit()`, marking those also missing from disk as `Removed`

### 7.2 Handle definitive view for removed-here nodes — DONE
- Node exists on disk, just moved
- Load from disk normally (current behavior)
- Show current content, not HEAD content

## File Summary

**New files:**
- `server/git_ops.rs` - git2 operations — DONE
- `server/diff.rs` - diff computation — DONE
- `server/to_org/render/diff.rs` - diff application to trees — DONE

**Modified files:**
- `server/Cargo.toml` - add git2 — DONE
- `server/types/orgnode.rs` - NodeDiff, FieldDiff enums; diff fields; new scaffolds — DONE
- `server/serve.rs` - ConnectionState, diff mode request handling — DONE
- `server/serve/handlers/single_root_view.rs` - pass diff_mode_enabled, apply to view — DONE
- `server/serve/handlers/save_buffer.rs` - strip removed nodes, merge commit check, recompute diff — DONE
- `server/serve/parse_metadata_sexp.rs` - parse diff metadata — DONE
- `server/org_to_text.rs` - serialize diff metadata — DONE
- `server/to_org/render/content_view.rs` - integrate diff computation — DONE
- `server/to_org/expand/definitive.rs` - handle removed node expansion from git — DONE

## Verification

### Unit tests
- Test diff_lists() with various reordering scenarios — DONE (5 tests)
- Test NodeDiff/FieldDiff serialization roundtrip
- Test parse_metadata_sexp with diff markers

### Integration tests
- Create test repo with .skg files
- Make changes (add, remove, modify, reorder)
- Enable diff mode, request view, verify markers
- Save, verify removed nodes stripped
- Test merge commit abort

### Manual testing
1. Start server with test data in git repo
2. Make uncommitted changes to .skg files
3. Enable diff mode via Emacs
4. View buffer, verify diff markers appear
5. Edit and save, verify removed nodes don't persist
6. Commit changes, verify diff markers disappear

## Implementation Status: COMPLETE

All phases have been implemented:
- ✓ Phase 5.1: `apply_diff_to_forest()` wired up in content_view.rs
- ✓ Phase 6.2/6.3: `remove_all_branches_marked_removed()`, `clear_diff_metadata()`, `validate_no_merge_commits()` called in save_buffer.rs
- ✓ Phase 6.4: Diff recomputation after save
- ✓ Phase 7: Definitive view expansion for removed nodes (loading from git via `from_git_replace_title_body()` and `extendDefinitiveSubtree_fromGit()`)
