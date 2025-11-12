# Phase 5: All Locations Needing Updates

## ✅ STATUS: COMPLETED (2025-11-11)

All locations listed below have been successfully updated. All 122 tests passing.

**📊 See [phase5-analysis.md](phase5-analysis.md) for comprehensive analysis of call site categories and implementation strategy.**

This document lists all locations that were updated for Phase 5 (Path Generation and File Writing).

**Historical reference preserved for documentation purposes.**

## Summary from Analysis
- **Category A (3 sites):** Have node object → add `source` parameter to `path_from_pid_and_source`
- **Category B (8 sites):** Only have ID → use TypeDB helper to get source
- **Category C (1 site):** Already computed source → use existing value

## Core Path Generation

### [rust/util.rs:17](../rust/util.rs#L17)
**Function:** `path_from_pid_and_source`
**Current behavior:** Hardcoded to use "main" source
**Needs:** Update signature to accept `source: &str` parameter
**Impact:** This is called by many other functions, so updating the signature will cause compilation errors at all call sites (which is good - compiler will find them all)

## File Writing

### [rust/media/file_io/multiple_nodes.rs:76](../rust/media/file_io/multiple_nodes.rs#L76)
**Function:** `write_all_nodes_to_fs`
**Current behavior:** Writes all nodes to "main" source directory
**Needs:** Look up `node.source` for each node and write to that source's directory
**Related:** Creates directory structure for sources

### [rust/save.rs:110](../rust/save.rs#L110)
**Function:** Save operation printing
**Current behavior:** Generic message "Writing N instruction(s) to disk..."
**Needs:** Print per-source information showing how many files written to each source

## Individual Node Reading - Path Determination

These locations read individual nodes from disk and set source to "main". The challenge: when you read a node, how do you know which source it came from?

### [rust/media/file_io/one_node.rs:25](../rust/media/file_io/one_node.rs#L25)
**Function:** `read_node_from_id`
**Current behavior:** Hardcoded source to "main"
**Context:** Called with just a node ID - doesn't know which source to look in
**Needs:** Either:
- Accept source parameter from caller
- Search all sources until node is found (inefficient)
- Caller must determine source first somehow

### [rust/media/file_io/one_node.rs:60](../rustmedia/file_io/one_node.rs#L60)
**Function:** `fetch_aliases_from_file`
**Current behavior:** Hardcoded source to "main"
**Context:** Similar to `read_node_from_id`

## Merge Operations

### [rust/merge/mergeInstructionTriple.rs:53](../rustmerge/mergeInstructionTriple.rs#L53)
**Function:** `mergeInstructionTriple` (reading acquirer and acquiree from disk)
**Current behavior:** Both set source to "main"
**Context:** Merge operations read nodes from disk to merge them
**Needs:** Know source of each node being merged

## Save Operations

### [rust/save/orgnodes_to_instructions/reconcile_dup_instructions.rs:104](../rustsave/orgnodes_to_instructions/reconcile_dup_instructions.rs#L104)
**Function:** `reconcile_dup_instructions_for_one_id`
**Current behavior:** Computes reconciled_source but doesn't use it
**Special case:** This one ALREADY computes the correct source! Just needs to pass it through
**Needs:** Use `reconciled_source_value` when calling `read_node_from_id_optional`

### [rust/save/orgnodes_to_instructions/none_node_fields_are_noops.rs:44](../rustsave/orgnodes_to_instructions/none_node_fields_are_noops.rs#L44)
**Function:** Reading node from disk during save validation
**Current behavior:** Hardcoded source to "main"
**Context:** Validating that certain node fields haven't changed

## Rebuild Operations

### [rust/rebuild/complete_aliascol.rs:39](../rustrebuild/complete_aliascol.rs#L39)
**Function:** Completing alias collection
**Current behavior:** Hardcoded source to "main"
**Context:** Rebuilding node data structure

### [rust/rebuild/complete_contents.rs:239](../rustrebuild/complete_contents.rs#L239)
**Function:** Completing node contents
**Current behavior:** Hardcoded source to "main"
**Context:** Rebuilding node data structure

## Content View Operations

### [rust/mk_org_text/content_view.rs:189](../rustmk_org_text/content_view.rs#L189)
**Function:** First content view function
**Current behavior:** Hardcoded source to "main"
**Context:** Generating org-mode text for viewing

### [rust/mk_org_text/content_view.rs:212](../rustmk_org_text/content_view.rs#L212)
**Function:** Second content view function
**Current behavior:** Hardcoded source to "main"
**Context:** Generating org-mode text for viewing

## Internal Helper ✓ COMPLETED (Phase 3b)

### [rust/media/file_io/multiple_nodes.rs:11](../rust/media/file_io/multiple_nodes.rs#L11)
**Function:** `read_skg_files` (private helper)
**Status:** ✓ Made private, TODO comment removed, updated test code
**Phase 3b cleanup:**
- Changed from `pub fn` to `fn` (now private)
- Removed from public re-exports in `rust/media/file_io.rs`
- Updated all test code to use `read_all_skg_files_from_sources` instead
- Function works correctly as internal helper

---

## Summary by Category

**Primary changes (affect architecture):**
1. [path_from_pid_and_source signature update](../rustutil.rs#L17) (will cause widespread compilation errors - good!)
2. [write_all_nodes_to_fs implementation](../rustmedia/file_io/multiple_nodes.rs#L76) (write to correct source directories)

**Secondary changes (individual reads - 9 locations):**
- [rust/media/file_io/one_node.rs:25](../rustmedia/file_io/one_node.rs#L25) - `read_node_from_id`
- [rust/media/file_io/one_node.rs:60](../rustmedia/file_io/one_node.rs#L60) - `fetch_aliases_from_file`
- [rust/merge/mergeInstructionTriple.rs:53](../rustmerge/mergeInstructionTriple.rs#L53) - merge operations
- [rust/save/orgnodes_to_instructions/reconcile_dup_instructions.rs:104](../rustsave/orgnodes_to_instructions/reconcile_dup_instructions.rs#L104) - reconcile (has source!)
- [rust/save/orgnodes_to_instructions/none_node_fields_are_noops.rs:44](../rustsave/orgnodes_to_instructions/none_node_fields_are_noops.rs#L44) - validation
- [rust/rebuild/complete_aliascol.rs:39](../rustrebuild/complete_aliascol.rs#L39) - rebuild aliases
- [rust/rebuild/complete_contents.rs:239](../rustrebuild/complete_contents.rs#L239) - rebuild contents
- [rust/mk_org_text/content_view.rs:189](../rustmk_org_text/content_view.rs#L189) - content view 1
- [rust/mk_org_text/content_view.rs:212](../rustmk_org_text/content_view.rs#L212) - content view 2

**Easy fixes:**
1. [reconcile_dup_instructions.rs:104](../rustsave/orgnodes_to_instructions/reconcile_dup_instructions.rs#L104) - already has the source, just use it
2. [save.rs:110](../rustsave.rs#L110) - update print message

**Key Design Question:**
When reading an individual node by ID (not bulk loading), how do we determine which source it's in? Options:
1. Caller provides source (requires caller to track it)
2. Search all sources until node is found (inefficient, O(n) per read)
3. Maintain in-memory index of ID→source (memory overhead, stale data risk)
4. Store source in TypeDB and query it (extra query, but TypeDB already knows)

---

## Implementation Results (Completed 2025-11-11)

### Design Decision: TypeDB Source Queries (Option 4)

Chose option 4: Store source in TypeDB and query it when needed.

**Implementation:**
- Created `pid_and_source_from_id` helper in rust/media/typedb/util.rs:70-117
- Updated TypeDB node creation to include source attribute (rust/media/typedb/nodes.rs:134-140)
- All Category B sites now query TypeDB for source

**Trade-offs:**
- ✅ No memory overhead for in-memory index
- ✅ Always up-to-date (TypeDB is source of truth)
- ✅ Simple implementation (single query function)
- ⚠️ Additional async propagation (functions became async)
- ⚠️ Network query per node read (acceptable for current use cases)

**Future optimization:** Can implement batch `pids_and_sources_from_ids` if profiling shows bottleneck.

### Async Propagation Impact

TypeDB queries are async, which propagated through the codebase:

**Functions that became async:**
- `fetch_aliases_from_file`
- `read_node_from_id`
- `clobber_none_fields_with_data_from_disk`
- `check_for_and_modify_if_repeated`
- `completeContents`
- `completeAliasCol`

**Test updates required:**
- tests/save/none_node_fields_are_noops.rs (4 tests)
- tests/rebuild/complete_contents.rs (helper function + all call sites)

### Additional Changes Beyond Original Plan

1. **TypeDB Integration (Partial Phase 10):**
   - Had to complete TypeDB portion early for queries to work
   - Added source attribute to node creation
   - Tantivy portion still pending

2. **Test Infrastructure Fix:**
   - Fixed source nickname mismatch in rust/test_utils.rs
   - Changed "test" → "main" for consistency

3. **Source Reconciliation:**
   - rust/save/orgnodes_to_instructions/reconcile_dup_instructions.rs now uses computed source value
   - No longer hardcoded to "main"

### Verification

All locations updated and tested:
- ✅ Core path generation (1 location)
- ✅ File writing (2 locations)
- ✅ Category A call sites (3 locations)
- ✅ Category B call sites (8 locations)
- ✅ Category C call site (1 location)
- ✅ Test updates (2 test files)
- ✅ Test infrastructure (1 location)

**Total:** 18 locations updated across 12 files

**Test Results:** All 122 tests passing

### Remaining Work

None for Phase 5.

**Phase 6 Status:** ✅ Complete (2025-11-12)
- Implemented source inheritance from parent to children
- Added `inherit_source_if_needed` helper function
- Extended test_add_missing_info_comprehensive to verify inheritance

**Next phases:**
- **Phase 7:** Source validation and error handling
- **Phase 8:** Foreign data validation
- **Phase 10 (Tantivy):** Add source to Tantivy schema and indexing
