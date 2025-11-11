# Phase 5: Batching Analysis for Source Lookups

## The Performance Question

When Category B call sites need to read a node from disk but only have an ID, they'll need to query TypeDB for the source. Should we query one at a time, or batch multiple lookups?

## Existing Batch Pattern: `pids_from_ids`

The codebase already has a batching pattern in `rust/media/typedb/util/pids_from_ids.rs`:

```rust
pub async fn pids_from_ids(
  db_name: &str,
  driver: &TypeDBDriver,
  node_ids: &[ID]
) -> Result<HashMap<ID, Option<ID>>, Box<dyn Error>>
```

**How it works:**
1. Takes a slice of IDs to look up
2. Builds a single TypeDB query with disjunction (OR clauses) for all IDs
3. Uses nested subqueries to get all PIDs in one query
4. Returns HashMap mapping each input ID to its PID

**Where it's used:**
- `rust/save/buffer_to_orgnodes/add_missing_info.rs` - assigns PIDs to all nodes in a buffer
- Pattern: collect all IDs → batch query → assign results back

## Analysis of Category B Call Sites

Let me examine whether each call site processes single or multiple IDs:

### Single-ID Operations (Not Batchable)

1. **`rust/media/file_io/one_node.rs:24`** - `read_node_from_id`
   - Called with one specific ID to read
   - One-off operation

2. **`rust/media/file_io/one_node.rs:59`** - `fetch_aliases_from_file`
   - Called with one specific ID to read
   - One-off operation

3. **`rust/mk_org_text/content_view.rs:188,211`** - Content view (2 places)
   - Each builds one node at a time
   - Sequential, not batched

4. **`rust/save/orgnodes_to_instructions/none_node_fields_are_noops.rs:38`**
   - Validates one node at a time
   - Part of validation loop but nodes processed sequentially

5. **`rust/rebuild/complete_aliascol.rs:37`**
   - Called once per AliasCol during DFS traversal
   - Sequential processing

6. **`rust/rebuild/complete_contents.rs:237`**
   - Called once per node during DFS traversal
   - Sequential processing

### Potentially Batchable Operations

7. **`rust/merge/mergeInstructionTriple.rs:56,60`** - Reading for merge
   - Reads **2 nodes** in quick succession (acquirer and acquiree)
   - Could batch these 2 lookups
   - **But:** Only 2 IDs, marginal benefit

## Performance Considerations

### Arguments FOR batching:
- **Future-proofing:** If code evolves to process more nodes simultaneously, infrastructure is ready
- **Consistency:** Follows established pattern from `pids_from_ids`
- **Network efficiency:** One TypeDB query vs many, reduces round-trips

### Arguments AGAINST batching:
- **Current usage:** Most call sites process single IDs
- **Complexity:** Requires refactoring call sites to collect IDs first, then distribute results
- **Code churn:** Would need to change API contracts for many functions
- **Diminishing returns:** For single-ID lookups, batching adds overhead without benefit

## Recommendation

### Initial Implementation (Phase 5): Single-ID Lookups

Create simple helpers for single-ID lookups:

```rust
pub async fn pid_and_source_from_id(
  db_name: &str,
  driver: &TypeDBDriver,
  node_id: &ID
) -> Result<Option<(ID, String)>, Box<dyn Error>>
```

**Rationale:**
- Matches current usage patterns (mostly one-off lookups)
- Simple to implement and use
- Can be optimized later if profiling shows it's a bottleneck

### Future Optimization (If Needed): Add Batch Function

If profiling reveals source lookups are a bottleneck, add:

```rust
pub async fn pids_and_sources_from_ids(
  db_name: &str,
  driver: &TypeDBDriver,
  node_ids: &[ID]
) -> Result<HashMap<ID, Option<(ID, String)>>, Box<dyn Error>>
```

This would follow the same pattern as `pids_from_ids` but also fetch the source attribute.

**When to optimize:**
- If rebuilds become slow (complete_contents/complete_aliascol process many nodes)
- If merge operations become a hotspot
- If profiling shows TypeDB queries for source are significant

## Implementation Strategy

### Phase 5 Approach:

1. **Create single-ID helper** - simple, fits current usage
2. **Measure performance** during real usage
3. **Add batch function later** if measurements justify it

### Query Pattern:

The single-ID query would be similar to `pid_from_id` but also select source:

```rust
query: format!(
  r#"match
    $node isa node, has id "{}", has source $source;
    fetch {{ "source": $source }};"#,
  node_id
)
```

For the batch version (future), combine with `pids_from_ids` pattern:
- Build disjunction for all IDs
- Nested subquery to get both PID and source
- Return `HashMap<ID, Option<(ID, String)>>`

## Summary

**Don't optimize prematurely.** Start with single-ID lookups, add batching if profiling shows it's needed.

The existing `pids_from_ids` infrastructure proves batching is *possible* and shows us the pattern to follow. But most current call sites don't benefit from batching, so keep it simple initially.
