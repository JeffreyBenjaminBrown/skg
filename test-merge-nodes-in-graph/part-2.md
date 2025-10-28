# Test Specification: merge_nodes_in_graph

## Test: Merging 1 into 2

When we merge node 1 into node 2:
- **Acquirer**: 2 (remains after merge)
- **Acquiree**: 1 (will be deleted)

### Change the Data

Use `saveinstructions_from_the_merges_in_an_orgnode_forest` on an appropriate forest (all it needs is a single node, node 2, with a merge instruction `(merge 1)`) to generate the SaveInstructions.

Run `merge_nodes_in_graph`.

### Then Verify

#### In TypeDB

1. **Node 1 is gone** - queries for node 1 should return nothing (or should resolve to node 2 via extra_id)

2. **Node 2 has three ids**: `2`, `2-extra-id` and `1`
   - After merge: 2 gains `1` as an additional extra_id

3. **Node 2 now contains five things**: [MERGED_ID, 21, 22, 11, 12] (in that order)
   - MERGED_ID is the UUID of the newly created MERGED node
   - Original 2's contents [21, 22] come after MERGED
   - Original 1's contents [11, 12] come last

4. **Hyperlinks rerouted**:
   - The hyperlink from 1 to 1-links-to is now from 2 to 1-links-to
   - The hyperlink from links-to-1 to 1 is now from links-to-1 to 2

5. **Subscribes relationships rerouted**:
   - Node 2 now subscribes to [1-subscribes-to] (transferred from 1)
   - Node subscribes-to-1 now subscribes to [2] instead of [1]

6. **Hides_from_its_subscriptions relationships**:
   - Node 2 now hides [hidden-from-1s-subscriptions] (transferred from 1, since hidden-from-1s-subscriptions is NOT in 2's contents)
   - Node hides-1-from-subscriptions should now ONLY hide [11], NOT [1, 11]
     - The relationship hiding 1 is DROPPED (1 no longer exists)
     - The relationship hiding 11 is kept

7. **Overrides_view_of relationships**:
   - Node 2 now overrides view of [1-overrides-view-of] (transferred from 1)
   - Node overrides-view-of-1 no longer overrides anything related to this merge
     - The relationship overriding 1 is DROPPED (1 no longer exists to be replaced)

#### On Disk

1. **1.skg is gone** - the file should be deleted

2. **2.skg has been updated**:
   - ids: [2, 2-extra-id, 1] (order: primary id first, then merged ids)
   - title: "2" (unchanged)
   - body: "2 body" (unchanged)
   - contains: [MERGED_ID, 21, 22, 11, 12] where MERGED_ID is the UUID of the MERGED node

3. **New MERGED node file exists** at `MERGED_ID.skg`:
   - ids: [MERGED_ID] (the random UUID)
   - title: "MERGED: 1"
   - body: "[[id:1-links-to][a link to 1-links-to]]" (copied from 1's body)
   - contains: [] (MERGED nodes have no contents)

#### In Tantivy

1. **"1" will NOT turn up in search results** as a standalone node
   - Searching for "1" may return nodes like "11", "12", "21", "1-links-to", etc. where "1" is a substring
   - But node 1 itself should not appear as a search result

2. **"2" WILL turn up in search results** (unchanged behavior)

3. **"MERGED: 1" WILL turn up in search results** (the new MERGED node is indexed)
