# Suggestions for Testing

## Unit Tests

1. **Cycle detection:**
   - A→B→C→A should error
   - A→B, C→D (independent) should pass
   - A→B→C (chain without cycle) should pass

2. **Conflict detection:**
   - Node with both `merge` and `toDelete` should error
   - Acquirer marked for deletion should error
   - Acquiree marked for deletion should error

3. **Filesystem operations:**
   - Read .skg file
   - Write merged .skg file
   - Delete .skg file

4. **Relationship rerouting:**
   - Test each relationship type independently
   - Verify counts before/after

## Integration Test Structure

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
6. Verify in returned view:
   - A exists with 3 children: MERGED, X, Y, Z
   - MERGED child has title "MERGED: Acquiree" and B's body
   - B does not appear
7. Verify in database:
   - A has extra_id = B's ID
   - Query for B returns A (via extra_id)
   - A's contains relationships include all of: MERGED, X, Y, Z
8. Verify in filesystem:
   - A's file has correct IDs and content
   - B's file is deleted
   - MERGED node's file exists

---

# Potential Edge Cases

## 1. Acquirer Has No Content

If acquirer has no children, after merge it should have:
- MERGED node
- Acquiree's children

This is handled by the formula: `[MERGED] + [] + [acquiree content]`

## 2. Acquiree Has No Content

If acquiree has no children:
- MERGED node still created (with acquiree's title/body)
- Acquirer gains MERGED as first child
- Acquirer's other children remain

Formula: `[MERGED] + [acquirer content] + []`

## 3. Both Have No Content

Both have no children:
- Acquirer gains MERGED node as only child
- MERGED node contains acquiree's title/body
- This is valid

## 4. Multiple Merges Into Same Acquirer

**Scenario:** A→B and A→C (A wants to merge both B and C)

**Recommendation:** Disallow for first version. Add validation error if node has multiple `Merge` requests in its `nodeRequests` set. If supported later, would need careful ordering.

---

# Recommendations

## Start Simple
1. Implement Phases 1-4 first (types, parsing, validation, extraction)
2. Test validation thoroughly before moving to merge logic
3. Implement merge logic for one relationship type (e.g., `contains`)
4. Test that thoroughly
5. Extend to other relationship types

## Consider Breaking Into Smaller PRs
1. PR 1: Types and parsing
2. PR 2: Validation
3. PR 3: Merge logic (TypeDB only, one relationship type)
4. PR 4: Extend to all relationship types
5. PR 5: Filesystem merge
6. PR 6: Tantivy integration
7. PR 7: Integration tests

## Error Handling
Add detailed logging throughout merge operations:
- Log each relationship found
- Log each relationship deleted
- Log each relationship created
- This will be invaluable for debugging

## Documentation
Document the merge behavior in user-facing docs:
- What happens during a merge
- What the MERGED node is
- How extra_ids work
- Limitations (e.g., one merge per node)
