# Fixtures-3: Multiple Sibling Groups

Tests BFS truncation with multiple sibling groups at different depths.

## Structure

```
1 (gen 1)
├─ 11 (gen 2)
│  ├─ 111 (gen 3)
│  └─ 112 (gen 3)
└─ 12 (gen 2)
   └─ 121 (gen 3)
```

## Purpose

Verifies that when the node limit is hit mid-generation:
1. The current sibling group is completed (both 111 and 112)
2. Other sibling groups in that generation are omitted (121 is not added)
3. Parent nodes after the limit parent are truncated (12 becomes indefinitive)
