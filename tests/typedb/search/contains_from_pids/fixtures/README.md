# Test Fixtures for contains_from_pids

This directory contains test fixtures for the `contains_from_pids` function.

## Containment Structure

```
1.skg
├── 2.skg
└── 3.skg
    └── 1.skg  # Note: 1 and 3 form a cycle

10.skg
├── 11.skg
└── 12.skg
```

## In words

- **1** contains **2** and **3**
- **3** contains **1** (forming a cycle with 1)
- **10** contains **11** and **12**

## Notes

- Each node's title matches its ID
- Nodes 1 and 3 form a cycle via the `contains` relationship
- Node 10 and its children (11, 12) are separate from the 1-3 cycle
