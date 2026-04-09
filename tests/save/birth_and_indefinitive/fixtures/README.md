# Test Fixtures for birth=Independent and indefinitive

These fixtures test the interaction between `birth=Independent` and `indefinitive` metadata flags during buffer saves.

## Initial State

The `contains` relationship forms a simple structure:

1.skg (empty)
2.skg
└── 3.skg
4.skg (empty)

- Node 1 has no children
- Node 2 contains node 3
- Node 4 is an island

## Test Scenario

The test simulates saving this buffer:

```org
* (skg (node (id 1) (source main))) 1
** (skg (node (id 2) (source main) (birth independent) indefinitive)) 2
*** (skg (node (id 4) (source main))) 4
```

## Expected Behavior

After save:

1. **Node 2** should have `contains = [3, 4]`
   - Node 3 is retained (due to `indefinitive`)
   - Node 4 is appended (new child in the buffer)

2. **Node 1** should have `contains = []`
   - Remains empty despite having node 2 as an org-child
   - Node 2 does not affect its parent due to `birth=Independent`

This verifies that:
- `birth=Independent` prevents a child from updating its parent's contents
- `indefinitive` allows appending to existing contents rather than replacing them
- Both flags work correctly together through the full save pipeline
