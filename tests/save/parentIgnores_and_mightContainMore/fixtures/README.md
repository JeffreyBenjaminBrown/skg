# Test Fixtures for parentIgnores and mightContainMore

These fixtures test the interaction between `treatment=parentIgnores` and `mightContainMore` metadata flags during buffer saves.

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
* (skg (id 1)) 1
** (skg (id 2) (treatment parentIgnores) mightContainMore) 2
*** (skg (id 4)) 4
```

## Expected Behavior

After save:

1. **Node 2** should have `contains = [3, 4]`
   - Node 3 is retained (due to `mightContainMore`)
   - Node 4 is appended (new child in the buffer)

2. **Node 1** should have `contains = []`
   - Remains empty despite having node 2 as an org-child
   - Node 2 does not affect its parent due to `treatment=parentIgnores`

This verifies that:
- `parentIgnores` prevents a child from updating its parent's contents
- `mightContainMore` allows appending to existing contents rather than replacing them
- Both flags work correctly together through the full save pipeline
