# Test Fixture Files

## The files 1.skg through 5.skg

### Contains Relationships
The `contains` relationship forms a simple tree:

```
1.skg
├── 2.skg
└── 3.skg (via alias 33)
```

Note: Files 4 and 5 are islands w/r/t the `contains` relationship.

### Hyperlinks
- **5.skg** contains hyperlinks to:
  - `[[id:22][hyperlink to another file]]` → 2.skg (via alias 22)
  - `[[id:33][to the third]]` → 3.skg (via alias 33)
  - `[[id:55][even to itself]]` → 5.skg (via alias 55, self-link)
- **4.skg** contains a hyperlink to something not in these fixtures:
  - `[[id:shgulasdghu][test]]`

### Subscription Relationships
- **2.skg** subscribes to: 4.skg, 5.skg
- **3.skg** subscribes to: 4.skg (via alias 44), 5.skg (via alias 55)

### Hide/Override Relationships
- **1.skg** hides from its subscriptions: 4.skg, 5.skg (via alias 55)
- **5.skg** overrides view of: 3.skg, 4.skg

### Aliases
Multiple IDs can refer to the same file:
- 2.skg has IDs: `2`, `22`
- 3.skg has IDs: `3`, `33`
- 4.skg has IDs: `4`, `44`
- 5.skg has IDs: `5`, `55`

## The files a.skg through c.skg

They have the contains relationships 'a → b → c → b',
which implies a cycle from b to c and back.

- **a** contains b (nothing contains a)
- **b** contains c
- **c** contains b
