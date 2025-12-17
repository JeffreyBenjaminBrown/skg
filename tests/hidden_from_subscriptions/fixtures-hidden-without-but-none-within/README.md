# Hidden Without But None Within

Tests that `HiddenOutsideOfSubscribeeCol` appears when a hidden node is NOT in any subscribee's content.

## Graph Structure

```
R (subscriber)
├── contains: [R1]
├── subscribes_to: [E1, E2]
└── hides_from_its_subscriptions: [H]

# There are no relationships except 'contains' among the rest:
E1 # bare leaf
└── E11 # bare leaf
└── E12
    └── E121 # bare leaf
E2 # bare leaf
R1 # bare leaf
H  # bare leaf, hidden from R's subscriptions but absent from them
```

## Expected Behavior

- H is hidden but NOT in any subscribee's content → H appears in `HiddenOutsideOfSubscribeeCol`
- `HiddenOutsideOfSubscribeeCol` appears as the first child of `SubscribeeCol`
- No `HiddenInSubscribeeCol` in either state (H is not in any subscribee's content)

## Key Rule: Indefinitive Subscribees are Bare Leaves

When subscribees are **indefinitive** (not yet expanded), they appear as bare leaves with no children.

## Expected Output Structure

Initial view from R:
```
R
├── SubscribeeCol
│   ├── HiddenOutsideOfSubscribeeCol (first child)
│   │   └── HiddenFromSubscribees H
│   ├── Subscribee E1 (indefinitive, bare leaf)
│   └── Subscribee E2 (indefinitive, bare leaf)
└── R1 (ordinary child, after SubscribeeCol)
```

View from R with definitive views expanded at each subscribee:
```
R
├── SubscribeeCol
│   ├── HiddenOutsideOfSubscribeeCol (first child)
│   │   └── HiddenFromSubscribees H
│   ├── Subscribee E1 (expanded)
│   │   ├── E11
│   │   └── E12 (indefinitive - not recursively expanded)
│   └── Subscribee E2 (expanded, but no content)
└── R1 (ordinary child, after SubscribeeCol)
```
Note: E12's child E121 doesn't appear because E12 is indefinitive (only top-level expansion of subscribees).
