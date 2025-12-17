# Overlapping Hidden Within

Tests behavior when a hidden node is in multiple subscribees' content.

## Graph Structure

```
R (subscriber)
├── contains: [R1]
├── subscribes_to: [E1, E2]
└── hides_from_its_subscriptions: [H]

R1 (ordinary child of R)

E1 (subscribee)
└── contains: [H]

E2 (subscribee)
└── contains: [H]

H (hidden node, in BOTH E1's and E2's content)
```

## Expected Behavior

- H is hidden AND is in both E1's and E2's content
- No `HiddenOutsideOfSubscribeeCol` because H is inside subscribees' content

## Key Rule: Indefinitive Subscribees are Bare Leaves

When subscribees are **indefinitive** (not yet expanded), they appear as bare leaves with no children.
H doesn't appear anywhere because it's inside indefinitive subscribees.

## Expected Output Structure

Initial view from R:
```
R
├── SubscribeeCol
│   ├── Subscribee E1 (indefinitive, bare leaf)
│   └── Subscribee E2 (indefinitive, bare leaf)
└── R1 (ordinary child, after SubscribeeCol)
```
Note: H doesn't appear because it's inside indefinitive subscribees.

View from R with definitive views expanded at each subscribee:
```
R
├── SubscribeeCol
│   ├── Subscribee E1
│   │   └── HiddenInSubscribeeCol
│   │       └── HiddenFromSubscribees H
│   └── Subscribee E2
│       └── HiddenInSubscribeeCol
│           └── HiddenFromSubscribees H
└── R1 (ordinary child, after SubscribeeCol)
```

## Why H Appears Twice (when expanded)

This is intentional. If the user wants to unhide H from just one subscription (E1 but not E2), they need to see it listed under each subscribee separately. The hiding is per-subscriber, but showing it per-subscribee lets the user understand where it would have appeared.
