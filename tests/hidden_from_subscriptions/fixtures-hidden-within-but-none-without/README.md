# Hidden Within But None Without

Tests behavior when all hidden nodes are inside subscribee content (none outside).

## Graph Structure

```
R (subscriber)
├── contains: [R1]
├── subscribes_to: [E1]
└── hides_from_its_subscriptions: [H]

R1 (ordinary child of R)

E1 (subscribee)
└── contains: [E11, H, E12]   # H is hidden, E11 and E12 are not

E11 (not hidden, in E1's content)
H (hidden, in E1's content)
E12 (not hidden, in E1's content)
```

## Expected Behavior

- H is hidden AND is in E1's content
- No `HiddenOutsideOfSubscribeeCol` because H is inside a subscribee

## Key Rule: Indefinitive Subscribees are Bare Leaves

When E1 is **indefinitive** (not yet expanded), it appears as a bare leaf with no children.
H doesn't appear anywhere because it's inside an indefinitive subscribee.

## Expected Output Structure

Initial view from R:
```
R
├── SubscribeeCol
│   └── Subscribee E1 (indefinitive, bare leaf)
└── R1 (ordinary child, after SubscribeeCol)
```
Note: H, E11, E12 don't appear because E1 is indefinitive.

View from R with definitive views expanded at each subscribee:
```
R
├── SubscribeeCol
│   └── Subscribee E1
│       ├── HiddenInSubscribeeCol (before content)
│       │   └── HiddenFromSubscribees H
│       ├── E11 (content, after HiddenInSubscribeeCol)
│       └── E12 (content, after HiddenInSubscribeeCol)
└── R1 (ordinary child, after SubscribeeCol)
```
Note: HiddenInSubscribeeCol precedes content regardless of .skg order.
