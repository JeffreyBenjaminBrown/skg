# Every Kind of Col

Tests that both `HiddenOutsideOfSubscribeeCol` and `HiddenInSubscribeeCol` collectors appear when needed.

## Graph Structure

```
R (subscriber)
├── contains: [R1]
├── subscribes_to: [E1, E2]
└── hides_from_its_subscriptions: [hidden-in-E1, hidden-in-E2, hidden-for-no-reason]

E1 (subscribee)
└── contains: [hidden-in-E1, E11]   # hidden first, then non-hidden

E2 (subscribee)
└── contains: [E21, hidden-in-E2]   # non-hidden first, then hidden

# The rest of these are leaves: just a title and an ID
R1 (ordinary child of R)
hidden-in-E1 (hidden, in E1's content)
hidden-in-E2 (hidden, in E2's content)
hidden-for-no-reason (hidden, NOT in any subscribee's content)
E11 (not hidden, in E1's content)
E21 (not hidden, in E2's content)
```

## Expected Behavior

- hidden-for-no-reason is hidden but NOT in any subscribee's content → appears in `HiddenOutsideOfSubscribeeCol`
- hidden-in-E1 is hidden AND is in E1's content → appears in `HiddenInSubscribeeCol` under E1 (when E1 is expanded)
- hidden-in-E2 is hidden AND is in E2's content → appears in `HiddenInSubscribeeCol` under E2 (when E2 is expanded)
- E11 and E21 are NOT hidden → appear as normal content (when subscribees are expanded)

## Key Rule: Indefinitive Subscribees are Bare Leaves

When a subscribee is **indefinitive** (not yet expanded), it appears as a bare leaf with no children.
`HiddenInSubscribeeCol` only appears when the subscribee is **definitively expanded**.

## Ordering Rule (when expanded)

**HiddenInSubscribeeCol always precedes content in the view**, regardless of the order in the .skg files:
- E1's .skg has `contains: [hidden-in-E1, E11]` but view shows HiddenInSubscribeeCol before E11
- E2's .skg has `contains: [E21, hidden-in-E2]` but view shows HiddenInSubscribeeCol before E21

## Expected Output Structure

Initial view from R:
```
R
├── SubscribeeCol
│   ├── HiddenOutsideOfSubscribeeCol (first child)
│   │   └── HiddenFromSubscribees hidden-for-no-reason
│   ├── Subscribee E1 (indefinitive, bare leaf)
│   └── Subscribee E2 (indefinitive, bare leaf)
└── R1 (ordinary child, after SubscribeeCol)
```
Note: hidden-in-E1 and hidden-in-E2 don't appear because they're inside indefinitive subscribees.

View from R with definitive views expanded at each subscribee:
```
R
├── SubscribeeCol
│   ├── HiddenOutsideOfSubscribeeCol (first child)
│   │   └── HiddenFromSubscribees hidden-for-no-reason
│   ├── Subscribee E1
│   │   ├── HiddenInSubscribeeCol (before content)
│   │   │   └── HiddenFromSubscribees hidden-in-E1
│   │   └── E11 (content, after HiddenInSubscribeeCol)
│   └── Subscribee E2
│       ├── HiddenInSubscribeeCol (before content)
│       │   └── HiddenFromSubscribees hidden-in-E2
│       └── E21 (content, after HiddenInSubscribeeCol)
└── R1 (ordinary child, after SubscribeeCol)
```
