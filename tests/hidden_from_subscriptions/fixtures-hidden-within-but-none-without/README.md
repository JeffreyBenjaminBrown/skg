# Hidden Within But None Without

Tests behavior when all hidden nodes are inside subscribee content (none outside).

## Graph Structure

```
R (subscriber)
├── contains: [R1]
├── subscribes_to: [E1]
└── hides_from_its_subscriptions: [H]

E1 (subscribee)
└── contains: [E11, H, E12]   # H is hidden, E11 and E12 are not

// Bare leaves
R1
H
E11
E12
```
