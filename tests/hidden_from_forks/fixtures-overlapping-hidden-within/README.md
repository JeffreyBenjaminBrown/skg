# Overlapping Hidden Within

Tests behavior when a hidden node is in multiple subscribees' content.

## Graph Structure

```
R (subscriber)
├── contains: [R1]
├── forks: [E1, E2]
└── hides: [H]

E1 (subscribee)
└── contains: [H]

E2 (subscribee)
└── contains: [H]

# Bare leaves
H
R1
```
