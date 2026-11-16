# Overlapping Hidden Within

Tests behavior when a hidden node is in multiple forkees' content.

## Graph Structure

```
R (forker)
├── contains: [R1]
├── forks: [E1, E2]
└── hides: [H]

E1 (forkee)
└── contains: [H]

E2 (forkee)
└── contains: [H]

# Bare leaves
H
R1
```
