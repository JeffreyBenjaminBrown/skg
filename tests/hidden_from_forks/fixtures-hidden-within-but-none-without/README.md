# Hidden Within But None Without

Tests behavior when all hidden nodes are inside forkee content (none outside).

## Graph Structure

```
R (forker)
├── contains: [R1]
├── forks: [E1]
└── hides: [H]

E1 (forkee)
└── contains: [E11, H, E12]   # H is hidden, E11 and E12 are not

// Bare leaves
R1
H
E11
E12
```
