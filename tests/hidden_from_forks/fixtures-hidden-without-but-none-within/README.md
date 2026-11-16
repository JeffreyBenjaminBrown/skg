# Hidden Without But None Within

Tests that `HiddenOutsideOfForkPartnersFolder` appears when a hidden node is NOT in any forkee's content.

## Graph Structure

```
R (forker)
├── contains: [R1]
├── forks: [E1, E2]
└── hides: [H]

E1 # no relationships but 'contains'
└── E11
└── E12
    └── E121

# Everything else is a bare leaf:
E11, E12, E121, E2, R1, H
```
