# Hidden Without But None Within

Tests that `HiddenOutsideOfSubscribeeCol` appears when a hidden node is NOT in any subscribee's content.

## Graph Structure

```
R (subscriber)
├── contains: [R1]
├── subscribes_to: [E1, E2]
└── hides_from_its_subscriptions: [H]

E1 # no relationships but 'contains'
└── E11
└── E12
    └── E121

# Everything else is a bare leaf:
E11, E12, E121, E2, R1, H
```
