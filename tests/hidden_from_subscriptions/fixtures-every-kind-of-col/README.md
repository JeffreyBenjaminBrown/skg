# Every Kind of Col

Tests that both `HiddenOutsideOfSubscribeeCol` and `HiddenInSubscribeeCol` collectors appear when needed.

## PITFALL: id "E" <=> title "subscribee"

Most nodes have titles equal to their IDs.
Subscribees don't, though.
The test function 'add_definitive_view_request_to_subscribees'
needs an easy way to find them,
so in their titles (which used to match)
I have replaced "E" with "subscribee-".

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
