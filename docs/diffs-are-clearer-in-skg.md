# Skg clarifies diffs

For a description of the data structure that makes this possible, see [[the schema](schema.tql)] and [[the content model](docs/content-model.md)].

Suppose file F contains two branches A and B, and both are big, and you swapped their order. In a system that represents F with a single file, the resulting diff is very large, and it takes some effort to determine that all you did was change the order of the two branches. In Skg, by contrast, F's representation on disk contains only two nodes, so the diff could not be simpler: The order of the two nodes in F changed. Their content is in separate files, and because it has not changed, it does not clutter the diff with irrelevant noise.

## A concrete example

Suppose you start with a file titled `1`:
```
  * 1
  ** 1a
  *** 1a1
  *** 1a2
  ** 1b
  *** 1b1
  *** 1b2
```

and then swap the order of the branches 1a and 1b:
```
  * 1
  ** 1b
  *** 1b1
  *** 1b2
  ** 1a
  *** 1a1
  *** 1a2
```

If `1` is represented as a single file, this is the diff:
```
   * 1
-  ** 1a
-  *** 1a1
-  *** 1a2
   ** 1b
   *** 1b1
   *** 1b2
+  ** 1a
+  *** 1a1
+  *** 1a2
```

If the branches 1a and 1b were bigger, that diff would be correspondingly bigger. The section of "-" symbols must be compared line-by-line to the section of "+" symbols before one can be sure that all that happened was a change in the order of 1a and 1b.

By contrast, using Skg's representation, the tree `1` would be represented using three separate files:
```
  * 1
  ** 1a
  ** 1b
```

```
  ** 1a
  *** 1a1
  *** 1a2
```

```
  ** 1b
  *** 1b1
  *** 1b2
```

The diff would involve only the first of those three files, and would look like this:
```
   * 1
-  ** 1a
   ** 1b
+  ** 1a
```

This greatly clarifies what happened.

This argument does not just extend to the case where the two branches were otherwise unchanged. If there are further changes inside each branch, Skg will have the same clarifying effect on those changes.
