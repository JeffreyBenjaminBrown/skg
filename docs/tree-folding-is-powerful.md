# Folding branches lets you handle more data.

Folding a branch just means temporarily hiding its descendents. It lets you handle more data than you could otherwise, by letting you see only the parts you want to see at any given moment.

This idea is only a few decades old, because you really need electronic apps in order to conveniently fold trees.

## An example of folding
For instance, you might have a document like this:
```
  * 1
  ** 1a
  *** 1a.1
  *** 1a.2
  ... -- imagine about 400 more nodes like that here
  *** 1a.400
  ** 1b
  ...
```

Because of the 400 items between `1a` and `1b`, you would not be able to see both at the same time without folding. Worse, you would not be able to see `1` from `1b`, even though the title in `1` is (probably) critical for understanding the meaning of the title of `1b`.

If you fold the branch titled `1a`, you can see all three of `1`, `1a` and `1b` at the same time:
```
  * 1
  ** 1a [folded]
  ** 1b
  ...
```

This `folding` helps a user navigate complex trees without getting lost. Enormous, sprawling content becomes easy to view and reshape.
