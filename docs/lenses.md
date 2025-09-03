# "Lenses": Subscription and overriding in detail

TODO: These examples are goofy. I intend to use more plausible ones.

## An example
Suppose this document, `mysticism`, is yours:
```
  * mysticism
  ** imagination
  ** God
  ** common objects with mystical associations
  *** black cats
  *** the moon
```

And suppose this document, `eggs`, belongs to your friend Egg Man:
```
  * eggs
  ** health
  *** protein
  *** cholesterol
  ** recipes
  ** mystical
  *** Which came first, the chicken or the egg?
```

You might choose to move the `mystical` branch from `eggs` into your `mysticism` document, under `common objects with mystical associations`. I will not call the new node a "copy", but rather a `clone` of `mystical` from `eggs`. Your document after integrating the clone would look like this:
```
  * mysticism
  ** imagination
  ** God
  ** common objects with mystical associations
  *** black cats
  *** the moon
  *** eggs / mystical -- CLONED from Egg Man's "eggs"
  **** Which came first, the chicken or the egg?
```
(Notice that the title of the root of the new branch, `eggs / mystical`, joins two separate titles found in `eggs`. That might be the default, but you can also choose some other title instead. The point is that `eggs / mystical` in your `mysticsm` comes from `mystical` in Egg Man's `eggs`.)

Suppose Egg Man then extends the original `eggs` note, so it looks like this:
```
  * eggs
  ** health
  ** recipes
  ** mystical
  *** Which came first, the chicken or the egg?
  *** Egg limpia (curanderismo) -- NEW
  *** The philosophical egg     -- NEW
```

Your clone in "mysticsm" of the original "eggs / mystical" is by default `subscribed` to the original. While it is subscribed (which you can change), after Egg Man adds the new nodes, when you look at your "mysticism" you might see the following:
```
  * mysticism
  ** imagination
  ** God
  ** common objects with mystical associations
  *** black cats
  *** the moon
  *** eggs / mystical -- CLONED from Egg Man's "eggs"
  **** Which came first, the chicken or the egg?
  **** <skg< unintegrated subscribed content from Egg Man's document "eggs" >>
  ***** Egg limpia (curanderismo)
  ***** The philosophical egg
```
The node `<skg< unintegrated subscribed content from Egg Man's document "eggs" >>` was automatically generated, based on the subscription from your node titled `eggs / mystical` to the node titled `mystical` in Egg Man's document `eggs`.

Since [branches can be folded](tree-folding-is-powerful.md), even if Egg Man has added hundreds of new nodes, the automatically-generated branch titled `<skg< unintegrated subscribed content from Egg Man's document "eggs" >>` does not have to clutter your view of `mysticism`.

You can of course unsubscribe from Egg Man's changes to that branch if you want, or even unsubscribe from Egg Man's writings entirely.

# Conlcusion
You and Egg Man can thus focus on exactly what you care about. Your "changes" to Egg Man's data don't have to affect Egg Man's experience at all. However, if you make public the fact that you integrated Egg Man's note into yours, and Egg Man has chosen to see such information, then an indicator in Egg Man's view of `eggs` will show that his `mystical` branch is part of a document owned by you. If Egg Man is curious, he can follow that indicator to discover your `mysticism` document.
