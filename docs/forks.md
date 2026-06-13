# Forks

A **fork** turns a *foreign* node — one in a source you do **not** own,
hence read-only — into an editable **clone** in a source you *do* own.
The clone keeps a live link to the original, so the original's later
additions still reach you, but your edits never touch the original.

See also [the sharing model](sharing-model.md) for the underlying
relations (`subscribes`, `overrides_view_of`,
`hides_from_its_subscriptions`) and `glossary.md` ("fork", "clone",
"subscribee as such", "override substitution").

## The gesture

You do not run a special command. You just **edit the foreign node**:

1. Make the foreign node *definitive* (editable) with the definitive-view
   request, `C-c s d`. (A foreign node is read-only until you do this.)
2. Edit it however you like — its title, its body, add / remove / reorder
   its children, at any depth.
3. Save.

Any real change to a foreign node is read as a request to fork it. (An
unchanged definitive foreign node forks nothing; saving it is a no-op.)

The save does not commit blindly. The server first returns a
**fork-confirmation buffer** and asks you to approve. The buffer is two
levels per fork: each top headline is the **clone-to-be** (your edited
title), and under it is the **original** it forks (its real id and
foreign source, marked `pO` — "its parent overrides it"). One headline
cannot honestly stand for both, since you may have re-titled the clone
and the two live in different sources.

The buffer is read-only **except each clone's source**, the one field you
may change: put point on a clone-to-be headline and rotate its source
with `C-c s s` (it offers the sources you own). This is how you place — or
redirect — a clone whose source could not be inferred (below).

- **Approve** (`C-c C-c` in that buffer) commits the forks into the
  sources shown and completes the save.
- **Decline** (`C-c C-k`, or just leave the buffer) aborts the *whole*
  save — nothing is written. The confirmation buffer stays open, so you
  can still search it for the IDs involved.

## What the clone is

When you fork a foreign node `N`, the clone `C` is a new node in a source
you own. `C`:

- copies `N`'s title, body, and **child list** — the child IDs only, a
  *shallow* copy, not `N`'s whole subtree (this is what makes forking
  cheap);
- **subscribes to** `N`, so `N`'s later additions surface to you as
  unintegrated content (below);
- **overrides** `N`, so *override substitution* draws `C` wherever `N`
  would otherwise appear as content — you see and edit `C` in `N`'s
  place from then on, without `N`'s containers being rewritten to point
  at `C`.

`C`'s source is resolved in priority order: the source you chose in the
confirmation buffer (`C-c s s`), else the source **inferred** from `N`'s
nearest owned ancestor in the view, else a **default** of your first
owned source. So a foreign node with no owned ancestor still forks — it
defaults to an owned source you can then rotate. The fork only fails for
lack of a source if you own **no** source at all. (The inference takes
`N`'s *immediate* owned container, never a distant owned node reached by
skipping a foreign ancestor.)

`N` itself is left completely untouched on disk.

## Unintegrated content: the subscribeeCol

Because `C` subscribes to `N`, a generated **subscribeeCol** appears
under `C` listing `N` as a *subscribee-as-such*. Its content is exactly
what you have **not** yet integrated: a subscribee-as-such shows only the
children the subscriber (`C`) neither hides nor already contains. A fresh
clone has `C.contains == N.contains`, so this starts **empty** — every
child you copied is excluded as already-contained. As the original gains
new children you have not pulled in, they appear here.

### An example

Suppose this document, `mysticism`, is yours, and `eggs` belongs to your
friend Egg Man (a source you do not own):

```
* eggs                              (Egg Man's, read-only to you)
** health
** recipes
** mystical
*** Which came first, the chicken or the egg?
```

You make `mystical` definitive, edit it (say you rename it to
`eggy mysticism`), and save. The confirmation buffer lists `mystical`;
you approve. Now your graph has a clone of `mystical` that you can file
under `mysticism`. Wherever `mystical` would be drawn, you see your clone
instead, marked as standing in for the original:

```
* mysticism                         (yours)
** common objects with mystical associations
*** eggy mysticism      «Oh» (drawn in place of Egg Man's "mystical")
**** Which came first, the chicken or the egg?
*** subscribeeCol
**** mystical           (the subscribee-as-such; empty so far)
```

When Egg Man later extends `mystical`:

```
* eggs
** mystical
*** Which came first, the chicken or the egg?
*** Egg limpia (curanderismo)       -- NEW
*** The philosophical egg           -- NEW
```

his two new children — which you have not integrated — show up under the
subscribee-as-such:

```
*** eggy mysticism      «Oh»
**** Which came first, the chicken or the egg?
*** subscribeeCol
**** mystical
***** Egg limpia (curanderismo)     (unintegrated)
***** The philosophical egg         (unintegrated)
```

Since [branches can be folded](tree-folding-is-powerful.md), even
hundreds of new originals' children need not clutter your view. To
integrate one, move it into your clone's own content; it then drops out
of the subscribee-as-such (it is now contained). You can also hide
individual children, or unsubscribe entirely.

## Once forked: monogamy

A node may have at most one clone *you* own. If you try to fork something
you have already forked, the save is rejected with a message naming your
existing clone — edit that clone instead. (The underlying data may
override a node many times; only your own override is limited to one.)

## Conclusion

You and Egg Man focus on exactly what each of you cares about. Your
"changes" to Egg Man's data never affect Egg Man's experience. If you
make public that you integrated his note, and he has chosen to see such
information, an indicator in his view of `eggs` shows that his `mystical`
branch lives on in a document you own, which he can follow to discover
your `mysticism`.
