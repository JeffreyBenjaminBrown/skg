# Forks

A **fork** turns a *foreign* node — one in a source you do **not** own,
hence read-only — into an editable **clone** in a source you *do* own.
The clone keeps a live link to the original, so the original's later
additions still reach you, but your edits never touch the original.

See also [the sharing model](sharing-model.md) for the underlying
relations (`subscribes`, `overrides_view_of`,
`hides_from_its_subscriptions`) and `glossary.md` ("fork", "clone",
"subscribee as such", "override substitution").

## The implicit gesture: editing a foreign node

You do not run a special command. You just **edit the foreign node**:

1. Make the foreign node *definitive* (editable) with the definitive-view
   request, `C-c s d`. (A foreign node is read-only until you do this.)
2. Edit it however you like — its title, its body, add / remove / reorder
   its children, at any depth.
3. Save.

Any real change to a foreign node is read as a request to fork it. (An
unchanged definitive foreign node forks nothing; saving it is a no-op.)

The save does not commit blindly. The server first returns a
**fork-confirmation buffer** and asks you to approve. The buffer opens
with a foldable instructions headline, then two levels per fork: each
top headline is the **clone-to-be** (your edited title), and under it
is the **original** it forks (its real id and foreign source, marked
`pO` — "its parent overrides it"). One headline cannot honestly stand
for both, since you may have re-titled the clone and the two live in
different sources.

Each clone needs a source you own. If your edit already **specified**
one — the fork was forced by new children whose metadata explicitly
names one owned source (see "Adding new content", below) — the
clone-to-be shows that source, settled, and you are not asked again.
Otherwise its source starts as the placeholder `PICK-A-SOURCE`, with a
suggested source noted just above it (the inferred source for a
foreign fork, or your config-first owned source otherwise), and the
client **prompts you to choose** for each such clone — the suggestion
is the prompt's default; `S-left`/`S-right` cycle the sources you own.
You can also set a source by hand: point on the clone-to-be headline,
then `C-c s s`. Approving while any placeholder remains is refused,
loudly; a deliberate choice keeps a fork from silently landing in a
default source.

- **Approve** (`C-c C-c` in that buffer, or answering the prompt)
  commits the forks into the sources you chose and completes the save.
- **Decline** (`C-c C-k`, or just leave the buffer) aborts the *whole*
  save — nothing is written. The confirmation buffer stays open, so you
  can still search it for the IDs involved.

## Adding new content under a foreign node

Appending a **new** headline under a foreign node is also just an
edit: it changes the foreign node's child list, so it forks it. The
new node itself needs an owned source too:

- A **bare** new headline (no metadata) rides the fork: it lands in
  the same source as the clone — whatever the confirmation flow
  settles on — just as a new node under an owned parent lands in that
  parent's source.
- A new headline whose metadata **names an owned source** (e.g.
  `(skg (node (source my-notes)))`) both places the new node there and
  *specifies* the clone's source (when all such new children agree),
  so the confirmation flow asks nothing beyond the approve.
- A new headline that explicitly names a **foreign** source is an
  error, as ever: foreign sources cannot be written.

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
  at `C`;
- **hides** any child of `N` that your forking edit *deleted*, so what
  you dismissed does not come straight back as unintegrated subscribed
  content (below). Later deletions from `C` hide likewise — see
  "Editing the clone", below.

`C`'s source is the one the confirmation flow settles on: specified by
your edit, or picked at the prompt (see above). When nothing specified
it, the *suggestion* is the source **inferred** from `N`'s nearest
owned ancestor in the view, or, with no owned ancestor, your
config-first owned source. So a foreign node with no owned ancestor
still forks — it just suggests a default you confirm or change. The
fork can only fail for lack of a source if you own **no** source at
all. (The inference takes `N`'s *immediate* owned container, never a
distant owned node reached by skipping a foreign ancestor.)

`N` itself is left completely untouched on disk.

## The explicit gesture: forking a node you own

Editing a foreign node forks it implicitly. To fork a node you **already
own** — for example a public clone `C` you made above, which you now want
to layer a *private* version `D` on top of — there is an explicit
command, **`skg-fork-node`** (`C-c m f`, alias `skg-fork`).

Put point on the node's headline and run it. It refuses if the buffer has
unsaved changes ("Save the buffer before forking.") — so the clone is
built from the node's saved snapshot, exactly what you see. Otherwise it
marks the node and auto-saves, and you get the **same fork-confirmation
buffer** as the implicit fork: pick the clone's source at the prompt —
e.g. your *private* source — (or set it with `C-c s s`), then approve
with `C-c C-c`, or decline with `C-c C-k` (which here also strips the
fork request so the next save does not re-fork).

Two differences from the implicit fork:

- The original is **yours**, so it is *not* dropped — your edits to it
  still save normally; the gesture only *adds* the clone that overrides
  it.
- The **suggested** source is your **config-first** owned source (the
  first `[[sources]]` you own in `skgconfig.toml`), since there is no
  foreign ancestor to infer from.

The result is an **override chain**: `D overrides C overrides N`. Chains
of any length are legal (only a *cycle* is forbidden — see
[the sharing model](sharing-model.md)). When all sources are active,
viewing `N`'s container draws the chain's end `D`; restrict the
source-set to hide `D`'s source and the same view draws the middle `C`
instead — either way `N` stays in the container's contents. If you point
`skg-fork-node` at a node that is *itself* a drawn substitute (one
carrying an `overridesHere` marker), it forks the node on screen — the
overrider — deepening the chain, never re-forking the node it stands for.

If the node already has a clone you own, `skg-fork-node` is rejected by
the same monogamy rule (below); deepen the chain by forking the
*overrider*, not the already-overridden node.

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

## Editing the clone: deletions become hides

Suppose the clone `C` and the original `N` both contain a child `X`,
and you delete `X` from `C` (or move it elsewhere). Without more, `X`
would immediately resurface under `C` as unintegrated subscribed
content — you removed it, `N` still contains it. So the save records a
hide: `C` now `hides_from_its_subscriptions` `X`, and the subscription
stays quiet about it. Putting `X` back among `C`'s contents removes
the hide again. (This is the same rule the forking edit itself
follows: children of `N` you deleted *while* forking are born hidden.
And it holds for any owned subscriber, not just clones — a fork is
just subscribe + override.)

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
