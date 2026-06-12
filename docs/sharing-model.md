# How sharing works in skg

Each author or source you want access to should be downloaded to some local folder. Some of these notes will, presumably, be ones you wrote; others can be ones others wrote.

You can navigate and edit them as if you owned them all. When you "modify" someone else's original note, you actually create a clone of it, in your data rather than theirs. The clone is implemented as an ordinary note just like the original, with the same text and branches, and with a subscription to the original. You can change the text of the clone, rearrange its branches, discard them, scatter them across your other files.

All of the original content is considered "integrated" into the clone. Branches you then delete from the clone or move elsewhere become branches which the clone "hides_from_its_subscriptions". These hiding relationships are recorded explicitly in the filesystem upon saving, but there is also an implicit "hides_from_its_subscriptions" relationship from the clone to all of its normal content. (That's because any branch that shows up as ordinary integrated content does not need to also show up in the subscriptions.)

If, later, the other author adds content to their note, it will be visible from the clone, thanks to its subscription to the original, as "unintegrated subscribed content". You can if you wish proceed to incorporate that content into yours, too.

Because nodes in Skg are represented as flat files, this way of forking foreign data duplicates very little of it. For instance, if the original had two branches when the clone was made, the clone would only need to `contain` two IDs. None of the descendents of those IDs need to be mentioned, unless and until one of them is also modified.

## Editing subscribed content in a view

The view-children of a subscribee-as-such can be edited (in limited ways) to control what the subscriber hides from the subscribee.

Let's call the subscriber R and the subscribee E.

If N is graph-content of E, and you delete N as a view-child of E, you have indicated to Skg that you would like to hide N from R's view of E. Skg obliges by creating a hides relationship from R to N. This does *not* change what E graph-contains; it just changes how R views E. (Remember, it's likely that E is not even something you own.)

If you later put that same child back among the subscribed node's children in the subscription view and save, the hide is removed.

This rule only applies to the subscribee's children. Edits to deeper descendants have the usual effect, subject to ownership.

If E does not contain N, and you make N a view-child of E, N will persist in your view but have no effect on E or R.

Title and body edits to E should be made from some view that does not present E as a subscriber. (C-c g RET from E will open such a view.) Skg rejects title/body edits to subscribees-as-such, regardless of who owns the subscribee.

The generated "hidden from this subscription" and "hidden from all subscriptions" collections are read-only displays. They show what R currently hides, but editing those collections is not how you hide or unhide subscribed content. To hide something, remove it from E's immediate visible graph-content in the subscription view. To unhide it, put it back there.

## Overriding: seeing your version where the original would appear

When you clone a foreign node, the clone `overrides_view_of` the original. The clone is the *overrider*, the original the *overridden*. Overriding changes what gets drawn: wherever the overridden node would appear as recursive content of some other node, Skg draws the overrider in its place. This is *override substitution*. So once you have cloned `mystical`, every view that would have shown Egg Man's `mystical` as content shows your `eggs / mystical` instead.

Substitution is careful in four ways:

- **It round-trips to the original on save.** The drawn overrider carries a hidden marker (`overridesHere`, herald "Oh") naming the node it stands for. When you save the container, Skg records the *original*'s ID in the container's content list, not the overrider's, so drawing your version never silently rewrites someone's contains list.
- **It is one level deep.** A node that is itself drawn raw — a member of a collection, a view root, or an *overridden-as-such* (an overridden node shown deliberately, e.g. under an `overriddenCol`) — shows its own children raw. You are looking at the original there, so its children are the original's. Substitution resumes one level further down.
- **Foreign override edges never substitute.** If someone else's node overrides one of yours, that is a display-only fact (it can earn an herald), but it does not change what you see. Only your own (`user_owns_it`) overrides substitute.
- **Visibility gates substitution.** An overrider whose source is inactive under the current source-set cannot be drawn, so it does not substitute; the original is drawn instead. Switching source-sets can therefore change which node a view draws.

Two invariants keep owned overrides coherent, checked both when you save and when the databases are rebuilt: **monogamy** (a node has at most one user-owned overrider) and **acyclicity** (no user-owned override chains, where your overrider is itself overridden by another node you own). Foreign and imported data can violate these; the resolver that walks override edges is cycle-guarded and total so bad data cannot hang a render.

When you ask for a brand-new single-root view of a node that is overridden (by anyone), Skg does not choose for you: it opens an *override-choice menu* — the requested node as root, each visible overrider beneath what it overrides — and explains so in the minibuffer. Pick the overrider you want, or bypass the menu (`skg-goto-bypassOverride`) to open the original node itself, drawn raw.

## The read-only collections: who subscribes to, overrides, or hides this node

A view of a node N can also show, as generated org-collections, the nodes that point *at* N:

- a `subscriberCol`: the nodes that subscribe to N (the data behind "who forked this");
- an `overriderCol`: the nodes that override N;
- a `hiderCol`: the nodes that hide N from their subscriptions.

These are *read-only sets* (see `glossary.md`). Their membership is generated from graph facts, not edited directly: you may reorder the members and the order is respected within that view, but inserting a node relabels it independent (it does not create the relationship), and deleting a member only makes it respawn on save. To actually add or remove one of these relationships, edit the node at the *other* end — the subscriber, overrider, or hider — not N's collection. When a save repairs such a collection (restoring a deleted member, demoting an inserted non-member), it says so in a warning, but only for the view you saved.
