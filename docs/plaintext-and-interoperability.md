# Does it use plaintext? Is it interoperable with Emacs org-mode?

Yes and yes.

I hope to create an isomorphism from Skg into [Dragon](https://github.com/CategoricalData/hydra/tree/main), whereupon it can easily be converted back and forth into anything else for which such an isomorphism has been written. Anything expressible in Skg and not the other system, or vice-versa, can (but needn't be) preserved as comments -- that is part of Hydra's magic.

Skg data is stored as plain, human-readable text, but not as org-mode text. Skg uses org-mode to manipulate the data, but the data is stored on disk as a dialect of YAML. (Skg files are valid YAML, but most YAML is not valid Skg.) Any of the `.skg` files in the `tests/` folder should give the idea for the syntax, or see its complete definition in the Node type in [../rust/types.rs](../rust/types.rs).

It will be possible to translate org-roam data into Skg, and vice-versa, but they aren't the same format. In Skg each node from a tree is its own file. This enables the sharing model ("Manipulate others' data as if it were your own" in the README), and

Even with that naive translation, though, the user experience need hardly be affected, because the resulting plethora of PROPERTIES buckets can be hidden from view.
provides the side-benefit of making diffs much easier to understand (described in `docs/diffs-are-clearer-in-skg.md`).

In Skg every node in a tree has an ID, whereas in org-mode, a node generally only has an ID if it is a top-level file node or if it is linked to from somewhere. Therefore, naively translating an org-file into skg and then back into org-mode will make it bigger. But don't worry! It will not be hard to strip extraneous IDs from the org data.

# The `contains` relationship and nesting org-mode headlines

The Skg containment relationship is identical to the org-mode relationship from a headline to one of its subheadlines. (I might rename it "shows" or "presents" or something, to make clear that it is the relationship that defines a "document" or a "view".)

What's different is their representation on disk. Skg represents each node of a document with a separate file in order to  enable sharing. I can't link to one of your nodes unless every node has an ID. And creating a lens onto one of your nodes would have a prohibitive memory cost if the node has many descendents.
