# Does it use plaintext? Is it interoperable with Emacs org-mode?

Yes and yes, but keep reading.

The data is stored as plain, human-readable text, but as org-mode text. Skg uses org-mode to manipulate the data, but the data is stored on disk as a dialect of YAML. (Skg files are valid YAML but most YAML files are not valid Skg files.) Any of the `.skg` files in the `tests/` folder should give the idea.

It will be possible to translate org-roam data into Skg, and vice-versa, but they aren't the same format. In Skg each node from a tree is its own file. This enables the sharing model ("Manipulate others' data as if it were your own" in the README), and

Even with that naive translation, though, the user experience need hardly be affected, because the resulting plethora of PROPERTIES buckets can be hidden from view.
provides the side-benefit of making diffs much easier to understand (described in `docs/diffs-are-clearer-in-skg.md`).

In Skg every node in a tree has an ID, whereas in org-mode, a node generally only has an ID if it is a top-level file node or if it is linked to from somewhere. Therefore, naively translating an org-file into skg and then back into org-mode will make it bigger. But don't worry! It will not be hard to strip extraneous IDs from the org data.
