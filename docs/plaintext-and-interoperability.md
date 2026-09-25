# Plaintext And Interoperability

## Does it use plaintext? Is it interoperable with Emacs org-mode?

Yes and yes.

I hope to create an isomorphism from Skg into [Dragon](https://github.com/CategoricalData/hydra/tree/main), whereupon it can easily be converted back and forth into anything else for which such an isomorphism has been written. Anything expressible in Skg and not the other system, or vice-versa, can (but needn't be) preserved as comments -- that is part of Hydra's magic.

Skg uses Org text to manipulate data, but stores it as YAML in `.skg` files. Skg files are valid YAML, but most YAML is not valid Skg. See the [file-format reference](../api-and-formats.md) and [on-disk node types](../server/types/nodes/fs.rs).

Skg already imports Org-roam files and exports selected content to plain Org; see the [command reference](COMMANDS.org). General Org and Markdown import are not implemented. Import creates a node for each file and heading, retaining existing IDs and assigning IDs where needed. The importer replaces existing `.skg` files in its destination, so use a fresh staging directory first.

Export is for readable documents, not lossless backup or synchronization with Org-roam. It omits identities and relationships needed to reconstruct the graph. See [returning imported data to Org](undoing-import.org) for the limitations.

## The `contains` relationship and nesting org-mode headlines

The Skg containment relationship is identical to the org-mode relationship from a headline to one of its subheadlines. (I might rename it "shows" or "presents" or something, to make clear that it is the relationship that defines a "document" or a "view".)

What's different is their representation on disk. Skg represents each node of a document with a separate file in order to  enable sharing. I can't link to one of your nodes unless every node has an ID. And creating a fork of one of your nodes would have a prohibitive memory cost if the node has many descendents.
