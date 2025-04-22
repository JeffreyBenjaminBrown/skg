# How text content is modeled in skg

Skg data is a collection of "nodes". Like a headline in Emacs org-mode, each node has a title, some non-title text content, and a list of other nodes it contains.

When a user requests a view of a node, they get not just that node's content, but the content of each thing it contains, and the content of each of those things, etc. The result is a tree. And if the node they requested a view of is contained in some other node, they get that node, too, and all of its content.

The result is a tree of text, and looks very much like an Emacs org-mode file. But the way skg represents content on disk is different from how org-mode does it. In org-mode, the entire tree of text is a single document, whereas in skg, each node is a separate document.

The Emacs model has the advantage of making single documents be self-contained and easy to read without special software. But the skg model makes it easy to share (see [[the sharing model](sharing-model.md)], and easy to compute much more readable diffs.

For an example of how the skg format simplifies diffs, suppose file F contains two branches A and B, and both are big, and you swapped their order. In Emacs, the resulting diff is very large, and it takes some effort to determine that all you did was change the order of the two branches. In skg, by contrast, the diff could not be simpler, because F only contains two nodes. Their order changed, but their contents did not. (If their contents had changed, that would have happened in the two files representing the two branches, rather than in F.)
