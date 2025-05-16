# what the `contains` relation means, and how it differs from hyperlinks

As is evident from the [[schema](../schema.tql)], there are a number of ways nodes can be connected: `contains`, `hyperlinks_to`, and a few more (involved in sharing) that are not relevant here. Containment and hyperlinks differ in two ways:

- Hyperlinks are defined by the text in a node, whereas containment is defined by nodes' spatial relationship to each other in a document.
- Following a hyperlink requires the user to jump to a different context, whereas following a containment relationship need only require moving your eyes. If A contains B, looking at A means looking at a document that already contains B underneath A.

The `contains` relation thus defines the `views`. If A contains B and B contains C, then any view of either A or B contains C.

# how trees are modeled in skg

When a user requests a view of a node, they get not just that node's content, but the content of each thing it contains, and the content of each of those things, etc. The result is a tree. And if the node they requested a view of is contained in some other node, they get that node, too, and all of its content.

The result is a tree of text, very much like an Emacs org-mode file.

## Each branch of a tree is a separate file.

The way Skg represents content on disk is unusual. In Emacs and most (all?) other applications, the entire tree of text would be a single document, whereas in Skg, each branch is a separate document.

The Emacs model has the advantage of making single documents be self-contained and easy to read without special software. But the Skg model makes it easy to share granularly (see [[the sharing model](./docs/sharing-model.md)], and [[easy to understand changes](./docs/diffs-are-clearer-in-skg.md)].

# What ensures a document is a tree (i.e. has no cycles)? What prevents cycles or infinite trees?

Nothing. The app can't control what data the user creates. But it can warn them, and it can provide finite views of infinite data (see the `repeated` field of the `OrgNode` type for how).

If the data is not a tree, ambiguities may arise regarding the context in which to show a node. For instance, suppose the user requests to see A, but A is contained in both B and C. In this case the application will simply show the options and let the user choose the context to view A from. It will also display a warning, suggesting the user replace at least one of the two competing `contains` relationships with a hyperlink.
