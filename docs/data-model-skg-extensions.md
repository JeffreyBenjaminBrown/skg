# Data Model Skg Extensions

Skg grafts a novel sharing model onto a familiar data model older than Rome.
## The familiar part: Trees with textlinks
This is [the data model that almost all serious knowledge mapping software seems to use](data-model-trees-with-links.md).

(If 'textlink' sounds weird, just think 'hyperlink'. If you use the internet, you are used to using these.)

### What the `contains` relation means, and how it differs from textlinks
Each document is a curated view of data defined by the `contains` relation. Any document that includes the node N also includes every node that descends from N via `contains`.

As is evident from the [schema](../schema.tql), there are a number of ways nodes can be connected: `contains`, `textlinks_to`, and a few more. Containment and textlinks differ in two ways:

- TextLinks are defined by the text in a node, whereas containment is defined by nodes' spatial relationships to each other in a tree (the document containing them).
- Following a textlink requires the user to jump to a different context, whereas following a containment relationship need only require moving your eyes. If A contains B, looking at A means looking at a document that already contains B underneath A.
## Each node of a document is a separate file.
This is unusual -- in most applications, the entire tree of text would be a single document. The one-file model has the advantage that a document can be easily read without special software. But the Skg model makes it possible to share granularly (see [the sharing model](sharing-model.md), and [easy to understand changes](diffs-are-clearer-in-skg.md).
## The overrides_view_of relationship
A file F can override the view of file O. If it does, then by default when the user opens a link to O, they will see F instead. (The override is not silent -- it will be visibly announced, and the user can still visit the overridden file if they like.)

Overriding is useful for both sharing and privacy, as is explained below.

Overriding is represented in the [schema](../schema.tql) with the `overrides_view_of` relationship.
## Sharing
When you "edit" a note N that you don't own, what actually happens is you gain a new note N', which begins with all of the content from N plus a (deletable) subscription to N and a (deletable) overrides relationship to N.

Because nodes in Skg are represented as flat files, N' duplicates very little data. For instance, if N consisted of two big branches, then the file representing N would only contain some text and two nodes. If you changed their order, you would have to duplicate N (with its text and its two nodes), but you would not have to duplicate anything inside either branch.

The overrides relationship causes Skg to show you N' when you follow links to N.

When new content appears in N, it will appear in your N' as "unintegrated subscribed content" (which you can proceed to integrate if you like). If you delete the subscription, this stops being true. You can also subscribe N' to other nodes apart from N.

If the original N contains C, and you delete C from your N', then N' gains a "hides_from_its_subscriptions" relationship to C, which prevents C from being shown as "unintegrated subscribed content" in N'.
## Privacy relies on overriding.
The previous section describes how overriding is used to integrate foreign content into one's data. Overriding is also the mechanism by which private data is added to public data.

Suppose you have a puBlic note B that contains branches X and Y. Suppose you would like to include a third branch Z, but you want Z to be private.

Skg's solution is to override B with a new priVate note V, which contains X, Y and Z. When you view B in "private mode", your private note V will override your view of B. But when you are in "public mode", following a link to B will bring you to B.
