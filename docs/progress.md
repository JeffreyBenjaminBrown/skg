# What's done, and what's left to do
For that plan and more in greater detail (plus architectural observations, conceptual problems, and more), see [[my org-roam notes on Skg](https://github.com/JeffreyBenjaminBrown/public_notes_with_github-navigable_links/blob/main/shareable_knowledge_gardens.org)].
## What's done
This code so far includes a Rust server and an Emacs client. The server can:

- read and write to files in a format called .skg (it's just .yaml, but not all .yaml is .skg)
- build, modify and consult a model of the data in TypeDB
- build, by consulting TypeDB, a document to send Emacs

The client can:

- Receive that document.
- Let the user edit it.
## What's partially done
### Save data
    The client can request saved data and manipulate it.
    The client can't send anything back to Rust. And if it could, Rust wouldn't know what to do with it. (It should save the data to disk, update TypeDB and Tantivy, and silently replace the Emacs document with a new one with IDs where the user had created new headings without IDs.) This is sketched in a little more detail in [[TODO.org](../devel-handy/TODO.org)].
### Inhomogeneous, multidimensional trees
The database is implemented in TypeDB. which should make the required queries easy. Otherwise this is completely unimplemented.

Multidimensionality would mean incorporating into the tree view of some data relationships beyond the "x contains y in document d" for one fixed d. Instead, the document/view could vary throughout the tree, and some nodes could include branches corresponding to foreign data that links into them. The branches shown under a node do not have to all be of the same sort, because intermediating branches can classify them.

A refactor in which the containment relationship were tripartite, "[node] contains [node] from point of view of [node]", would make this more natural.
### The schema ought to use a tripartite content relationship, not a binary one
The content relationship ought to be not "[host] contains [branch]", but "[host] contains [branch] when seen from [perspective]", where each of the three roles is filled by the ID of a single node. .skg files that used to be represented with a single host ID and a number of branch IDs should now be a host id, a perspective id, and the list of branches.

More detail here:
https://github.com/JeffreyBenjaminBrown/public_notes_with_github-navigable_links/blob/main/node_contains_branch_in_view.org
### Sharing
The user experience for sharing has been documented, and the [[schema](../schema.tql)] includes the necessary relations -- subscription ("subscribes"), hiding from a subscription view ("hides_from_its_subscriptions"). Those relationships can be read into TypeDB.

There is not yet code to modify them or query using them.
