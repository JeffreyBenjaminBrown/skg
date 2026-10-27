See also the [API](../docs/api-and-formats.org), the
[technical data model](../docs/data-model_technical.org), and the
[glossary](../docs/glossary.org).

Note that the above documents, this document, and any other documentation might be obsolete. The definitive source of truth is the code, not the docs.

# What this program does.

Skg is for manipulating a knowledge graph. The bulk of the logic is performed
by the Rust server in `server/`. The Emacs Lisp client lives in `elisp/`, and
the Neovim Lua client in `nvim/`. Both use the same server protocol. The user's
canonical data is stored in `.skg` files, which are valid YAML (but not every
YAML file is an Skg file). The graph and search index can be rebuilt from them.

At startup the server reads every configured `.skg` file, validates an
immutable in-Rust graph, and rebuilds Tantivy from the same nodes. The graph is
the runtime graph store; Tantivy is only the derived full-text index.

`shared/` holds data that the server and both clients read: the herald
styles (`herald-styles.json`) and the relations schema (`relations.json`).
The clients read them at load time, relative to their own source; Rust
keeps its own types and checks them against these files in conformance
tests. `tests/shared/` holds data only tests read, such as rendering
cases both clients' tests share.

The user views and edits the graph as Org text in Emacs or Neovim.
When a client asks for a view, Rust sends a whole buffer of text.
When the user saves, the client sends the whole buffer back. Saving
updates the authoritative files, publishes a validated graph generation,
queues the Tantivy update, and sends updated views to the client.

Every buffer the server sends is drawn by one operation, the refresh
(`refresh_viewforest` in `server/refresh/`): it takes a parsed,
validated buffer and the graph, decides from the two as they are now
what the buffer should say, corrects it, and reports its corrections.
A fresh view, every rerender after a save or a request, the search
enrichment and the diff view's buffers are all refreshes. Its model is
in [the technical data model](../docs/data-model_technical.org),
"The refresh: how a view follows the graph".

Viewnodes are not all graphnodes:

```
viewnode = vognode | folder | propertyMember | changeViewnode
         | bufferRoot | tombstone
vognode  = occurrence | ghost | unknown | restricted
```

A vognode stands for a graphnode's ID, which need not name a current
graphnode. An occurrence stands for a node the graph has; a ghost,
only in the diff view, for a node HEAD has and the worktree lacks; an
unknown, for a reference that resolves to nothing; a Restricted
vognode, for a node the skgrepo restriction keeps out that a visible
record names. Folders carry
relationships or a node's aliases, IDs and flags, as their members;
property members are those aliases, IDs and flags; change viewnodes
describe a change the diff view shows.
The parse gives every new headline an ID and a skgrepo before
validation. Editability, viewnode kind, and whether a child claims
membership of its viewparent's list determine how text is interpreted
on save; not every displayed headline is an instruction to write a
graphnode. See [the viewnode types](../server/types/viewnode.rs) and
[the metadata parser](../server/serve/parse_metadata_sexp.rs) for the current
representation, and the API document for its wire format.
