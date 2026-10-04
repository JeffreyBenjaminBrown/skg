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

Viewnodes are not all graph nodes:

```
viewnode = vognode | propertyFolder | property | partnerFolder
         | bufferRoot | deadViewnode
vognode  = active | inactive | phantom
```

A vognode represents a graphnode, which need not exist. Active and inactive
vognodes represent current graph nodes; phantoms represent missing or
historical occurrences. Property folders and partner folders carry aliases,
IDs, flags, or relationships.
New editable nodes may lack an ID until save preparation assigns one.
Editability, view-node kind, and relationship context determine how text is
interpreted on save; not every displayed headline is an instruction to write
a graph node. See [the view-node types](../server/types/viewnode.rs) and
[the metadata parser](../server/serve/parse_metadata_sexp.rs) for the current
representation, and the API document for its wire format.
