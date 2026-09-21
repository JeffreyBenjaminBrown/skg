See also the [API](../api-and-formats.md), the
[technical data model](../docs/data-model_technical.org), and the
[glossary](../glossary.md).

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

The user views and edits the graph as Org text in Emacs or Neovim.
When a client asks for a view, Rust sends a whole buffer of text.
When the user saves, the client sends the whole buffer back. Saving
updates the authoritative files, publishes a validated graph generation,
queues the Tantivy update, and sends updated views to the client.

View nodes are not all graph nodes. Active and inactive vognodes represent
current graph nodes; phantoms represent missing or historical occurrences;
folders and qualifiers carry relationships, aliases, IDs, or properties.
New editable nodes may lack an ID until save preparation assigns one.
Editability, view-node kind, and relationship context determine how text is
interpreted on save; not every displayed headline is an instruction to write
a graph node. See [the view-node types](../server/types/viewnode.rs) and
[the metadata parser](../server/serve/parse_metadata_sexp.rs) for the current
representation, and the API document for its wire format.
