See also the [API](../api-and-formats.md), the
[technical data model](../docs/data-model_technical.org), and the
[glossary](../glossary.md).

Note that the above documents, this document, and any other documentation might be obsolete. The definitive source of truth is the code, not the docs.

# What this program does.

Skg is for manipulating a knowledge graph. The bulk of the logic is performed by the server, written in Rust, found in server/. The client is written in Emacs, found in elisp/. The user's data is stored on disk, in a collection of '.skg' files, which are all valid YAML (but not vice-versa). When the program stops, those files are the only record of the user's data.

At startup the server reads every configured `.skg` file, validates an
immutable in-Rust graph, and rebuilds Tantivy from the same nodes. The graph is
the runtime graph store; Tantivy is only the derived full-text index.

The idea is for the user to view and edit the graph using org-mode in Emacs.
When Emacs asks for a "view" of the data, Rust sends a whole buffer of text to
Emacs. When the user saves the data, Emacs sends the whole buffer back. Saving
updates the authoritative files, publishes a validated graph generation,
queues the Tantivy update, and sends an updated buffer to Emacs.

Most headlines are 'content' -- that is, they correspond to something in the graph. If they are new, they won't have an ID, but once they are saved, they will. The ID is one kind of 'metadata' that can precede a headline. There are some others, as detailed in the `OrgNodeMetadata`. Of these, the most interesting is 'node_type'.

The default type of an org node is 'content'. Content nodes cause Rust to change the contents of the graph upon saving. But there can be other kinds. So far the only other one implemented is an 'alias' node. Its org-children are aliases that become associated with the 'alias' node's parent. Thus an 'alias' node does not dictate content; rather, it influences the data within the 'content' node that is its org-parent.
