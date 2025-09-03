See also the [API](../api.md), the [schema](../schema.tql), and the [glossary](../glossary.md).

Note that the above documents, this document, and any other documentation might be obsolete. The definitive source of truth is the code, not the docs.

# What this program does.

Skg is for manipulating a knowledge graph. The bulk of the logic is performed by the server, written in Rust, found in rust/. The client is written in Emacs, found in elisp/. The user's data is stored on disk, in a collection of '.skg' files, which are all valid YAML (but not vice-versa). When the program stops, those files are the only record of the user's data.

While the server is running, it keeps a lot of information about the graph in TypeDB, and a little bit indexed in Tantivy. Some information about the graph might be in neither; if so, it is fetched from disk before being presented to the user.

The idea is for the user to view and edit the graph using org-mode in Emacs. When Emacs asks for a "view" of the data, Rust sends a whole buffer of text to Emacs. When the user saves the data, Emacs sends the whole buffer back. Saving causes Rust to update TypeDB, Tantivy and the files on disk, and to then send an updated buffer to Emacs.

Most headlines are 'content' -- that is, they correspond to something in the graph. If they are new, they won't have an ID, but once they are saved, they will. The ID is one kind of 'metadata' that can precede a headline. There are some others, as detailed in the `OrgNodeMetadata`. Of these, the most interesting is 'node_type'.

The default type of an org node is 'content'. Content nodes cause Rust to change the contents of the graph upon saving. But there can be other kinds. So far the only other one implemented is an 'alias' node. Its org-children are aliases that become associated with the 'alias' node's parent. Thus an 'alias' node does not directly dictate content; rather, it influences the data within the 'content' node that is its org-parent.
