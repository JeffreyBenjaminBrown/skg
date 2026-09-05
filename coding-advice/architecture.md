See also the [API](../api-and-formats.md), the [schema](../schema.tql), and the [glossary](../glossary.md).

Note that the above documents, this document, and any other documentation might be obsolete. The definitive source of truth is the code, not the docs.

# What this program does.

Skg is for manipulating a knowledge graph. The bulk of the logic is performed
by the Rust server in `server/`.  The interactive clients are Emacs Lisp in
`elisp/` and Lua for Neovim in `nvim/`.  One server process admits exactly one
interactive client at a time; a later connection replaces the same retained
session after a buffer census rather than creating another user.

The canonical graph data is stored on disk in `.skg` files, all valid YAML
(but not vice versa).  Private maintenance archives separately preserve
interrupted editor work and evidence; they are recovery records, not another
selected graph.

While the server is running, it keeps a lot of information about the graph in TypeDB, and a little bit indexed in Tantivy. Some information about the graph might be in neither; if so, it is fetched from disk before being presented to the user.

The user views and edits the graph as Org text in Emacs or an Org-compatible
Neovim buffer.  When the client asks for a view, Rust sends a whole buffer of
text.  On save the client sends the whole buffer back.  Rust prepares the
complete filesystem mutation, fences it against the selected byte manifest,
then coherently updates the `.skg` files, in-Rust graph, TypeDB, and Tantivy
before returning updated view text.

# Runtime and maintenance ownership

Filesystem activity is not permission to mutate the selected graph.  The
process-owned observer builds an immutable candidate first; selecting it
requires the durable maintenance protocol and a recovery archive whenever
editor work is at risk.

```text
Rust server process
  watcher -> low-priority observer -> immutable Gdisk
                                  -> maintenance coordinator/journal
                                  -> generation gate -> G1 stores
                                  -> semantic evidence bundle
                                              |
                                              | exact operation/ACK protocol
                                              v
the sole interactive client
  explicit buffer registry -> private archive -> view dispositions/recovery
```

The server owns observation, candidate and incident identity, maintenance
epochs, selected graph/store generations, the transaction journal, retained
view authority, and queued presentation operations.  A request reads one
immutable selected snapshot rather than independently consulting mutable
stores.  Ordinary observation, Git work, and partial maintenance leave that
snapshot queryable; full rebuild alone takes a visibly exclusive generation
gate while reconstructing the derived stores.

The client owns exact editor text, native undo, windows, and application
tokens.  At maintenance entry it locks an explicit registry census and
publishes a private checksummed archive before an external writer or
destructive store step can begin.  It acknowledges exact view dispositions,
evidence checksums, archive finalization, and the terminal unlock.  A
disconnect does not erase any of those debts.

A dirty impacted view is retired as detached recovery; a dirty orthogonal
view remains byte-for-byte untouched and editable but is marked presentation
stale.  A clean impacted view can be rerendered from its retained recipe and
forest.  Search membership is never rerun automatically.

Most headlines are 'content' -- that is, they correspond to something in the graph. If they are new, they won't have an ID, but once they are saved, they will. The ID is one kind of 'metadata' that can precede a headline. There are some others, as detailed in the `OrgNodeMetadata`. Of these, the most interesting is 'node_type'.

The default type of an org node is 'content'. Content nodes cause Rust to change the contents of the graph upon saving. But there can be other kinds. So far the only other one implemented is an 'alias' node. Its org-children are aliases that become associated with the 'alias' node's parent. Thus an 'alias' node does not dictate content; rather, it influences the data within the 'content' node that is its org-parent.
