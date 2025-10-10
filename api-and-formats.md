# API

Communication between Rust and Emacs is via TCP on port 1730, configurable in any skgconfig.toml. The connection is persistent.

Note: Port 1729 is used for Rust-TypeDB communication (the TypeDB server), not Rust-Emacs communication.

So far there are these endpoints:

- Verify connection
  - Request: ((request . "verify connection"))
  - Response: Plain text with newline termination: "This is the skg server verifying the connection."
- Search in titles
  - Request: ((request . "title matches") (terms . "SEARCH_TERMS"))
  - Response: Plain text with newline termination.
- Single root content tree view from ID
  - Request: ((request . "single root content view") (id . "NODE_ID"))
  - Response: length-prefixed content, formatted `Content-Length: LENGTH\r\n\r\nPAYLOAD`, where `PAYLOAD` constitutes `LENGTH` bytes. PAYLOAD may contain quotation marks; hence the length prefix. The document structure is detailed below, under `Single root content tree view`.
- Node aliases
  - Request: ((request . "node aliases") (id . "NODE_ID") (level . "LEVEL"))
  - Response: length-prefixed content, formatted `Content-Length: LENGTH\r\n\r\nPAYLOAD`, where `PAYLOAD` constitutes `LENGTH` bytes and contains org-mode formatted aliases.
- Containerward view
  - Request: ((request . "containerward view") (headline . "HEADLINE_TEXT")). HEADLINE_TEXT includes asterisks and metadata but no trailing newline.
  - Response: length-prefixed content, formatted `Content-Length: LENGTH\r\n\r\nPAYLOAD`, where `PAYLOAD` constitutes `LENGTH` bytes and contains org-mode formatted containerward view starting from the specified node.
- Save buffer
  - Request: First `((request . \"save buffer\"))\n"`, then `Content-Length: LENGTH\r\n\r\nPAYLOAD`, where `PAYLOAD` has length `LENGTH`.
  - Response: success/failure indicator followed by length-prefixed content:
    - Success: `save: success\nContent-Length: LENGTH\r\n\r\nPAYLOAD` where PAYLOAD contains the processed buffer content
    - Failure: `save: failure\nContent-Length: LENGTH\r\n\r\nPAYLOAD` where PAYLOAD contains org-mode formatted error details

Error responses are sent as simple text.

# Single root content tree view

Rust sends the entire text document to Emacs, org-bullets and all.
Each org-bullet is followed by a space, some metadata, a space,
and then headline text.

The metadata is an s-expression containing bare values and key-value pairs,
wrapped in "(skg ...)" -- something like
```
 (skg (id long-string) repeated (key value) another-value)
```

Any such string is valid metadata
(although much of it might be ignored by Emacs),
if and only if it adheres to the following:

- parentheses
  - The metadata starts with "(skg" and ends with ")".
  - Key-value pairs are wrapped in their own parens: "(key value)".
- whitespace
  - Whitespace separates all elements.
  - Extra whitespace is ignored.
  - Keys and values should contain no whitespace.

# The .skg file format

It is a specialization of YAML -- that is, every .skg file is valid YAML, although not vice-versa. The fields might change; the definitive source of truth is in `rust/types.rs`. But for the moment they are:

- title: A string, with no newlines. Must be present.
- ids: A list of UUIDs. The first one is the primary id, which is equal to the filename minus the `.skg` extension. Must be non-empty. There can be more than one because nodes might be merged.
- body: An optional string, perhaps including newlines.
- contains: A list of UUIDs. These appear as the node's "children" in an org-view.
- subscribes_to: A list of UUIDs. Can be omitted if empty.
- hides_from_its_subscriptions: A list of UUIDs. Can be omitted if empty.
- overrides_view_of: A list of UUIDs. Can be omitted if empty.

Each node's filename is just its primary ID followed by ".skg".

# The metadata headers

Most org headlines in the client will have a 'metadata header',
consisting of some bare values and some key-value pairs
separated by whitespace. Each kv pair is wrapped in parens: (key value).
The whole collection is wrapped in (skg ...).
This metadata appears right after the org bullet and a whitespace,
and right before another whitespace and the node's title.

The possible metadata is specified in `rust/types/orgnode.rs`.
Metadata is not WYSIWYG; its appearance in the client
is determined by `elisp/heralds-minor-mode`.
