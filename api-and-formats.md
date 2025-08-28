# API

Communication between Rust and Emacs is via TCP on port 1730. The connection is persistent.

So far there are three endpoints:

- Search in titles
  - Request: ((request . "title matches") (terms . "SEARCH_TERMS"))
  - Response: Plain text with newline termination.
- Single root content tree view from ID
  - Request: ((request . "single root content view") (id . "NODE_ID"))
  - Response: length-prefixed content, formatted `Content-Length: LENGTH\r\n\r\nPAYLOAD`, where `PAYLOAD` constitutes `LENGTH` bytes. PAYLOAD may contain quotation marks; hence the length prefix. The document structure is detailed below, under `Single root content tree view`.
- Save buffer
  - Request: First `((request . \"save buffer\"))\n"`, then `Content-Length: LENGTH\r\n\r\nPAYLOAD`, where `PAYLOAD` has length `LENGTH`.
  - Response: length-prefixed content, formatted `Content-Length: LENGTH\r\n\r\nPAYLOAD`, where `PAYLOAD` constitutes `LENGTH` bytes and contains the processed buffer content.

Error responses are sent as simple text.

# Single root content tree view

Rust sends the entire text document to Emacs, org-bullets and all.
Each org-bullet is followed by a space, some metadata, a space,
and then headline text.

The metadata is a comma-separated list of values and key-value pairs,
bracketed by "<<" and ">>" -- something like
```
 <<id:long-string, repeated, key:value, another-value>>
```

Any such string is valid metadata
(although much of it might be ignored by Emacs),
if and only if it adheres to the following:

- commas
  - The values and key-value pairs are separated by commas.
  - There should be no other commas in the metadata.
- colons
  - Keys are separated from values by colons.
  - There should be no other colons.
- whitespace
  - Whitespace in the raw metadata is ignored.
  - Keys and values should contain no whitespace.
- < and > characters
  - The metadata should start with << and wend with >>.
  - There should be no other < or > characters.

# The .skg file format

It is a specialization of YAML -- that is, every .skg file is valid YAML, although not vice-versa. The fields might change; the definitive source of truth is in `rust/types.rs`. But for the moment they are:

- title: A string, with no newlines. Must be present.
- ids: A list of UUIDs. The first one is the primary id, which is equal to the filename minus the `.skg` extension. Must be non-empty. There can be more than one because nodes might be merged.
- body: An optional string, perhaps including newlines.
- contains: A list of UUIDs. These appear as the node's "children" in an org-view.
- subscribes_to: A list of UUIDs. Can be omitted if empty.
- hides_from_its_subscriptions: A list of UUIDs. Can be omitted if empty.
- overrides_view_of: A list of UUIDs. Can be omitted if empty.
