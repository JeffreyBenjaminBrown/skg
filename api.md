# The new way (in progress)

## From Rust to Emacs

Rust sends the entire text document to Emacs, org-bullets and all.
Each org-bullet is followed by a space and, some metadata, a space,
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
  - There should be no < or > characters.

# The old way

## From Emacs to Rust

Protocol: TCP on port 1730
Format: Line-delimited S-expressions

### Endpoints

Terms in all caps below represent variables.

- Document Request: ((request . "single document") (id . "NODE_ID"))
- Title Search: ((request . "title matches") (terms . "SEARCH_TERMS"))

## From Rust to Emacs

- Single document view:
  - `view`    : `single document`
  - `content` : a length-1 list of nodes, each containing:
    - `id`       : A URI. Each node has a different one.
    - `heading`  : the text of a heading (bullet)
    - `focused`  : absent almost everywhere, but `t` for the node which the document was summoned in order to view.
    - `folded`   : absent or `t`. Indicates folding in the org-mode sense.
    - `body`     : absent or the text just under the heading.
    - `repeated` : absent or `t`. See comment in `rust/types.rs`.
    - `content`  : possibly absent, a list of nodes.
- Title search:
  - A text document displayed verbatim by Emacs. (Its structure is obvious to a human reader, but Emacs does not parse it.)
