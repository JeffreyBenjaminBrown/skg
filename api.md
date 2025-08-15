# From Emacs to Rust

Protocol: TCP on port 1730
Format: Line-delimited S-expressions

## Endpoints

Terms in all caps below represent variables.

- Document Request: ((request . "single document") (id . "NODE_ID"))
- Title Search: ((request . "title matches") (terms . "SEARCH_TERMS"))

# From Rust to Emacs

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
