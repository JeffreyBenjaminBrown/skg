# From Emacs to Rust

Protocol: TCP on port 1730
Format: Line-delimited S-expressions

## Endpoints

Terms in all caps below represent variables.

- Document Request: ((request . "single document") (id . "NODE_ID"))
- Title Search: ((request . "title matches") (terms . "SEARCH_TERMS"))
