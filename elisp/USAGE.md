# Using the Emacs client

First start the TYpeDB server
  (run `typedb server` from a shell)
and after that start the Rust server
  (evaluate `cargo run` from a shell,
   from the root of this project).
Finally within Emacs, evaluate `elisp/skg-init.el`
  (visit it, then run `M-x eval-buffer`).

Now these commands can be run by evaluating them from here:
```
  (skg-request-single-root-content-view-from-node "1")
    ;; try 1, 1a, or a
  (skg-request-title-matches "second")
    ;; try match, title, second
  (skg-doc-disconnect)
```
and this can be run interactively from the skg content view buffer:
```
  (skg-request-save-buffer) ;; `M-x skg-save`
```

`(request-sexp-doc-from-node)` causes Emacs to ask Rust to ask TypeDB
for a content-view document containing the node with the specified ID.
Rust builds an s-expression representing an org-document
based on the result, and sends that to Emacs,
causing Emacs to open an org-mode buffer displaying the results.

`(request-title-matches)` asks Rust to search the Tantivy index
for titles matching the search terms,
and displays the results in a buffer.

Surprisingly, the TCP connection
does not need to be explicitly launched,
because each of the client's `request-*` functions
calls `(skg-tcp-connect-to-rust)`
(which is idempotent and cheap to rerun).
