# Initializing

First start the TYpeDB and Rust servers,
by running `run-servers.sh` (from the shell).

Then load the Emacs client,
by evaluating the following (here, in Emacs):
```elisp
(progn
  (load-file "skg-init.el")
  (skg-client-init "../data/skgconfig.toml"))
```

# Commands you can run

(See a later section for what each command does.)

Now these commands can be evaluated (here, in Emacs):
```
  (skg-request-single-root-content-view-from-id "root")
  (skg-request-title-matches "2")
    ;; try match, title, or second
  (skg-verify-connection)
    ;; test connection to server
  (skg-doc-disconnect)
```

From a skg content view buffer, these can be run:
```
skg-request-save-buffer
skg-request-containerward-view
skg-request-sourceward-view
```

From any link in a search results buffer, links can be visited:
```
skg-visit-link
```

# What those commands do

`(request-sexp-doc-from-node)` causes Emacs to ask Rust to ask TypeDB
for a content-view document containing the node with the specified ID.
Rust builds an s-expression representing an org-document
based on the result, and sends that to Emacs,
causing Emacs to open an org-mode buffer displaying the results.

`(request-title-matches)` asks Rust to search the Tantivy index
for titles matching the search terms,
and displays the results in a buffer.

`(skg-verify-connection)` sends a simple ping to the Rust server
to verify the connection is working. The server responds with a
confirmation message that is displayed in the minibuffer.

Surprisingly, the TCP connection
does not need to be explicitly launched,
because each of the client's `request-*` functions
calls `(skg-tcp-connect-to-rust)`
(which is idempotent and cheap to rerun).
