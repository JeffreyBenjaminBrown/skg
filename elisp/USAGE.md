# A full list of user-facing commands

(This list will probably go stale very soon.)
```
heralds-minor-mode
skg-open-empty-content-view
skg-visit-link
skg-request-save-buffer
skg-request-single-root-content-view-from-id
skg-request-title-matches
skg-request-aliases-view
skg-request-containerward-view
skg-request-sourceward-view
```

And these are sometimes useful for troubleshooting,
not intended for regular use.
```
skg-verify-connection
skg-doc-disconnect
```

# Initializing

First start the TYpeDB and Rust servers,
by running `run-servers.sh` (from the shell).

Then load the Emacs client,
by evaluating the following (here, in Emacs):
```elisp
(progn
  (load-file "skg-init.el")
  (skg-client-init "../data/subscribe/skgconfig.toml"))
```

# Commands you can run

(See a later section for what each command does.)

Now these commands can be evaluated (here, in Emacs):
```
  (skg-request-single-root-content-view-from-id "1")
  (skg-request-title-matches "1")
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
