# User-facing commands

can be found [here](./COMMANDS.org)

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
