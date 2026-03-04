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
  (skg-client-init "../data/real/skgconfig.toml"))
```

# Commands you can run

(See a later section for what each command does.)

Now these commands can be evaluated (here, in Emacs):
```
  (skg-request-single-root-content-view-from-id "wut")
  (skg-request-title-matches "science")
    ;; try match, title, or second
  (skg-git-diff-toggle)
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
