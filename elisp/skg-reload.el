;;; skg-reload.el --- Reload the client from disk, no restart  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; 'skg-reload' (M-x skg-reload) reloads every skg module from disk,
;;; to pick up code changes without restarting Emacs. Nothing in the
;;; client requires this file; load it with 'load-file' first (as
;;; docs/setup.org does). It closes the connection first
;;; ('skg-connection-end'), unloads 'skg--reload-unloaded-features',
;;; then loads 'skg-init.el' again ('skg--reload-modules').
;;;
;;; It carries across the reload what would otherwise be lost: the
;;; settings 'skg-client-init' made and the linkstack
;;; ('skg--reload-preserved-vars'), and the herald rule table and
;;; skgrepo labels fetched from the server, restored inside an
;;; 'unwind-protect' so a load error cannot lose them. It never unloads
;;; 'skg--reload-kept-features': 'skg-buffer.el' and
;;; 'skg-keymaps-and-aliases.el' it re-reads with 'load-file' instead,
;;; and 'skg-log.el' it leaves as first loaded. The docstring of
;;; 'skg-reload' says why.
;;;
;;; Code:

(defconst skg--reload-preserved-vars
  '( skg-port
     skg-config-file-path
     skg-config-dir
     skg-linkstack )
  "Variables from `skg-state' whose values `skg-reload' carries across
the unload. See `skg-reload'.")

(defun skg-reload ()
  "Unload *almost* all skg features and reload from disk.

Two files are deliberately absent from the unload list:

- `skg-buffer' defines `skg-content-view-mode' and the
  permanent-local `skg-view-id'. `unload-feature' would
  destroy both for every already-open skg buffer: the mode
  degrades to org-mode and the buffer-local var is cleared.

- `skg-keymaps-and-aliases' defines minor-mode maps that
  `define-minor-mode' consults at load time. Unloading it and
  then re-loading dependent modules (via the transitive require
  chain) makes those maps void at the define site. Re-reading it
  before `skg-init' also handles newly added map variables in a
  long-running Emacs session where the feature was already
  provided by older code.

Both files are idempotent on re-evaluation (no top-level
hooks, no advice, just `defvar', `define-derived-mode',
`define-key', and `defun's), so we pick up edits to them via
plain `load-file' instead.

The herald rule table (`heralds--transform-rules', fetched from
the server at connect time) and the skgrepo label table served
beside it (`heralds--skgrepo-labels') are captured before the unload
and re-installed after, since `unload-feature' on
`heralds-minor-mode' would otherwise wipe them until the next
connect.

PITFALL: the re-install is wrapped in `unwind-protect'. The herald
table is session-only state with no on-disk repo, so once
`unload-feature' wipes it the only way back is the captured copy.
If a reloaded file signals (e.g. a stray edit-in-progress), an
unprotected re-install would be skipped and the table lost for the
rest of the session -- the user then sees \"Heralds disabled: no
herald rule table from the server\" on every `heralds-minor-mode'
toggle, even though the server is fine. `unwind-protect' restores
the table whether or not the reload itself succeeds; the reload
error still propagates so the user can fix it.

The client settings `skg-client-init' makes (`skg-port',
`skg-config-file-path', `skg-config-dir') and the user's navigation
history (`skg-linkstack') are carried across the same way, inside
the same `unwind-protect', since `unload-feature' on `skg-state'
would otherwise wipe them. Without the first three, the next request
after a reload could not reconnect (it has no port), and every
request failed until the user ran `skg-client-init' again. Everything
else in `skg-state' (the TCP process, the response handler maps, the
length-prefix receiver state) is rebuilt, not carried: the connection
is closed anyway, and the next request opens a fresh one.

The open connection to the server is closed first
\(`skg-connection-end'), so that the reloaded client does not leave
the old one open beside the one it makes on its next request."
  (interactive)
  (when (fboundp 'skg-connection-end)
    (skg-connection-end))
  (let* ((client-settings
          ;; Captured as (SYMBOL . VALUE); see the docstring.
          (mapcar (lambda (sym)
                    (cons sym (and (boundp sym) (symbol-value sym))))
                  skg--reload-preserved-vars))
         (herald-rules (and (boundp 'heralds--transform-rules)
                            heralds--transform-rules))
         (skgrepo-labels (and (boundp 'heralds--skgrepo-labels)
                              heralds--skgrepo-labels))
         (elisp-dir
          (file-name-directory
           (symbol-file 'skg-reload 'defun))))
    (unwind-protect
        (skg--reload-modules elisp-dir)
      (dolist (pair client-settings)
        (set (car pair) (cdr pair)))
      (when herald-rules
        ;; A load error may have aborted the reload before
        ;; `heralds-minor-mode' was re-loaded, leaving
        ;; `heralds-install-rules' undefined; pull it back in so we can
        ;; still restore the captured table.
        (unless (fboundp 'heralds-install-rules)
          (load-file (expand-file-name "heralds-minor-mode.el" elisp-dir)))
        (heralds-install-rules herald-rules)
        (setq heralds--skgrepo-labels skgrepo-labels))))
  (message "skg: all modules reloaded"))

(defconst skg--reload-unloaded-features
  '( skg-client
     skg-compare-sexpr
     skg-config
     skg-picker
     skg-id-search
     skg-linkstack
     skg-push-ancestry
     skg-length-prefix
     skg-lens
     skg-lock-buffers
     skg-metadata
     skg-change-facts
     skg-restore-requests
     skg-modify-graph
     skg-view-new-empty
     skg-org-fold
     skg-redraw-delta
     skg-recovery
     skg-request-file-path
     skg-request-herald-rules
     skg-request-investigate-unknown-node
     skg-request-delete-references-to-absent-node
     skg-request-describe-heralds
     skg-request-relRepo-info
     skg-request-flag-state
     skg-request-export-org
     skg-request-import-md-and-org
     skg-request-diff-view
     skg-processed
     skg-request-rebuild-session-lifetime-stores
     skg-request-rerender-all-views
     skg-request-save
     skg-redraw-whole-view
     skg-save-confirmations
     skg-messages
     skg-request-server-warnings
     skg-repo-files-reloaded
     skg-request-single-root-content-view
     skg-request-repo-sets
     skg-request-perspective
     skg-request-receipt
     skg-request-extend-diff-view
     skg-request-stage-and-commit
     skg-request-strip-body-whitespace
     skg-request-text-search
     skg-request-verify-connection
     skg-search-make-link
     skg-request-views
     skg-sexpr-dsl-for-edits
     skg-sexpr-cycling
     skg-sexpr-edit
     skg-sexpr-org-bijection
     skg-sexpr-search
     skg-shared
     skg-metadata-fields
     skg-state
     skg-test-utils
     skg-readable-ids
     skg-link-annotations
     skg-file-minor-mode
     skg-zippy-mode
     skg-view-ancestry
     skg-fork-dedup
     heralds-minor-mode )
  "The features `skg--reload-modules' unloads, so that requiring them
again loads the files on disk. A feature left out stays as it was first
loaded: requiring it is a no-op, so its old definitions outlive every
reload, and a function that moved into its file since is void (this is
how `skg-redraw-delta' kept whole redraws of saved buffers from
happening). Every feature the client's files provide is here, but for
those `skg-reload' reloads by hand or keeps on purpose,
`skg--reload-kept-features'.")

(defconst skg--reload-kept-features
  '( skg-buffer               ; reloaded by `load-file'; see `skg-reload'
     skg-keymaps-and-aliases  ; reloaded by `load-file'; see `skg-reload'
     skg-reload               ; the code running the reload
     skg-log )                ; keeps the user's log settings
  "The client features `skg--reload-modules' does not unload.")

(defun skg--reload-modules (elisp-dir)
  "Unload every skg feature and reload the client from ELISP-DIR.
The destructive half of `skg-reload', kept separate so the
herald-table preservation in `skg-reload' can be exercised without
actually unloading the world.  See `skg-reload' for why
`skg-buffer' and `skg-keymaps-and-aliases' are reloaded by hand
rather than via `unload-feature'."
  (dolist (feat skg--reload-unloaded-features)
    (when (featurep feat)
      (unload-feature feat t)))
  (load-file (expand-file-name "skg-keymaps-and-aliases.el" elisp-dir))
  (load-file (expand-file-name "skg-init.el"                elisp-dir))
  (load-file (expand-file-name "skg-buffer.el"              elisp-dir)))

(provide 'skg-reload)
