;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Force-reload all skg modules.
;;; Evaluate this file to pick up code changes without restarting Emacs.

(defun skg-reload ()
  "Unload *almost* all skg features and reload from disk.

Eight stateful files are deliberately absent from the unload list:

- `skg-buffer' defines `skg-content-view-mode' and the
  permanent-local `skg-view-uri'. `unload-feature' would
  destroy both for every already-open skg buffer: the mode
  degrades to org-mode and the buffer-local var is cleared.

- `skg-keymaps-and-aliases' defines minor-mode maps that
  `define-minor-mode' consults at load time. Unloading it and
  then re-loading dependent modules (via the transitive require
  chain) makes those maps void at the define site. Re-reading it
  before `skg-init' also handles newly added map variables in a
  long-running Emacs session where the feature was already
  provided by older code.

- `skg-state' owns the live TCP process and its request coordinator.
  Unloading it forgets the process without closing it, so the next
  `skg-client-init' leaks the old server connection and opens a new
  one.  Its `defvar' state survives a plain re-evaluation while its
  functions and request-record accessors are refreshed.

- `skg-buffer-registry' owns stable application tokens and lifecycle records
  for already-open buffers.  Its `defvar' table and permanent-local records
  survive while a plain re-evaluation refreshes the registry functions.

- `skg-request-reload-paths' owns live filesystem-watch descriptors.
  Those watches likewise must survive, while re-evaluation safely
  refreshes their symbol callbacks, hook, and server-push handler.

- `skg-maintenance' and `skg-pull' own the live incident/archive/application
  and client Git-child retry state.  Their `defvar' state must likewise survive
  while their functions are refreshed.

- `skg-worktree-guard' installs process-boundary advice.  Plain
  re-evaluation refreshes its functions without duplicating advice.

All eight stateful files are idempotent on re-evaluation, so we pick up
edits to them via plain `load-file' instead.

The herald rule table (`heralds--transform-rules', fetched from
the server at connect time) is captured before the unload and
re-installed after, since `unload-feature' on
`heralds-minor-mode' would otherwise wipe it until the next
connect.

PITFALL: the re-install is wrapped in `unwind-protect'. The herald
table is session-only state with no on-disk source, so once
`unload-feature' wipes it the only way back is the captured copy.
If a reloaded file signals (e.g. a stray edit-in-progress), an
unprotected re-install would be skipped and the table lost for the
rest of the session -- the user then sees \"Heralds disabled: no
herald rule table from the server\" on every `heralds-minor-mode'
toggle, even though the server is fine. `unwind-protect' restores
the table whether or not the reload itself succeeds; the reload
error still propagates so the user can fix it."
  (interactive)
  (when (and (boundp 'skg--active-request-id)
             skg--active-request-id)
    (user-error "skg: wait for request %s before reloading client code"
                skg--active-request-id))
  (let* ((herald-rules (and (boundp 'heralds--transform-rules)
                            heralds--transform-rules))
         (elisp-dir
          (file-name-directory
           (symbol-file 'skg-reload 'defun))))
    (unwind-protect
        (skg--reload-modules elisp-dir)
      (when herald-rules
        ;; A load error may have aborted the reload before
        ;; `heralds-minor-mode' was re-loaded, leaving
        ;; `heralds-install-rules' undefined; pull it back in so we can
        ;; still restore the captured table.
        (unless (fboundp 'heralds-install-rules)
          (load-file (expand-file-name "heralds-minor-mode.el" elisp-dir)))
        (heralds-install-rules herald-rules))))
  (message "skg: all modules reloaded"))

(defun skg--reload-modules (elisp-dir)
  "Unload every skg feature and reload the client from ELISP-DIR.
The destructive half of `skg-reload', kept separate so the
herald-table preservation in `skg-reload' can be exercised without
actually unloading the world.  See `skg-reload' for why
`skg-buffer', `skg-keymaps-and-aliases', `skg-state', `skg-buffer-registry',
`skg-request-reload-paths', `skg-maintenance', `skg-pull', and
`skg-worktree-guard' are
reloaded by hand rather than via `unload-feature'.  The stateless recovery
archive writer is also re-evaluated before the incident coordinator which
calls it."
  (let ((skg-features
         '( skg-client
            skg-compare-sexpr
            skg-config
            skg-focus
            skg-id-search
            skg-length-prefix
            skg-lens
            skg-lock-buffers
            skg-metadata
            skg-modify-graph
            skg-view-new-empty
            skg-org-fold
            skg-request-file-path
            skg-request-diff-analysis
            skg-request-export-org
            skg-request-git-diff-mode
            skg-request-rerender-all-views
            skg-request-save
            skg-request-single-root-content-view
            skg-request-source-sets
            skg-request-text-search
            skg-request-verify-connection
            skg-search-make-link
            skg-request-views
            skg-sexpr-dsl-for-edits
            skg-sexpr-cycling
            skg-sexpr-edit
            skg-sexpr-org-bijection
            skg-sexpr-search
            skg-activeNode-defaults
            skg-test-utils
            skg-readable-ids
            skg-file-minor-mode
            skg-git-add
            skg-view-org-ancestry
            heralds-minor-mode )))
    (dolist (feat skg-features)
      (when (featurep feat)
        (unload-feature feat t)))
    (load-file (expand-file-name "skg-state.el"                elisp-dir))
    (load-file (expand-file-name "skg-buffer-registry.el"      elisp-dir))
    (load-file (expand-file-name "skg-keymaps-and-aliases.el" elisp-dir))
    (load-file (expand-file-name "skg-init.el"                elisp-dir))
    (load-file (expand-file-name "skg-recovery-archive.el"    elisp-dir))
    (load-file (expand-file-name "skg-maintenance.el"         elisp-dir))
    (load-file (expand-file-name "skg-pull.el"                elisp-dir))
    (load-file (expand-file-name "skg-worktree-guard.el"      elisp-dir))
    (load-file (expand-file-name "skg-request-reload-paths.el" elisp-dir))
    (load-file (expand-file-name "skg-buffer.el"              elisp-dir))))

(provide 'skg-reload)
