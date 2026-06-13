;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Force-reload all skg modules.
;;; Evaluate this file to pick up code changes without restarting Emacs.

(defun skg-reload ()
  "Unload *almost* all skg features and reload from disk.

Two files are deliberately absent from the unload list:

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

Both files are idempotent on re-evaluation (no top-level
hooks, no advice, just `defvar', `define-derived-mode',
`define-key', and `defun's), so we pick up edits to them via
plain `load-file' instead.

The herald rule table (`heralds--transform-rules', fetched from
the server at connect time) is captured before the unload and
re-installed after, since `unload-feature' on
`heralds-minor-mode' would otherwise wipe it until the next
connect."
  (interactive)
  (let ((herald-rules (and (boundp 'heralds--transform-rules)
                           heralds--transform-rules))
        (skg-features
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
            skg-truenode-defaults
            skg-state
            skg-test-utils
            skg-readable-ids
            skg-file-minor-mode
            skg-git-add
            skg-view-org-ancestry
            heralds-minor-mode )))
    (dolist (feat skg-features)
      (when (featurep feat)
        (unload-feature feat t)))
    (let ((elisp-dir
           (file-name-directory
            (symbol-file 'skg-reload 'defun))))
      (load-file (expand-file-name "skg-keymaps-and-aliases.el" elisp-dir))
      (load-file (expand-file-name "skg-init.el"                elisp-dir))
      (load-file (expand-file-name "skg-buffer.el"              elisp-dir)))
    (when herald-rules
      (heralds-install-rules herald-rules)))
  (message "skg: all modules reloaded"))

(provide 'skg-reload)
