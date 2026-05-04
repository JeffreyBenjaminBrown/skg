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

- `skg-keymaps-and-aliases' defines `skg-id-stack-mode-map',
  which `skg-id-search's `define-minor-mode' consults at load
  time. Unloading it and then re-loading `skg-id-search' (via
  the transitive require chain) makes the map void at the
  define site.

Both files are idempotent on re-evaluation (no top-level
hooks, no advice, just `defvar', `define-derived-mode',
`define-key', and `defun's), so we pick up edits to them via
plain `load-file' at the end instead."
  (interactive)
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
            skg-view-new-empty
            skg-org-fold
            skg-request-file-path
            skg-request-git-diff-mode
            skg-request-rerender-all-views
            skg-request-save
            skg-request-single-root-content-view
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
            skg-show-org-ancestry
            heralds-minor-mode )))
    (dolist (feat skg-features)
      (when (featurep feat)
        (unload-feature feat t))))
  (let ((elisp-dir
         (file-name-directory
          (symbol-file 'skg-reload 'defun))))
    (load-file (expand-file-name "skg-init.el"                elisp-dir))
    (load-file (expand-file-name "skg-keymaps-and-aliases.el" elisp-dir))
    (load-file (expand-file-name "skg-buffer.el"              elisp-dir)))
  (message "skg: all modules reloaded"))

(provide 'skg-reload)
