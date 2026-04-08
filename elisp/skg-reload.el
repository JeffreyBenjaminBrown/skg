;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Force-reload all skg modules.
;;; Evaluate this file to pick up code changes without restarting Emacs.
;;;
;;; USER-FACING FUNCTIONS
;;;   skg-reload

(defun skg-reload ()
  "Unload all skg features and reload from disk."
  (interactive)
  (let ((skg-features
         '( skg-buffer
            skg-client
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
            skg-request-title-matches
            skg-request-verify-connection
            skg-request-views
            skg-sexpr-dsl-for-edits
            skg-sexpr-cycling
            skg-sexpr-edit
            skg-sexpr-org-bijection
            skg-sexpr-search
            skg-truenode-defaults
            skg-state
            skg-test-utils
            skg-keymaps-and-aliases
            skg-view-org-ancestry
            heralds-minor-mode )))
    (dolist (feat skg-features)
      (when (featurep feat)
        (unload-feature feat t))))
  (let ((init-file
         (expand-file-name
          "skg-init.el"
          (file-name-directory
           (symbol-file 'skg-reload 'defun)))))
    (load-file init-file))
  (message "skg: all modules reloaded"))

(provide 'skg-reload)
