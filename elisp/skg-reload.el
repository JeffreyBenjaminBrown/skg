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
         '( skg-commands
            skg-client
            skg-request-title-matches
            skg-request-save
            skg-request-single-root-content-view
            skg-request-verify-connection
            skg-request-file-path
            skg-request-git-diff-mode
            skg-request-views
            skg-new-empty-view
            skg-length-prefix
            skg-state
            skg-buffer
            skg-metadata
            skg-id-search
            skg-lock-buffers
            skg-org-fold
            skg-focus
            skg-sexpr-edit
            skg-sexpr-search
            skg-sexpr-dsl-for-edits
            skg-sexpr-org-bijection
            skg-compare-sexpr
            skg-lens
            skg-test-utils
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
