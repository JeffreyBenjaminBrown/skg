;;; -*- lexical-binding: t; -*-
;;;
;;; USER-FACING FUNCTIONS
;;;   skg-git-diff-toggle

(defun skg-git-diff-toggle ()
  "Toggle git diff mode on the server.
When enabled, subsequent content views and saves show
what changed between HEAD and the worktree."
  (interactive)
  (let* ((tcp-proc (skg-tcp-connect-to-rust))
         (request-sexp
          (concat (prin1-to-string
                   '((request . "git diff mode toggle")))
                  "\n")))
    (setq skg-doc--response-handler
          #'skg--git-diff-mode-result)
    (process-send-string tcp-proc request-sexp)))

(defun skg--git-diff-mode-result (_tcp-proc string)
  (message "%s" (string-trim string)))

(provide 'skg-request-git-diff-mode)
