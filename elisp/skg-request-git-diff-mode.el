;;; -*- lexical-binding: t; -*-
;;;
;;; USER-FACING FUNCTIONS
;;;   skg-diff-view

(require 'skg-length-prefix)

(defun skg-diff-view ()
  "Toggle git diff mode on the server.
When enabled, subsequent content views and saves show
what changed between HEAD and the worktree."
  (interactive)
  (let* ((tcp-proc (skg-tcp-connect-to-rust))
         (request-sexp
          (concat (prin1-to-string
                   '((request . "git diff mode toggle")))
                  "\n")))
    (skg-register-response-handler
     'git-diff-mode
     (lambda (_tcp-proc payload)
       (let* ((response (read payload))
              (content (cadr (assoc 'content response))))
         (message "%s" (or (and content (format "%s" content))
                           "toggled"))
         (skg-request-save-buffer)))
     t)
    (skg-lp-reset)
    (process-send-string tcp-proc request-sexp)))

(provide 'skg-request-git-diff-mode)
