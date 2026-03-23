;;; -*- lexical-binding: t; -*-
;;;
;;; USER-FACING FUNCTIONS
;;;   skg-view-diff-mode

(require 'skg-length-prefix)
(require 'skg-request-save) ; for skg-big-nonfatal-message
(require 'skg-request-rerender-all-views)

(defun skg-view-diff-mode ()
  "Toggle git diff mode on the server.
When enabled, subsequent content views and saves show
what changed between HEAD and the worktree."
  (interactive)
  (let ((dirty-buffers
         (cl-remove-if-not
          (lambda (buf)
            (and (buffer-local-value 'skg-view-uri buf)
                 (buffer-modified-p buf)))
          (buffer-list))))
    (when dirty-buffers
      (error "Cannot toggle diff mode: unsaved skg buffer(s): %s"
             (mapconcat #'buffer-name dirty-buffers ", "))))
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
         (if (and content (string-match-p "\nWarning:" content))
             (skg-big-nonfatal-message
              "*skg diff-mode warnings*"
              (car (split-string content "\n"))
              content)
           (message "%s" (or content "toggled")))
         (skg-request-rerender-all-views)))
     t)
    (skg-lp-reset)
    (process-send-string tcp-proc request-sexp)))

(provide 'skg-request-git-diff-mode)
