;;; -*- lexical-binding: t; -*-
;;;
;;; USER-FACING FUNCTIONS
;;;   skg-view-diff-mode

(require 'skg-length-prefix)
(require 'skg-request-save) ; for skg-show-save-warnings

(defun skg-view-diff-mode ()
  "Toggle git diff mode on the server.
When enabled, subsequent content views and saves show
what changed between HEAD and the worktree."
  (interactive)
  (let* ((tcp-proc (skg-tcp-connect-to-rust))
         (save-buffer (current-buffer))
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
         (with-current-buffer ;; Use with-current-buffer because the process filter runs in whatever buffer is current when the TCP response arrives — NOT the buffer the user called skg-view-diff-mode from.
             save-buffer
           (skg-request-save-buffer))))
     t)
    (skg-lp-reset)
    (process-send-string tcp-proc request-sexp)))

(provide 'skg-request-git-diff-mode)
