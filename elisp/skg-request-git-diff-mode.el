;;; -*- lexical-binding: t; -*-
;;;
;;; USER-FACING FUNCTIONS
;;;   skg-view-diff-mode

(require 'skg-length-prefix)
(require 'skg-lock-buffers)
(require 'skg-request-save) ; for skg-big-nonfatal-message
(require 'skg-request-rerender-all-views)

(defun skg-view-diff-mode ()
  "Toggle git diff mode on the server and rerender all views.
When enabled, subsequent content views and saves show
what changed between HEAD and the worktree.
Sends a single combined request; the server responds with
git-diff-mode, then streams rerender-lock, rerender-view*,
rerender-done."
  (interactive)
  (let ((unsaved-buffers
         (cl-remove-if-not
          (lambda (buf)
            (and (buffer-local-value 'skg-view-uri buf)
                 (buffer-modified-p buf)))
          (buffer-list))))
    (when unsaved-buffers
      (error "Cannot toggle diff mode: unsaved skg buffer(s): %s"
             (mapconcat #'buffer-name unsaved-buffers ", "))))
  (let ((tcp-proc (skg-tcp-connect-to-rust)))
    (skg--begin-stream "diff-mode toggle")
    (skg--lock-all-skg-buffers)
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
           (message "%s" (or content "toggled")))))
     t)
    (skg--register-rerender-stream-handlers)
    (skg-lp-reset)
    (process-send-string
     tcp-proc
     (concat (prin1-to-string
              '((request . "git diff mode toggle and rerender")))
             "\n"))))

(provide 'skg-request-git-diff-mode)
