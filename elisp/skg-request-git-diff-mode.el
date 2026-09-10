;;; -*- lexical-binding: t; -*-

(require 'skg-length-prefix)
(require 'skg-lock-buffers)
(require 'skg-request-save) ; for skg-big-nonfatal-message
(require 'skg-request-rerender-all-views)
(require 'skg-state) ; for skg--git-diff-mode-enabled

(defun skg-view-diff-mode (&optional approved-pids)
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
         (cond
          ;; Mirror the server's per-connection state. A refusal (any
          ;; other content) leaves the mirror unchanged.
          ((and content (string-prefix-p "Git diff mode enabled" content))
           (setq skg--git-diff-mode-enabled t))
          ((and content (string-prefix-p "Git diff mode disabled" content))
           (setq skg--git-diff-mode-enabled nil)))
         (if (and content (string-match-p "\nWarning:" content))
             (skg-big-nonfatal-message
              "*skg diff-mode warnings*"
              (car (split-string content "\n"))
              content)
           (message "%s" (or content "toggled")))))
     t)
    (skg--register-rerender-stream-handlers)
    (skg--register-rerender-overPrivateText-confirmation
     (lambda (pids) (skg-view-diff-mode pids))
     'git-diff-mode)
    (skg-lp-reset)
    (process-send-string
     tcp-proc
     (concat (prin1-to-string
              (append
               '((request . "git diff mode toggle"))
               (when approved-pids
                 `((allow-overPrivateText-telescopes ,@approved-pids)))))
             "\n"))))

(provide 'skg-request-git-diff-mode)
