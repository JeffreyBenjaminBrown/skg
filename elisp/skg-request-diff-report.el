;;; -*- lexical-binding: t; -*-

(require 'skg-keymaps-and-aliases)
(require 'skg-length-prefix)
(require 'skg-buffer) ; for skg--unsaved-view-buffers
(require 'skg-request-save) ; for skg-big-nonfatal-message

(defun skg-diff-report ()
  "Request an org report of semantic graph changes."
  (interactive)
  (let ((unsaved-buffers (skg--unsaved-view-buffers)))
    (when unsaved-buffers
      (error "Cannot make diff report: unsaved skg buffer(s): %s"
             (mapconcat #'buffer-name unsaved-buffers ", "))))
  (let* ((include-staged (y-or-n-p "Include staged changes? "))
         (include-unstaged
          (if include-staged
              (y-or-n-p "Include unstaged changes? ")
            t))
         (tcp-proc (skg-tcp-connect-to-rust)))
    (skg-register-response-handler
     'diff-report
     #'skg--diff-report-handler
     t)
    (skg-lp-reset)
    (process-send-string
     tcp-proc
     (concat
      (prin1-to-string
       `((request . "diff report")
         (include-staged . ,(if include-staged "true" "false"))
         (include-unstaged . ,(if include-unstaged "true" "false"))))
      "\n"))))

(defun skg--diff-report-handler (_tcp-proc payload)
  "Display a diff-report response PAYLOAD."
  (condition-case err
      (let* ((response (read payload))
             (content (cadr (assoc 'content response)))
             (errors-list (cadr (assoc 'errors response)))
             (warnings-list (cadr (assoc 'warnings response)))
             (has-errors (skg--message-list-nonempty-p errors-list))
             (has-warnings (skg--message-list-nonempty-p warnings-list)))
        (skg-big-nonfatal-message
         "*skg diff report*"
         (cond
          ((and has-errors has-warnings)
           "Diff report completed with errors and warnings")
          (has-errors
           "Diff report completed with errors")
          (has-warnings
           "Diff report completed with warnings")
          (t
           "Diff report complete"))
         (or content "* diff report failed\n** Empty response\n"))
        (when (or has-errors has-warnings)
          (skg-big-nonfatal-message
           "*skg diff report messages*"
           "Diff report messages"
           (skg-errors-and-warnings-to-org-string
            errors-list warnings-list)))
        (with-current-buffer "*skg diff report*"
          (skg-report-mode 1)))
    (error
     (message "skg: diff-report handler error: %S" err))))

(provide 'skg-request-diff-report)
