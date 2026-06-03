;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'skg-keymaps-and-aliases)
(require 'skg-length-prefix)
(require 'skg-request-save) ; for skg-big-nonfatal-message

(define-minor-mode skg-diff-analysis-mode
  "Minor mode for SKG analysis buffers."
  :lighter " SKG-Analysis"
  :keymap skg-diff-analysis-mode-map)

(defun skg-diff-report ()
  "Request an org report of semantic graph changes."
  (interactive)
  (let ((unsaved-buffers
         (cl-remove-if-not
          (lambda (buf)
            (and (buffer-local-value 'skg-view-uri buf)
                 (buffer-modified-p buf)))
          (buffer-list))))
    (when unsaved-buffers
      (error "Cannot analyze diff: unsaved skg buffer(s): %s"
             (mapconcat #'buffer-name unsaved-buffers ", "))))
  (let* ((include-staged (y-or-n-p "Include staged changes? "))
         (include-unstaged
          (if include-staged
              (y-or-n-p "Include unstaged changes? ")
            t))
         (tcp-proc (skg-tcp-connect-to-rust)))
    (skg-register-response-handler
     'diff-analysis
     #'skg--diff-analysis-handler
     t)
    (skg-lp-reset)
    (process-send-string
     tcp-proc
     (concat
      (prin1-to-string
       `((request . "diff analysis")
         (include-staged . ,(if include-staged "true" "false"))
         (include-unstaged . ,(if include-unstaged "true" "false"))))
      "\n"))))

(defun skg--diff-analysis-handler (_tcp-proc payload)
  "Display a diff-analysis response PAYLOAD."
  (condition-case err
      (let* ((response (read payload))
             (content (cadr (assoc 'content response)))
             (errors-list (cadr (assoc 'errors response)))
             (warnings-list (cadr (assoc 'warnings response)))
             (has-errors (skg--message-list-nonempty-p errors-list))
             (has-warnings (skg--message-list-nonempty-p warnings-list)))
        (skg-big-nonfatal-message
         "*skg diff analysis*"
         (cond
          ((and has-errors has-warnings)
           "Diff analysis completed with errors and warnings")
          (has-errors
           "Diff analysis completed with errors")
          (has-warnings
           "Diff analysis completed with warnings")
          (t
           "Diff analysis complete"))
         (or content "* diff analysis failed\n** Empty response\n"))
        (when (or has-errors has-warnings)
          (skg-big-nonfatal-message
           "*skg diff analysis messages*"
           "Diff analysis messages"
           (skg-errors-and-warnings-to-org-string
            errors-list warnings-list)))
        (with-current-buffer "*skg diff analysis*"
          (skg-diff-analysis-mode 1)))
    (error
     (message "skg: diff-analysis handler error: %S" err))))

(provide 'skg-request-diff-analysis)
