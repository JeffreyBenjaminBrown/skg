;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Stage every change and git-commit it, in every owned
;;; skgrepo that has changes, with one message
;;; ('docs/diff-view.org', "Stage and commit").
;;; When the readiness summary is incomplete, the server answers with
;;; it instead and changes nothing; only the typed word "yes" resends
;;; the request confirmed. Every answer is followed by a rerender
;;; stream, which after a commit redraws the open buffers of the
;;; diff view.
;;;
;;; DATA USED/ASSUMED: See docs/api-and-formats.org.

(require 'skg-buffer)
(require 'skg-length-prefix)
(require 'skg-lock-buffers)
(require 'skg-request-rerender-all-views)
(require 'skg-request-save) ; for skg--readiness-summary-message

(defun skg-stage-and-commit ()
  "Stage every change and git-commit it, in every owned skgrepo
that has changes, with one message. If not every changed file is
commit-ready, show the readiness summary and proceed only if the
user types \"yes\"."
  (interactive)
  (when (skg--unsaved-view-buffers)
    (user-error "Save or revert every skg view before staging and committing"))
  (let ((commit-message (read-string "Commit message: ")))
    (when (string-empty-p (string-trim commit-message))
      (user-error "A commit message is required"))
    (skg--stage-and-commit-send commit-message nil)))

(defun skg--stage-and-commit-send (commit-message confirmed)
  "Send the request with COMMIT-MESSAGE, and '(confirmed . t)' when
CONFIRMED."
  (let ((tcp-proc (skg-tcp-connect-to-rust)))
    (skg--begin-stream "stage and commit")
    (skg--lock-all-skg-buffers)
    (skg--register-rerender-stream-handlers)
    (skg--wrap-rerender-done-with-after-empty-stream)
    (skg-register-response-handler
     'stage-and-commit
     (lambda (_tcp payload)
       (skg--stage-and-commit-handler commit-message payload))
     t)
    (skg-lp-reset)
    (process-send-string
     tcp-proc
     (concat (prin1-to-string
              (append `((request . "stage and commit")
                        (message . ,commit-message))
                      (when confirmed '((confirmed . t)))))
             "\n"))))

(defun skg--stage-and-commit-handler (commit-message payload)
  "Handle the stage-and-commit response PAYLOAD. On a request for
confirmation, ask for it, and on \"yes\" resend COMMIT-MESSAGE
confirmed once the empty rerender stream that follows is done."
  (condition-case err
      (let ((response (read payload)))
        (if (assq 'needs-confirmation response)
            (when (skg--stage-and-commit-confirmed-p
                   (assq 'readiness-summary response))
              (setq skg--rerender-after-empty-stream
                    (lambda ()
                      (skg--stage-and-commit-send commit-message t))))
          (message "%s" (skg--stage-and-commit-result-message response))))
    (error (message "skg: stage-and-commit handler error: %S" err))))

(defun skg--stage-and-commit-confirmed-p (summary)
  "Show the readiness SUMMARY; non-nil iff the user types exactly \"yes\"."
  (string= "yes"
           (read-string
            (format "%s. Type yes to commit anyway: "
                    (skg--readiness-summary-message summary)))))

(defun skg--stage-and-commit-result-message (response)
  "The line shown for RESPONSE, '((commits ((SKGREPO COMMIT-ID) ...))
\(errors (...)) (warnings (...)))'."
  (let ((commits (cadr (assq 'commits response)))
        (errors (cadr (assq 'errors response))))
    (concat
     (if commits
         (format "Committed in %s"
                 (mapconcat
                  (lambda (commit)
                    (let ((commit-id (format "%s" (cadr commit))))
                      (format "%s (%s)" (car commit)
                              (substring commit-id 0 (min 7 (length commit-id))))))
                  commits ", "))
       "Nothing was committed")
     (if errors
         (format ". Errors: %s"
                 (mapconcat (lambda (e) (format "%s" e)) errors "; "))
       "."))))

(provide 'skg-request-stage-and-commit)
