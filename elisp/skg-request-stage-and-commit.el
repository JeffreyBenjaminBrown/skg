;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Stage every change and git-commit it, in every owned
;;; skgrepo that has changes, with one message
;;; ('docs/diff-view.org', "Stage and commit").
;;; When the readiness summary is incomplete, or some owned gitrepo
;;; has entries staged already, the server asks for confirmation
;;; instead and changes nothing; only the typed word "yes" resends the
;;; request confirmed. Every answer is followed by a rerender
;;; stream, which after a commit redraws the open buffers of the
;;; diff view.
;;;
;;; DATA USED/ASSUMED: See docs/api-and-formats.org.

(require 'skg-buffer)
(require 'skg-readiness) ; for skg--readiness-summary-message
(require 'skg-request-rerender-all-views)

(defun skg-stage-and-commit ()
  "Stage every change and git-commit it, in every owned skgrepo
that has changes, with one message. If not every changed file is
commit-ready, or some owned gitrepo has entries staged already,
show the readiness summary and those entries, and proceed only if
the user types \"yes\"."
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
  (skg--send-streaming-request
   "stage and commit"
   'stage-and-commit
   (append `((request . "stage and commit")
             (message . ,commit-message))
           (when confirmed '((confirmed . t))))
   (lambda (_tcp payload)
     (skg--stage-and-commit-handler commit-message payload))
   nil t))

(defun skg--stage-and-commit-handler (commit-message payload)
  "Handle the stage-and-commit response PAYLOAD. On a request for
confirmation, ask for it, and on \"yes\" resend COMMIT-MESSAGE
confirmed once the empty rerender stream that follows is done."
  (condition-case err
      (let ((response (read payload)))
        (if (assq 'needs-confirmation response)
            (when (skg--stage-and-commit-confirmed-p response)
              (setq skg--rerender-after-empty-stream
                    (lambda ()
                      (skg--stage-and-commit-send commit-message t))))
          (message "%s" (skg--stage-and-commit-result-message response))))
    (error (message "skg: stage-and-commit handler error: %S" err))))

(defun skg--stage-and-commit-confirmed-p (response)
  "Show the readiness summary of RESPONSE, a request for confirmation,
and the entries it lists as staged already; non-nil iff the user
types exactly \"yes\"."
  (string= "yes"
           (read-string
            (format "%s%s. Type yes to commit anyway: "
                    (string-remove-suffix
                     "." (skg--readiness-summary-message
                          (assq 'readiness-summary response)))
                    (skg--stage-and-commit-pre-staged-message
                     (cadr (assq 'pre-staged response)))))))

(defun skg--stage-and-commit-pre-staged-message (pre-staged)
  "The prompt's clause for PRE-STAGED, '((GITREPO PATH...) ...)', or
the empty string when there is none."
  (if (null pre-staged) ""
    (format ". Staged already, so committed too: %s"
            (mapconcat
             (lambda (gitrepo-and-paths)
               (format "%s (%s)"
                       (car gitrepo-and-paths)
                       (mapconcat (lambda (path) (format "%s" path))
                                  (cdr gitrepo-and-paths) ", ")))
             pre-staged "; "))))

(defun skg--stage-and-commit-result-message (response)
  "The line shown for RESPONSE, '((commits ((SKGREPO COMMIT-ID) ...))
\(errors (...)) (warnings (...)))'."
  (let ((commits (cadr (assq 'commits response)))
        (errors-line (skg--response-errors-line response)))
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
     (if errors-line
         (format ". Errors: %s" errors-line)
       "."))))

(provide 'skg-request-stage-and-commit)
