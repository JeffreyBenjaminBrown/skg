;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Stage every change and git-commit it, in every owned
;;; skgrepo that has changes, with one message
;;; ('docs/diff-view.org', "Stage and commit").
;;; When the readiness summary is incomplete, or some owned gitrepo
;;; has entries already staged, the server asks for confirmation
;;; instead and changes nothing; only the typed word "yes" resends the
;;; request confirmed. Every answer is followed by a rerender
;;; stream, which after a commit rerenders the open buffers of the
;;; diff view.
;;;
;;; DATA USED/ASSUMED: See docs/api-and-formats.org.

(require 'skg-buffer)
(require 'skg-readiness) ; for skg--readiness-summary-message
(require 'skg-request-save) ; for skg-big-nonfatal-message
(require 'skg-request-rerender-all-views)

(defun skg-stage-and-commit ()
  "Stage every change and git-commit it, in every owned skgrepo
that has changes, with one message. If not every changed file is
commit-ready, or some owned gitrepo has entries already staged,
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
CONFIRMED. The message goes one string per line, as
'(message-lines \"LINE\" ...)', since the server reads one request
per line."
  (skg--send-streaming-request
   "stage and commit"
   'stage-and-commit
   (append `((request . "stage and commit")
             (message-lines ,@(split-string commit-message "\n")))
           (when confirmed '((confirmed . t))))
   (lambda (_tcp payload)
     (skg--stage-and-commit-handler commit-message payload))
   nil t))

(defun skg--stage-and-commit-handler (commit-message payload)
  "Handle the stage-and-commit response PAYLOAD. A request for
confirmation is asked about once the empty rerender stream that
follows is done, and \"yes\" resends COMMIT-MESSAGE confirmed."
  (condition-case err
      (let ((response (read payload)))
        (if (assq 'needs-confirmation response)
            ;; PITFALL: asking here, inside the process filter, would let
            ;; a quit at the prompt escape the filter with the rest of the
            ;; stream unread, leaving the stream in progress.
            (setq skg--rerender-after-empty-stream
                  (lambda ()
                    (skg--stage-and-commit-confirm-and-resend
                     commit-message response)))
          (message "%s" (skg--stage-and-commit-result-message response))))
    (error (message "skg: stage-and-commit handler error: %S" err))))

(defconst skg--stage-and-commit-already-staged-buffer-name
  "*skg already staged*"
  "The buffer listing every path already staged, by gitrepo, when the
server asks to confirm a commit that would include them.")

(defun skg--stage-and-commit-confirm-and-resend (commit-message response)
  "Ask to confirm RESPONSE, a request for confirmation, and on \"yes\"
resend COMMIT-MESSAGE confirmed. Otherwise, a quit at the prompt
included, say that nothing was committed."
  (if (condition-case nil
          (skg--stage-and-commit-confirmed-p response)
        (quit nil))
      (skg--stage-and-commit-send commit-message t)
    (message "Not committed.")))

(defun skg--stage-and-commit-confirmed-p (response)
  "Show the readiness summary of RESPONSE, a request for confirmation,
and how many entries it lists as already staged in each gitrepo, the
entries themselves in the buffer
`skg--stage-and-commit-already-staged-buffer-name'; non-nil iff the
user types exactly \"yes\"."
  (let ((already-staged (cadr (assq 'already-staged response))))
    (when already-staged
      (skg-big-nonfatal-message
       skg--stage-and-commit-already-staged-buffer-name ""
       (skg--stage-and-commit-already-staged-listing already-staged)))
    (string= "yes"
             (read-string
              (format "%s%s. Type yes to commit anyway: "
                      (string-remove-suffix
                       "." (skg--readiness-summary-message
                            (assq 'readiness-summary response)))
                      (skg--stage-and-commit-already-staged-message
                       already-staged))))))

(defun skg--stage-and-commit-already-staged-message (already-staged)
  "The prompt's clause for ALREADY-STAGED, '((GITREPO PATH...) ...)':
how many paths each gitrepo has staged, and where they are listed; or
the empty string when there is none."
  (if (null already-staged) ""
    (format ". Already staged, so committed too: %s (listed in %s)"
            (mapconcat
             (lambda (gitrepo-and-paths)
               (let ((count (length (cdr gitrepo-and-paths))))
                 (format "%s: %d %s" (car gitrepo-and-paths) count
                         (if (= count 1) "path" "paths"))))
             already-staged ", ")
            skg--stage-and-commit-already-staged-buffer-name)))

(defun skg--stage-and-commit-already-staged-listing (already-staged)
  "ALREADY-STAGED, '((GITREPO PATH...) ...)', as org text: a headline per
gitrepo, then its paths, one per line."
  (mapconcat
   (lambda (gitrepo-and-paths)
     (concat (format "* %s\n" (car gitrepo-and-paths))
             (mapconcat (lambda (path) (format "%s\n" path))
                        (cdr gitrepo-and-paths) "")))
   already-staged ""))

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
