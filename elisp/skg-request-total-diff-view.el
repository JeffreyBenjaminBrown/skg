;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: The total diff view
;;; ('TODO/total-diff-view/2_plan.org', "One command, four buffers").
;;; One command sends one "total diff view" request per buffer, in
;;; order, and opens each buffer the server does not answer with
;;; '(empty . t)', named "diff-view: " followed by the buffer's name.
;;; Each is an ordinary registered content view.
;;;
;;; DATA USED/ASSUMED: See docs/api-and-formats.org.

(require 'org-id)
(require 'skg-log)
(require 'skg-length-prefix)
(require 'skg-buffer)
(require 'skg-request-save) ; For message formatting/display helpers

(defconst skg-total-diff-view-buffers
  '("all changes" "merges" "skgrepo changes" "order-only changes")
  "The buffers of the total diff view, in the order they open.")

(defun skg--total-diff-view-request-string (buffer view-id)
  "The request sexp string for the total diff view's BUFFER."
  (concat (prin1-to-string
           `((request . "total diff view")
             (buffer . ,buffer)
             (view-id . ,view-id)))
          "\n"))

(defun skg-diff-view (&optional tcp-proc)
  "Open the total diff view: every change in the owned skgrepos
since their last commits, in up to four buffers. Buffers that would
be empty are not opened."
  (interactive)
  (skg--request-total-diff-view-buffers
   (or tcp-proc (skg-tcp-connect-to-rust))
   skg-total-diff-view-buffers
   0))

(defun skg--request-total-diff-view-buffers (tcp-proc buffers opened)
  "Request the first of BUFFERS over TCP-PROC. Its answer opens it,
unless empty, and then requests the rest. OPENED counts the buffers
opened so far."
  (if (null buffers)
      (when (= opened 0)
        (message "The total diff view is empty: nothing changed since the last commits."))
    (let ((buffer (car buffers))
          (view-id (org-id-uuid)))
      (skg-register-response-handler
       'total-diff-view
       (lambda (tcp-proc payload)
         (let ((outcome
                (skg--handle-total-diff-view-response
                 tcp-proc payload buffer view-id)))
           ;; A refusal applies to every buffer, so it ends the
           ;; sequence. The dispatcher drops this one-shot handler
           ;; after it returns, so the next request is sent afterward.
           (unless (eq outcome 'failed)
             (run-at-time
              0 nil #'skg--request-total-diff-view-buffers
              tcp-proc (cdr buffers)
              (if (eq outcome 'opened) (1+ opened) opened)))))
       t)
      (skg-lp-reset)
      (process-send-string
       tcp-proc (skg--total-diff-view-request-string buffer view-id)))))

(defun skg--handle-total-diff-view-response (tcp-proc payload buffer view-id)
  "Open the total diff view's BUFFER from PAYLOAD under VIEW-ID,
unless the server answered '(empty . t)'. Shows any errors and
warnings. Returns 'opened, 'empty, or 'failed (no buffer, and
errors)."
  (condition-case err
      (let* ((response (read payload))
             (empty (cdr (assoc 'empty response)))
             (content (cadr (assoc 'content response)))
             (errors-list (cadr (assoc 'errors response)))
             (warnings-list (cadr (assoc 'warnings response)))
             (opened (and (not empty)
                          content
                          (not (string= content "")))))
        (when opened
          (skg-open-org-buffer-from-text
           tcp-proc content (concat "diff-view: " buffer) view-id))
        (when (or (skg--message-list-nonempty-p errors-list)
                  (skg--message-list-nonempty-p warnings-list))
          (skg-big-nonfatal-message
           "*SKG Diff View Messages*"
           (format "The total diff view's \"%s\" buffer reported problems"
                   buffer)
           (skg-errors-and-warnings-to-org-string
            errors-list warnings-list)))
        (cond (opened 'opened)
              (empty 'empty)
              ((skg--message-list-nonempty-p errors-list) 'failed)
              (t 'empty)))
    (error
     (skg-log 'error 'view "parsing total diff view response: %S" err)
     'failed)))

(provide 'skg-request-total-diff-view)
