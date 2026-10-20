;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Ask the server what an unknown node is: the node its ID
;;; resolves to, if any, or else what the git history of every
;;; skgrepo says it used to be. Shows the answer, Org text, in a
;;; read-only buffer.
;;;
;;; DATA USED/ASSUMED: See docs/api-and-formats.org,
;;; "Investigate unknown node".

(require 'skg-length-prefix)
(require 'skg-metadata) ; for skg--unknown-node-id-at-point
(require 'skg-request-save) ; for skg-big-nonfatal-message, skg-report-mode

(defun skg-investigate-unknown-node ()
  "Show what git history says about an unknown node.
The ID is the one of the unknown headline at point; elsewhere, it is
read from the minibuffer."
  (interactive)
  (let ((skgid (or (skg--unknown-node-id-at-point)
                   (read-string "Investigate ID: ")))
        (tcp-proc (skg-tcp-connect-to-rust)))
    (when (string-empty-p skgid)
      (user-error "No ID to investigate"))
    (skg-register-response-handler
     'investigate-unknown-node
     #'skg--investigate-unknown-node-handler
     t)
    (skg-lp-reset)
    (process-send-string
     tcp-proc
     (skg--investigate-unknown-node-request-string skgid))))

(defun skg--investigate-unknown-node-request-string (skgid)
  "The request sexp string investigating SKGID."
  (concat (prin1-to-string
           `((request . "investigate unknown node")
             (id . ,skgid)))
          "\n"))

(defun skg--investigate-unknown-node-handler (_tcp-proc payload)
  "Show an investigate-unknown-node response PAYLOAD."
  (condition-case err
      (let* ((response (read payload))
             (content (cadr (assoc 'content response)))
             (errors-list (cadr (assoc 'errors response)))
             (buffer
              (skg-big-nonfatal-message
               "*skg unknown node*"
               (if (skg--message-list-nonempty-p errors-list)
                   "The investigation failed"
                 "Investigation complete")
               (or content "* investigation failed\n** Empty response\n"))))
        (with-current-buffer buffer
          (skg-report-mode 1)
          (read-only-mode 1)))
    (error
     (message "skg: investigate-unknown-node handler error: %S" err))))

(provide 'skg-request-investigate-unknown-node)
