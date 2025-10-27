;;; -*- lexical-binding: t; -*-
;; DATA USED/ASSUMED: See /api.md.

(require 'skg-length-prefix)
(require 'skg-buffer)

(defun skg-request-single-root-content-view-from-id (node-id &optional tcp-proc)
  "Ask Rust for an single root content view view of NODE-ID.
Installs a length-prefixed response handler.
Optional TCP-PROC allows reusing an existing connection."
  (interactive "sNode ID: ")
  (let* ((tcp-proc (or tcp-proc (skg-tcp-connect-to-rust)))
         (request-s-exp
          (concat (prin1-to-string
                   `((request . "single root content view")
                     (id . ,node-id)))
                  "\n")))
    (setq ;; Prepare LP state and handler
     skg-lp--buf               (unibyte-string) ;; empty string
     skg-lp--bytes-left        nil
     skg-doc--response-handler
     (lambda (tcp-proc chunk)
       (skg-lp-handle-generic-chunk
        (lambda (tcp-proc payload)
          (skg-open-org-buffer-from-text
           tcp-proc payload skg-content-view-buffer-name))
         tcp-proc chunk)))
    (process-send-string tcp-proc request-s-exp)) )

(provide 'skg-request-single-root-content-view)
