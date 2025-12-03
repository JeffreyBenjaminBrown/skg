;;; -*- lexical-binding: t; -*-
;;;
;;; USER-FACING FUNCTIONS
;;;   skg-request-single-root-content-view-from-id
;;;
;;; DATA USED/ASSUMED: See /api.md.

(require 'skg-length-prefix)
(require 'skg-buffer)
(require 'skg-request-save) ; For skg-show-save-errors and skg-show-save-warnings

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
          (skg-handle-content-view-sexp tcp-proc payload))
         tcp-proc chunk)))
    (process-send-string tcp-proc request-s-exp)) )

(defun skg-handle-content-view-sexp (tcp-proc sexp-string)
  "Parse and handle content view response s-exp: ((content ...) (errors ...))."
  (condition-case err
      (let* ((response (read sexp-string))
             (content-pair (assoc 'content response))
             (errors-pair (assoc 'errors response))
             (content-value (cadr content-pair))
             (errors-list (cadr errors-pair)))
        ;; If content is not nil, open the buffer
        (when content-value
          (skg-open-org-buffer-from-text
           tcp-proc content-value skg-content-view-buffer-name))
        ;; If there are errors, show them
        (when (and errors-list (not (equal errors-list nil)))
          (let ((errors-text (if (listp errors-list)
                                 (mapconcat 'identity errors-list "\n\n")
                               errors-list)))
            (skg-show-save-warnings errors-text))))
    (error
     (message "ERROR parsing content view response: %S" err)
     (message "Sexp string was: %S" sexp-string))))

(provide 'skg-request-single-root-content-view)
