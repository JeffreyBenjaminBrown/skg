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
         (view-uri (org-id-uuid))
         (clean-id (if (stringp node-id)
                       (substring-no-properties node-id)
                     node-id))
         (request-s-exp
          (concat (prin1-to-string
                   `((request . "single root content view")
                     (id . ,clean-id)
                     (view-uri . ,view-uri)))
                  "\n")))
    (setq ;; Prepare LP state and handler
     skg-lp--buf               (unibyte-string) ;; empty string
     skg-lp--bytes-left        nil
     skg-doc--response-handler
     (lambda (tcp-proc chunk)
       (skg-lp-handle-generic-chunk
        (lambda (tcp-proc payload)
          (skg-handle-content-view-sexp tcp-proc payload view-uri))
         tcp-proc chunk)))
    (process-send-string tcp-proc request-s-exp)) )

(defun skg-handle-content-view-sexp (tcp-proc sexp-string view-uri)
  "Parse and handle content view response s-exp: ((content ...) (errors ...)).
VIEW-URI is the pre-generated UUID to assign to the new buffer."
  (condition-case err
      (let* ((response (read sexp-string))
             (content-value (cadr (assoc 'content response)))
             (errors-list (cadr (assoc 'errors response))))
        (when content-value ;; If content is not nil, open the buffer
          (skg-open-org-buffer-from-text
           tcp-proc content-value (skg-content-view-buffer-name
                                   content-value)
           view-uri))
        (when ;; If there are errors, show them
            (and errors-list (not (equal errors-list nil)))
          (let ((errors-text (if (listp errors-list)
                                 (mapconcat 'identity errors-list "\n\n")
                               errors-list)))
            (skg-show-save-warnings errors-text))))
    (error
     (message "ERROR parsing content view response: %S" err)
     (message "Sexp string was: %S" sexp-string))))

(provide 'skg-request-single-root-content-view)
