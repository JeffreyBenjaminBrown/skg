;;; -*- lexical-binding: t; -*-
;;;
;;; DATA USED/ASSUMED: See docs/api-and-formats.org.

(require 'skg-log)
(require 'skg-length-prefix)
(require 'skg-buffer)
(require 'skg-messages) ; For message formatting/display helpers

(defun skg--single-root-view-request-string (clean-gnid view-id
                                                      &optional approved-pids)
  "The request sexp string for a single root content view of CLEAN-ID."
  (concat (prin1-to-string
           (append
            `((request . "single root content view")
              (id . ,clean-gnid)
              (view-id . ,view-id))
            (when approved-pids
              `((approved-overPrivateText-pids ,@approved-pids)))))
          "\n"))

(defun skg-request-single-root-content-view-from-id
    (node-gnid &optional tcp-proc approved-pids view-id
             stale-view-id-retry-p)
  "Ask Rust for a single-root content view of NODE-ID.
Registers a response handler in the dispatch map.
Optional TCP-PROC allows reusing an existing connection.
The server opens NODE-ID itself; recursive content beneath the root may
still substitute.
APPROVED-PIDS and VIEW-ID preserve an overPrivateText-telescope approval retry.
STALE-VIEW-ID-RETRY-P is an internal guard that prevents repeated recovery."
  (interactive "sNode ID: ")
  (let* ((tcp-proc (or tcp-proc (skg-tcp-connect-to-rust)))
         (view-id (or view-id (org-id-uuid)))
         (clean-gnid (if (stringp node-gnid)
                       (substring-no-properties node-gnid)
                     node-gnid))
         (request-s-exp
          (skg--single-root-view-request-string
           clean-gnid view-id approved-pids)))
    ;; Register handler in dispatch map (one-shot)
    (skg-register-response-handler
     'content-view
     (lambda (tcp-proc payload)
       (skg--unregister-response-handlers '(overPrivateText-telescope-confirmation))
       (skg-handle-content-view-sexp
        tcp-proc payload view-id clean-gnid approved-pids
        stale-view-id-retry-p))
     t)
    (skg--register-overPrivateText-confirmation-instead-of
     'content-view
     (lambda (tcp-proc pids) ;; the connection that brought the confirmation
       (skg-request-single-root-content-view-from-id
        clean-gnid tcp-proc pids view-id
        stale-view-id-retry-p)))
    (skg-lp-reset)
    (process-send-string tcp-proc request-s-exp)) )

(defun skg--finish-switchToContentView
    (tcp-proc switch-view-id node-gnid approved-pids
              stale-view-id-retry-p)
  "Display SWITCH-VIEW-ID, or repair stale server bookkeeping once."
  (let ((buf (skg-find-buffer-by-view-id switch-view-id)))
    (cond
     ((not buf)
      (skg-log 'warn 'view
               "server said switch to view %s, but no buffer found"
               switch-view-id)
      (if stale-view-id-retry-p
          (message
           "skg: could not visit %s: server twice returned a missing view (%s)"
           node-gnid switch-view-id)
        (skg-send-close-view-id tcp-proc switch-view-id)
        (skg-request-single-root-content-view-from-id
         node-gnid tcp-proc approved-pids nil t)))
     ((eq buf (current-buffer))
      (message "Already viewing this node (it is a root of this view)"))
     (t (pop-to-buffer buf)))))

(defun skg--defer-switch-to-content-view
    (tcp-proc switch-view-id node-gnid approved-pids
              stale-view-id-retry-p)
  "Handle a switch response outside the network process filter."
  (run-at-time
   0 nil #'skg--finish-switchToContentView
   tcp-proc switch-view-id node-gnid approved-pids
   stale-view-id-retry-p))

(defun skg-handle-content-view-sexp
    (tcp-proc sexp-string view-id
              &optional node-gnid approved-pids
              stale-view-id-retry-p)
  "Parse and handle content view response s-exp.
Expected shape: ((content ...) (errors ...) (warnings ...)).
If the server returns ((switch-to-view ID)) instead, switch to the
existing buffer for that view rather than opening a new one.
VIEW-ID is the pre-generated UUID to assign to the new buffer.
NODE-ID and APPROVED-PIDS reproduce the request during
one stale-view-ID recovery attempt; STALE-VIEW-ID-RETRY-P prevents an infinite
retry."
  (condition-case err
      (let* ((response (read sexp-string))
             (switch-view-id (cadr (assoc 'switch-to-view response))))
        (if switch-view-id
            ;; The requested ID is already a root of an open view.
            (skg--defer-switch-to-content-view
             tcp-proc (format "%s" switch-view-id) node-gnid approved-pids
             stale-view-id-retry-p)
          ;; Normal content view response.
          (let* ((content-value (cadr (assoc 'content response)))
                 (errors-list (cadr (assoc 'errors response)))
                 (warnings-list (cadr (assoc 'warnings response)))
                 (has-errors (skg--message-list-nonempty-p errors-list))
                 (has-warnings (skg--message-list-nonempty-p warnings-list))
                 (has-content (and content-value
                                   (not (string= content-value "")))))
            (when has-content
              (let ((buf-name (skg-content-view-buffer-name
                               content-value)))
                (skg-open-org-buffer-from-text
                 tcp-proc content-value buf-name view-id)))
            (when (or has-errors has-warnings)
              (skg-big-nonfatal-message
               "*SKG Content View Messages*"
               (cond
                ((and has-errors has-warnings)
                 "Content view reported errors and warnings")
                (has-errors
                 "Content view failed")
                (t
                 "Content view completed with warnings"))
               (skg-errors-and-warnings-to-org-string
                errors-list warnings-list))))))
    (error
     (message "skg content view error: %S" err)
     (skg-log 'error 'view "parsing content view response: %S" err)
     (skg-log 'error 'view "sexp string was: %S" sexp-string))))

(provide 'skg-request-single-root-content-view)
