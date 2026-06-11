;;; -*- lexical-binding: t; -*-
;;;
;;; DATA USED/ASSUMED: See /api.md.

(require 'skg-log)
(require 'skg-length-prefix)
(require 'skg-buffer)
(require 'skg-request-save) ; For message formatting/display helpers

(defun skg--single-root-view-request-string (clean-id view-uri bypass-override)
  "The request sexp string for a single root content view of CLEAN-ID.
When BYPASS-OVERRIDE is non-nil, the request carries
\(override-choice . \"bypass\")."
  (concat (prin1-to-string
           (append
            `((request . "single root content view")
              (id . ,clean-id)
              (view-uri . ,view-uri))
            (when bypass-override
              '((override-choice . "bypass")))))
          "\n"))

(defun skg-request-single-root-content-view-from-id (node-id &optional tcp-proc bypass-override)
  "Ask Rust for an single root content view view of NODE-ID.
Registers a response handler in the dispatch map.
Optional TCP-PROC allows reusing an existing connection.
When BYPASS-OVERRIDE is non-nil, the request carries
\(override-choice . \"bypass\"): if NODE-ID is overridden, the
server opens the node itself instead of the override-choice menu.
\(Recursive content beneath the root still substitutes.)"
  (interactive "sNode ID: ")
  (let* ((tcp-proc (or tcp-proc (skg-tcp-connect-to-rust)))
         (view-uri (org-id-uuid))
         (clean-id (if (stringp node-id)
                       (substring-no-properties node-id)
                     node-id))
         (request-s-exp
          (skg--single-root-view-request-string
           clean-id view-uri bypass-override)))
    ;; Register handler in dispatch map (one-shot)
    (skg-register-response-handler
     'content-view
     (lambda (tcp-proc payload)
       (skg-handle-content-view-sexp tcp-proc payload view-uri))
     t)
    (skg-lp-reset)
    (process-send-string tcp-proc request-s-exp)) )

(defun skg-handle-content-view-sexp (tcp-proc sexp-string view-uri)
  "Parse and handle content view response s-exp.
Expected shape: ((content ...) (errors ...) (warnings ...)).
If the server returns ((switch-to-view URI)) instead, switch to the
existing buffer for that view rather than opening a new one.
VIEW-URI is the pre-generated UUID to assign to the new buffer."
  (condition-case err
      (let* ((response (read sexp-string))
             (switch-uri (cadr (assoc 'switch-to-view response))))
        (if switch-uri
            ;; The requested ID is already a root of an open view.
            (let ((buf (skg-find-buffer-by-uri switch-uri)))
              (cond
               ((not buf)
                (skg-log 'warn 'view "server said switch to view %s, but no buffer found" switch-uri))
               ((eq buf (current-buffer))
                (message "Already viewing this node (it is a root of this view)"))
               (t (switch-to-buffer buf))))
          ;; Normal content view response.
          (let* ((content-value (cadr (assoc 'content response)))
                 (errors-list (cadr (assoc 'errors response)))
                 (warnings-list (cadr (assoc 'warnings response)))
                 (server-uri ;; The server may override the client-generated URI; it does for override-choice menus, registered under "override-menu:PID". PITFALL: the server's sexp printer leaves space-free strings unquoted, so this can arrive as a symbol; normalize to a string.
                  (let ((u (cadr (assoc 'view-uri response))))
                    (when u (format "%s" u))))
                 (effective-uri (or server-uri view-uri))
                 (to-minibuffer ;; Optional one-line echo: minibuffer only, never buffer text, never a popped window.
                  (cadr (assoc 'to-minibuffer response)))
                 (has-errors (skg--message-list-nonempty-p errors-list))
                 (has-warnings (skg--message-list-nonempty-p warnings-list))
                 (has-content (and content-value
                                   (not (string= content-value "")))))
            (when has-content
              (let ((buf-name (skg-content-view-buffer-name
                               content-value)))
                (skg-open-org-buffer-from-text
                 tcp-proc content-value buf-name effective-uri)))
            (when to-minibuffer
              (message "%s" to-minibuffer))
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
