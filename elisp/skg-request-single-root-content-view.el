;;; -*- lexical-binding: t; -*-
;;;
;;; DATA USED/ASSUMED: See /api.md.

(require 'skg-log)
(require 'skg-length-prefix)
(require 'skg-buffer)
(require 'skg-request-save) ; For message formatting/display helpers

(defun skg--single-root-view-request-string
    (clean-id view-uri bypass-override &optional approved-pids fresh-view-p)
  "The request sexp string for a single root content view of CLEAN-ID.
When BYPASS-OVERRIDE is non-nil, the request carries
\(override-choice . \"bypass\")."
  (concat (prin1-to-string
           (append
            `((request . "single root content view")
              (id . ,clean-id)
              (view-uri . ,view-uri))
            (when bypass-override
              '((override-choice . "bypass")))
            (when approved-pids
              `((allow-ugly-telescopes ,@approved-pids)))
            (when fresh-view-p '((fresh-view . "true")))))
          "\n"))

(defun skg-request-single-root-content-view-from-id
    (node-id &optional tcp-proc bypass-override approved-pids view-uri
             stale-uri-retry-p fresh-view-p)
  "Ask Rust for an single root content view view of NODE-ID.
Registers a response handler in the dispatch map.
Optional TCP-PROC allows reusing an existing connection.
When BYPASS-OVERRIDE is non-nil, the request carries
\(override-choice . \"bypass\"): if NODE-ID is overridden, the
server opens the node itself instead of the override-choice menu.
\(Recursive content beneath the root still substitutes.)
APPROVED-PIDS and VIEW-URI preserve an ugly-telescope approval retry.
STALE-URI-RETRY-P is an internal guard that prevents repeated recovery.
FRESH-VIEW-P asks the server not to redirect to an already-open root."
  (interactive "sNode ID: ")
  (let* ((tcp-proc (or tcp-proc (skg-tcp-connect-to-rust)))
         (view-uri (or view-uri (org-id-uuid)))
         (clean-id (if (stringp node-id)
                       (substring-no-properties node-id)
                     node-id))
         (request-s-exp
          (skg--single-root-view-request-string
           clean-id view-uri bypass-override approved-pids fresh-view-p)))
    ;; Register handler in dispatch map (one-shot)
    (skg-register-response-handler
     'content-view
     (lambda (tcp-proc payload)
       (skg-remove-response-handler 'ugly-telescope-confirmation)
       (skg-handle-content-view-sexp
        tcp-proc payload view-uri clean-id bypass-override approved-pids
        stale-uri-retry-p fresh-view-p))
     t)
    ;; Alternative to content-view. Keep it non-one-shot so only the
    ;; content-view branch contributes to the pending-response count.
    (skg-register-response-handler
     'ugly-telescope-confirmation
     (lambda (tcp-proc payload)
       (skg-remove-response-handler 'ugly-telescope-confirmation)
       (skg-remove-response-handler 'content-view)
       (let* ((response (read payload))
              (prompt (format "%s" (cadr (assoc 'prompt response))))
              (pids (mapcar (lambda (pid) (format "%s" pid))
                            (cadr (assoc 'pids response)))))
         (when (y-or-n-p (concat prompt " "))
           (if fresh-view-p
               (skg-request-single-root-content-view-from-id
                clean-id tcp-proc bypass-override pids view-uri
                stale-uri-retry-p t)
             (skg-request-single-root-content-view-from-id
              clean-id tcp-proc bypass-override pids view-uri
              stale-uri-retry-p)))))
     nil)
    (skg-submit-request tcp-proc request-s-exp)) )

(defun skg--finish-switchToContentView
    (tcp-proc switch-uri node-id bypass-override approved-pids
              stale-uri-retry-p fresh-view-p)
  "Display SWITCH-URI, or repair stale server bookkeeping once."
  (let ((buf (skg-find-buffer-by-uri switch-uri)))
    (cond
     ((not buf)
      (skg-log 'warn 'view
               "server said switch to view %s, but no buffer found"
               switch-uri)
      (if stale-uri-retry-p
          (message
           "skg: could not visit %s: server twice returned a missing view (%s)"
           node-id switch-uri)
        (skg-send-close-view-uri tcp-proc switch-uri)
        (if fresh-view-p
            (skg-request-single-root-content-view-from-id
             node-id tcp-proc bypass-override approved-pids nil t t)
          (skg-request-single-root-content-view-from-id
           node-id tcp-proc bypass-override approved-pids nil t))))
     ((eq buf (current-buffer))
      (message "Already viewing this node (it is a root of this view)"))
     (t (pop-to-buffer buf)))))

(defun skg--defer-switch-to-content-view
    (tcp-proc switch-uri node-id bypass-override approved-pids
              stale-uri-retry-p fresh-view-p)
  "Handle a switch response outside the network process filter."
  (run-at-time
   0 nil #'skg--finish-switchToContentView
   tcp-proc switch-uri node-id bypass-override approved-pids
   stale-uri-retry-p fresh-view-p))

(defun skg-handle-content-view-sexp
    (tcp-proc sexp-string view-uri
              &optional node-id bypass-override approved-pids
              stale-uri-retry-p fresh-view-p)
  "Parse and handle content view response s-exp.
Expected shape: ((content ...) (errors ...) (warnings ...)).
If the server returns ((switch-to-view URI)) instead, switch to the
existing buffer for that view rather than opening a new one.
VIEW-URI is the pre-generated UUID to assign to the new buffer.
NODE-ID, BYPASS-OVERRIDE, and APPROVED-PIDS reproduce the request during
one stale-URI recovery attempt; STALE-URI-RETRY-P prevents an infinite
retry."
  (condition-case err
      (let* ((response (read sexp-string))
             (_session (skg-require-current-server-session response))
             (switch-uri (cadr (assoc 'switch-to-view response))))
        (if switch-uri
            ;; The requested ID is already a root of an open view.
            (let ((target (skg-find-buffer-by-uri (format "%s" switch-uri))))
              (when (buffer-live-p target)
                (let ((record (buffer-local-value
                               'skg--buffer-record target)))
                  (unless record
                    (error "Skg refuses to switch to an unregistered view"))
                  (skg-require-current-server-session response record)))
              (skg--defer-switch-to-content-view
               tcp-proc (format "%s" switch-uri) node-id bypass-override
               approved-pids stale-uri-retry-p fresh-view-p))
          ;; Normal content view response.
          (let* ((content-value (cadr (assoc 'content response)))
                 (errors-list (cadr (assoc 'errors response)))
                 (warnings-list (cadr (assoc 'warnings response)))
                 (server-uri ;; The server may override the client-generated URI; it does for override-choice menus, registered under "override-menu:PID". PITFALL: the server's sexp printer leaves space-free strings unquoted, so this can arrive as a symbol; normalize to a string.
                  (let ((u (cadr (assoc 'view-uri response))))
                    (when u (format "%s" u))))
                 (effective-uri (or server-uri view-uri))
                 (authority (skg--view-authority-from-response response))
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
                 tcp-proc content-value buf-name effective-uri
                 (if (string-prefix-p "override-menu:" effective-uri)
                     'override-choice-menu
                   'content-view)
                 `((kind . "single-root") (root-id . ,node-id))
                 authority)))
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
