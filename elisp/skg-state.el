;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Global state variables for the skg client.
;;;
;;; TODO: Can these globals be avoided?

(require 'cl-lib)
(require 'org-id)
(require 'skg-log)

(defvar skg-rust-tcp-proc nil
  "Persistent TCP connection to the Rust backend. See
https://www.gnu.org/software/emacs/manual/html_node/elisp/Network-Processes.html")

(defvar skg--client-session-id
  (format "emacs-%d-%s" (emacs-pid) (org-id-uuid))
  "Stable identity of this Emacs process across Skg reconnects and code reloads.")

(defvar skg--connection-handshake-state nil
  "Progress of the mandatory handshake for the current TCP connection.
The value is nil, `sent', `census', `census-texts', `verified',
`busy-initializing', or `failed'.")

(defvar skg--connection-handshake-error nil
  "Exact server or transport error which prevented connection verification.")

(defvar skg--server-session-id nil
  "Server instance identity for the current verified TCP connection.")

(defvar skg--connection-busy-message nil
  "Server status reported by the exceptional busy-initializing signal.")

(defvar skg--active-source-set-name "server-default"
  "Name claimed in a reconnect handshake; replaced by server authority.")

(defvar skg--maintenance-archive-folder nil)
(defvar skg--maintenance-archive-identity nil)
(defvar skg--maintenance-state nil)

(defvar skg--rebuilding nil
  "Non-nil exactly while server metadata says a graph/search pair is rebuilding.")

(defvar skg--graph-write-admission nil
  "Current server admission for new editable views: `open', `closed', or nil.")

(defvar skg--graph-transition-status nil
  "Current server graph transition status, independent of incident reports.")

(defvar skg--pending-incidents nil
  "Compact server-reported maintenance incident summaries.")

(defvar skg--pending-query-waits nil
  "Verified coarse summaries of durable query waits from the server.")

(defvar skg--client-constructor-admission 'open
  "Whether new editable client view constructors may join maintenance census.")

(defvar skg--owner-publication-revision nil
  "Newest owner publication revision accepted for the current server session.")

(defun skg-update-rebuilding-status (response)
  "Update the rebuilding flag when RESPONSE explicitly carries that status.
An omitted field preserves the last authoritative value."
  (when-let ((entry (assq 'rebuilding response)))
    (let ((value (cadr entry)))
      (cond
       ((memq value '(t true))
        (setq skg--rebuilding t))
       ((null value)
        (setq skg--rebuilding nil))
       ((equal value "true")
        (setq skg--rebuilding t))
       ((equal value "nil")
        (setq skg--rebuilding nil))
       (t
        (error "Invalid rebuilding status %S" value))))
    (force-mode-line-update t))
  skg--rebuilding)

(defun skg-update-global-server-status (response)
  "Install explicit process-wide graph status from current-session RESPONSE.
Report-local selected-* fields never update the current graph identity."
  (when (cl-some (lambda (key) (assoc key response))
                 '(current-graph-generation current-manifest-revision
                   graph-write-admission graph-transition-status rebuilding
                   pending-incidents pending-query-waits
                   owner-publication-revision))
    (when (cl-some (lambda (key) (assoc key response))
                   '(current-graph-generation current-manifest-revision
                     graph-write-admission graph-transition-status
                     pending-incidents pending-query-waits
                     owner-publication-revision))
      (skg-require-current-server-session response))
    (let* ((revision-entry (assoc 'owner-publication-revision response))
           (revision (and revision-entry (cadr revision-entry))))
      (when revision-entry
        (unless (natnump revision)
          (error "Invalid owner publication revision %S" revision)))
      (unless (and revision-entry
                   skg--owner-publication-revision
                   (< revision skg--owner-publication-revision))
        (when revision-entry
          (setq skg--owner-publication-revision revision))
        (when-let ((entry (assoc 'graph-write-admission response)))
          (let ((value (intern (format "%s" (cadr entry)))))
            (unless (memq value '(open closed))
              (error "Invalid graph write admission %S" (cadr entry)))
            (setq skg--graph-write-admission value)
            ;; A published open graph reopens fresh editable views after a
            ;; census barrier. The pre-census `closing' state still lets
            ;; already serialized requests drain.
            (when (and (eq value 'open)
                       (eq skg--client-constructor-admission 'closed))
              (setq skg--client-constructor-admission 'open))))
        (when-let ((entry (assoc 'graph-transition-status response)))
          (setq skg--graph-transition-status (cadr entry)))
        (when-let ((entry (assoc 'pending-incidents response)))
          (setq skg--pending-incidents (cadr entry)))
        (when-let ((entry (assoc 'pending-query-waits response)))
          (setq skg--pending-query-waits (cadr entry))
          (when (fboundp 'skg-query-wait-ingest-pending)
            (skg-query-wait-ingest-pending skg--pending-query-waits)))
        (when-let ((entry (assoc 'current-graph-generation response)))
          (unless (natnump (cadr entry))
            (error "Invalid current graph generation %S" (cadr entry)))
          (setf (alist-get 'graph-generation skg--server-store-state)
                (cadr entry)))
        (when-let ((entry (assoc 'current-manifest-revision response)))
          (unless (natnump (cadr entry))
            (error "Invalid current manifest revision %S" (cadr entry)))
          (setf (alist-get 'manifest-revision skg--server-store-state)
                (cadr entry)))
        (skg-update-rebuilding-status response)))))

(defun skg-requested-view-write-authority ()
  "Return the authority a new view request may ask the server to grant."
  (pcase skg--graph-write-admission
    ('open (if (eq skg--client-constructor-admission 'open)
               'editable
             'read-only))
    ('closed 'read-only)
    ('closing 'read-only)
    (_ (error "Skg view admission is unavailable before verification"))))

(defun skg-require-client-constructor-admission ()
  "Refuse a new interactive constructor outside the writable boundary."
  (unless (eq skg--client-constructor-admission 'open)
    (user-error "Skg client constructor admission is closed")))

(defun skg-view-write-authority-from-response (response)
  "Return RESPONSE's mandatory view authority, rejecting obsolete grants."
  (let* ((entry (assoc 'view-write-authority response))
         (authority (and entry (intern (format "%s" (cadr entry))))))
    (unless (memq authority '(editable read-only))
      (error "Skg response omitted valid view-write-authority"))
    (when (and (eq authority 'editable)
               (or (eq skg--graph-write-admission 'closed)
                   (eq skg--client-constructor-admission 'closed)))
      (error "Skg refused editable view authority while admission is closed"))
    authority))

(cl-defstruct skg--request-record
  id incident-id handlers terminal-handler failure-handler finalizer finalized-p)

(defvar skg--request-records (make-hash-table :test #'equal)
  "Sent request records keyed by connection-local request ID.")
(defvar skg--request-draft nil
  "Request record collecting handlers before its operation is submitted.")
(defvar skg--request-queue nil
  "FIFO of complete request wire strings waiting behind the active request.")
(defvar skg--active-request-id nil)
(defvar skg--dispatching-request-id nil)
(defvar skg--next-request-number 0)

(defvar skg--server-push-handlers (make-hash-table :test #'equal)
  "Handlers for unsolicited server-owned operation frames, keyed by kind.")

(defun skg-register-server-push-handler (frame-kind handler)
  "Register HANDLER for unsolicited FRAME-KIND frames."
  (puthash (format "%s" frame-kind) handler skg--server-push-handlers))

(defun skg-remove-server-push-handler (frame-kind)
  "Stop handling unsolicited FRAME-KIND frames."
  (remhash (format "%s" frame-kind) skg--server-push-handlers))

(defun skg--fresh-request-id ()
  (format "emacs-%d-%d" (emacs-pid) (cl-incf skg--next-request-number)))

(defun skg--ensure-request-draft ()
  (or skg--request-draft
      (setq skg--request-draft
            (make-skg--request-record
             :id (skg--fresh-request-id) :handlers nil))))

(defun skg-fresh-incident-id ()
  "Return an opaque ID for one reconciliation episode."
  (format "incident-%s" (org-id-uuid)))

(defun skg-register-response-handler (frame-kind handler &optional one-shot)
  "Register HANDLER for FRAME-KIND on the request being constructed."
  (let* ((record (skg--ensure-request-draft))
         (handlers (skg--request-record-handlers record)))
    (setf (skg--request-record-handlers record)
          (cons (cons frame-kind (cons handler one-shot))
                (assoc-delete-all frame-kind handlers))))
  (when one-shot
    (setq skg-lp--pending-count (1+ skg-lp--pending-count))))

(defun skg--request-record-for-edit ()
  (or (and skg--dispatching-request-id
           (gethash skg--dispatching-request-id skg--request-records))
      skg--request-draft))

(defun skg-remove-response-handler (frame-kind)
  (when-let ((record (skg--request-record-for-edit)))
    (let ((entry (assoc frame-kind
                        (skg--request-record-handlers record))))
      (when (and entry (cddr entry))
        (setq skg-lp--pending-count
              (max 0 (1- skg-lp--pending-count))))
      (setf (skg--request-record-handlers record)
            (assoc-delete-all frame-kind
                              (skg--request-record-handlers record))))))

(defun skg-response-handler-registered-p (frame-kind)
  (when-let ((record (skg--request-record-for-edit)))
    (assoc frame-kind (skg--request-record-handlers record))))

(defun skg--request-with-identity (request-text request-id incident-id)
  (let ((request (car (read-from-string request-text))))
    (concat (prin1-to-string
             (append request
                     (when (and skg--server-session-id
                                (not (assoc 'server-session-id request))
                                (not (equal (cdr (assoc 'request request))
                                            "verify connection")))
                       `((server-session-id . ,skg--server-session-id)))
                     `((request-id . ,request-id))
                     (when (and incident-id
                                (not (assoc 'incident-id request)))
                       `((incident-id . ,incident-id)))))
            "\n")))

(defun skg--stamp-queued-request-wire (wire)
  "Add the now-verified server session to a pre-handshake queued WIRE."
  (let* ((line-end (string-match "\n" wire))
         (line (if line-end (substring wire 0 line-end) wire))
         (rest (if line-end (substring wire (1+ line-end)) ""))
         (request (car (read-from-string line))))
    (if (or (assoc 'server-session-id request)
            (equal (cdr (assoc 'request request)) "verify connection"))
        wire
      (unless skg--server-session-id
        (error "Skg cannot send a request before server session verification"))
      (let ((result nil))
        (dolist (field request)
          (when (and (eq (car-safe field) 'request-id)
                     (not (assoc 'server-session-id result)))
            (push `(server-session-id . ,skg--server-session-id) result))
          (push field result))
        (concat (prin1-to-string (nreverse result)) "\n" rest)))))

(defun skg-require-current-server-session (response &optional record)
  "Return RESPONSE's current server session, or reject stale authority.
When RECORD is non-nil, also require that its origin session matches."
  (let* ((raw-session (cadr (assoc 'server-session-id response)))
         ;; Rust prints space-free string atoms without quotes, so `read'
         ;; may return the UUID as a symbol.
         (session (and raw-session (format "%s" raw-session))))
    (unless (and (stringp session)
                 (equal session skg--server-session-id))
      (error "Skg refused authority from server session %S; current session is %S"
             session skg--server-session-id))
    (when (and record
               (not (equal session
                           (skg--buffer-record-server-session-id record))))
      (error "Skg buffer belongs to an earlier server session; reopen it"))
    session))

(defun skg-submit-request (tcp-proc request-text &optional content incident-id)
  "Submit one complete operation, queuing it behind the active request.
CONTENT, when non-nil, is appended with its Content-Length header.
INCIDENT-ID keeps retries in one longer reconciliation episode."
  (let* ((record (or skg--request-draft
                     (make-skg--request-record
                      :id (skg--fresh-request-id) :handlers nil)))
         (request-id (skg--request-record-id record))
         (_ (setf (skg--request-record-incident-id record) incident-id))
         (wire (skg--request-wire
                request-text request-id incident-id content)))
    (puthash request-id record skg--request-records)
    (setq skg--request-draft nil)
    (setq skg--request-queue
          (nconc skg--request-queue
                 (list (list request-id tcp-proc wire nil))))
    (skg--dispatch-next-request)
    request-id))

(defun skg--request-wire (request-text request-id incident-id content)
  (concat
   (skg--request-with-identity request-text request-id incident-id)
   (when content
     (format "Content-Length: %d\r\n\r\n%s"
             (string-bytes content) content))))

(defun skg-submit-request-continuation (tcp-proc request-text
                                                 &optional content)
  "Send a continuation frame for the request whose handler is running."
  (unless (and skg--dispatching-request-id
               (equal skg--dispatching-request-id
                      skg--active-request-id))
    (error "No active request is being dispatched"))
  (process-send-string
   tcp-proc
   (let ((record (gethash skg--dispatching-request-id
                          skg--request-records)))
     (skg--request-wire
      request-text skg--dispatching-request-id
      (and record (skg--request-record-incident-id record)) content))))

(defun skg--dispatch-next-request ()
  ;; A new socket is not ordinary-request authority.  In particular, a
  ;; verification response can finish while its handler is still arranging
  ;; the required census (or can fail while constructing that census).  Do not
  ;; let the next ordinary draft escape merely because the verification record
  ;; became terminal.  Barrier requests are explicitly marked and may pass;
  ;; ordinary work is released only by `skg--complete-buffer-census'.
  (when (and (null skg--active-request-id)
             skg--request-queue
             (or (nth 3 (car skg--request-queue))
                 (eq skg--connection-handshake-state 'verified)))
    (pcase-let ((`(,request-id ,tcp-proc ,wire . ,_)
                 (pop skg--request-queue)))
      (setq skg--active-request-id request-id)
      (condition-case err
          (process-send-string tcp-proc (skg--stamp-queued-request-wire wire))
        (error
         (skg-fail-all-requests
          (format "request send failed: %s" (error-message-string err))))))))

(defun skg-set-request-terminal-handler (handler)
  "Set HANDLER for the terminal outcome of the request being drafted."
  (setf (skg--request-record-terminal-handler (skg--ensure-request-draft))
        handler))

(defun skg-set-request-failure-handler (handler)
  "Set HANDLER for transport/protocol failure of the request being drafted."
  (setf (skg--request-record-failure-handler (skg--ensure-request-draft))
        handler))

(defun skg-set-request-finalizer (finalizer)
  "Set idempotent FINALIZER for the request being drafted."
  (setf (skg--request-record-finalizer (skg--ensure-request-draft))
        finalizer))

(defun skg-submit-priority-request (tcp-proc request-text handlers
                                             &optional content incident-id)
  "Queue an internal REQUEST-TEXT before ordinary drafts.
HANDLERS has the request-record handler representation.  This is reserved for
connection and maintenance barriers: it deliberately does not consume or
mutate the ordinary request draft which may have caused a reconnect.
INCIDENT-ID, when non-nil, binds every response to that maintenance incident."
  (let* ((record (make-skg--request-record
                  :id (skg--fresh-request-id) :handlers handlers
                  :incident-id incident-id))
         (request-id (skg--request-record-id record))
         (wire (skg--request-wire
                request-text request-id incident-id content)))
    (puthash request-id record skg--request-records)
    (setq skg-lp--pending-count
          (+ skg-lp--pending-count (cl-count-if #'cddr handlers)))
    (setq skg--request-queue
          (cons (list request-id tcp-proc wire t) skg--request-queue))
    (skg--dispatch-next-request)
    request-id))

(defun skg--finalize-request-record (record reason)
  "Finalize RECORD exactly once, passing REASON to its finalizer."
  (unless (skg--request-record-finalized-p record)
    (setf (skg--request-record-finalized-p record) t)
    (let ((unfired-one-shots
           (cl-count-if #'cddr (skg--request-record-handlers record))))
      (setq skg-lp--pending-count
            (max 0 (- skg-lp--pending-count unfired-one-shots))))
    (when-let ((finalizer (skg--request-record-finalizer record)))
      (condition-case err
          (funcall finalizer reason)
        (error
         (skg-log 'error 'request-finalizer
                  "finalizer failed for %s: %S"
                  (skg--request-record-id record) err))))))

(defun skg--finish-request (request-id &optional terminal-status)
  (when-let ((record (gethash request-id skg--request-records)))
    (when-let ((handler (skg--request-record-terminal-handler record)))
      (condition-case err
          (funcall handler terminal-status)
        (error
         (skg-log 'error 'request-terminal
                  "terminal handler failed for %s: %S" request-id err))))
    (skg--finalize-request-record record
                                  (or terminal-status 'terminal))
    (remhash request-id skg--request-records))
  (when (equal request-id skg--active-request-id)
    (setq skg--active-request-id nil)
    (skg--dispatch-next-request)))

(defun skg-fail-all-requests (reason)
  "Fail and finalize every draft, active, and queued request exactly once."
  ;; Prevent any finalizer from dispatching a successor on the dead socket.
  (setq skg--request-queue nil
        skg--active-request-id nil
        skg--dispatching-request-id nil)
  (let (records)
    (maphash (lambda (_id record) (push record records))
             skg--request-records)
    (when skg--request-draft (push skg--request-draft records))
    (setq skg--request-draft nil)
    (dolist (record records)
      (unless (skg--request-record-finalized-p record)
        (when-let ((handler (skg--request-record-failure-handler record)))
          (condition-case err
              (funcall handler reason)
            (error
             (skg-log 'error 'request-failure
                      "failure handler failed for %s: %S"
                      (skg--request-record-id record) err))))
        (skg--finalize-request-record record reason))))
  (clrhash skg--request-records)
  (setq skg-lp--pending-count 0))

(defun skg-clear-request-coordinator ()
  "Reset through typed request failure; intended for connection replacement."
  (skg-fail-all-requests "request coordinator reset")
  (setq skg--request-draft nil
        skg--request-queue nil
        skg--active-request-id nil
        skg--dispatching-request-id nil
        skg-lp--pending-count 0))

(progn ;; Length-prefixed (Content-Length) receiver state
  (defvar skg-lp--buf (unibyte-string)
    "Unibyte byte accumulator for length-prefixed responses.")
  (defvar skg-lp--bytes-left nil
    "Receiver body state.
Nil means expect a header; an integer is an ordinary byte length; an
`(:artifact TOTAL DESCRIPTOR-LENGTH)' list preserves the binary boundary.")
  (defvar skg-lp--pending-count 0
    "Number of one-shot responses still expected.
Incremented by `skg-register-response-handler' for one-shot handlers,
decremented by the dispatcher after processing one."))

(defun skg-lp-reset ()
  "Reset the LP state machine to expect a fresh message.
Does not reset `skg-lp--pending-count' — that is managed
by `skg-register-response-handler' and the dispatcher."
  (setq skg-lp--buf        (unibyte-string)
        skg-lp--bytes-left nil))

(defvar skg-config-dir nil
  "Directory containing the skgconfig.toml file.
Set by `skg-client-init'. Used to resolve relative paths
returned by the server (e.g. for get-file-path responses).")

(defvar skg-id-stack nil
  "Stack of (id title) pairs for navigation history.
Each element is a list of two strings.")

(defvar skg--git-diff-mode-enabled nil
  "The client's mirror of the server's per-connection git diff mode.
Set by `skg-view-diff-mode's response handler; reset to nil whenever
a fresh TCP connection is made (the server starts each connection
with diff mode off). Consumers use it only to EXPLAIN -- e.g.
`skg-git-add-if-new-recursive' finds new files via the diff-mode
newX markers, so with diff mode off it says why nothing was found
instead of reporting 0 files.")

(provide 'skg-state)
