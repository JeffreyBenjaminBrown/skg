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

(defvar skg--connection-busy-message nil
  "Server status reported by the exceptional busy-initializing signal.")

(defvar skg--active-source-set-name "server-default"
  "Name claimed in a reconnect handshake; replaced by server authority.")

(defvar skg--maintenance-archive-folder nil)
(defvar skg--maintenance-archive-identity nil)
(defvar skg--maintenance-state nil)

(defvar skg--rebuilding nil
  "Non-nil exactly while server metadata says a graph/search pair is rebuilding.")

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
                     `((request-id . ,request-id))
                     (when incident-id
                       `((incident-id . ,incident-id)))))
            "\n")))

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
          (process-send-string tcp-proc wire)
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
