;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Global state variables for the skg client.
;;;
;;; TODO: Can these globals be avoided?

(require 'cl-lib)

(defvar skg-rust-tcp-proc nil
  "Persistent TCP connection to the Rust backend. See
https://www.gnu.org/software/emacs/manual/html_node/elisp/Network-Processes.html")

(cl-defstruct skg--request-record id handlers)

(defvar skg--request-records (make-hash-table :test #'equal)
  "Sent request records keyed by connection-local request ID.")
(defvar skg--request-draft nil
  "Request record collecting handlers before its operation is submitted.")
(defvar skg--request-queue nil
  "FIFO of complete request wire strings waiting behind the active request.")
(defvar skg--active-request-id nil)
(defvar skg--dispatching-request-id nil)
(defvar skg--next-request-number 0)

(defun skg--fresh-request-id ()
  (format "emacs-%d-%d" (emacs-pid) (cl-incf skg--next-request-number)))

(defun skg--ensure-request-draft ()
  (or skg--request-draft
      (setq skg--request-draft
            (make-skg--request-record
             :id (skg--fresh-request-id) :handlers nil))))

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

(defun skg--request-with-id (request-text request-id)
  (let ((request (car (read-from-string request-text))))
    (concat (prin1-to-string
             (append request `((request-id . ,request-id))))
            "\n")))

(defun skg-submit-request (tcp-proc request-text &optional content)
  "Submit one complete operation, queuing it behind the active request.
CONTENT, when non-nil, is appended with its Content-Length header."
  (let* ((record (or skg--request-draft
                     (make-skg--request-record
                      :id (skg--fresh-request-id) :handlers nil)))
         (request-id (skg--request-record-id record))
         (wire (skg--request-wire request-text request-id content)))
    (puthash request-id record skg--request-records)
    (setq skg--request-draft nil)
    (setq skg--request-queue
          (nconc skg--request-queue (list (list request-id tcp-proc wire))))
    (skg--dispatch-next-request)
    request-id))

(defun skg--request-wire (request-text request-id content)
  (concat
   (skg--request-with-id request-text request-id)
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
   (skg--request-wire request-text skg--dispatching-request-id content)))

(defun skg--dispatch-next-request ()
  (when (and (null skg--active-request-id) skg--request-queue)
    (pcase-let ((`(,request-id ,tcp-proc ,wire)
                 (pop skg--request-queue)))
      (setq skg--active-request-id request-id)
      (process-send-string tcp-proc wire))))

(defun skg--finish-request (request-id)
  (when-let ((record (gethash request-id skg--request-records)))
    (let ((unfired-one-shots
           (cl-count-if #'cddr (skg--request-record-handlers record))))
      (setq skg-lp--pending-count
            (max 0 (- skg-lp--pending-count unfired-one-shots))))
    (remhash request-id skg--request-records))
  (when (equal request-id skg--active-request-id)
    (setq skg--active-request-id nil)
    (skg--dispatch-next-request)))

(defun skg-clear-request-coordinator ()
  (clrhash skg--request-records)
  (setq skg--request-draft nil
        skg--request-queue nil
        skg--active-request-id nil
        skg--dispatching-request-id nil
        skg-lp--pending-count 0))

(progn ;; Length-prefixed (Content-Length) receiver state
  (defvar skg-lp--buf (unibyte-string)
    "Unibyte byte accumulator for length-prefixed responses.")
  (defvar skg-lp--bytes-left nil
    "If nil, expecting header. If an integer, number of body bytes remaining.")
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
