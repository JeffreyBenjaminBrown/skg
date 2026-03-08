;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Global state variables for the skg client.
;;;
;;; USER-FACING FUNCTIONS AND GLOBAL VARIABLES
;;;   skg-id-stack (variable)
;;;
;;; TODO: Can these globals be avoided?

(defvar skg-rust-tcp-proc nil
  "Persistent TCP connection to the Rust backend. See
https://www.gnu.org/software/emacs/manual/html_node/elisp/Network-Processes.html")

(defvar skg-response-handler-map nil
  "Alist from response-type symbols to handler entries.
Each entry is (TYPE . (HANDLER . ONE-SHOT-P)).
HANDLER is called as (funcall HANDLER tcp-proc payload).
If ONE-SHOT-P is non-nil, the entry is removed after use.

PITFALL: Keyed by response-type, not by request instance.
If the user fires two requests of the same type in rapid
succession, the second registration replaces the first's
handler. In practice this hasn't been a problem because
responses are fast. If it ever matters, the fix is a
per-request token or a queue.")

(defun skg-register-response-handler (response-type handler &optional one-shot)
  "Register HANDLER for RESPONSE-TYPE in `skg-response-handler-map'.
If ONE-SHOT is non-nil, the handler is removed after first use
and `skg-lp--pending-count' is incremented.
Replaces any existing handler for the same type."
  (setq skg-response-handler-map
        (cons (cons response-type (cons handler one-shot))
              (assoc-delete-all response-type
                               skg-response-handler-map)))
  (when one-shot
    (setq skg-lp--pending-count (1+ skg-lp--pending-count))))

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

(provide 'skg-state)
