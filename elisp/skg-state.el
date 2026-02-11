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

(defvar skg-doc--response-handler nil
  ;; PITFALL | TODO:
  ;; A global handler is racy. After process-send-string,
  ;; Emacs returns to the event loop immediately (nothing blocks),
  ;; so the user could invoke a second request before the first
  ;; response arrives, overwriting this handler. In practice this
  ;; hasn't been a problem because responses are fast and users
  ;; don't fire commands in rapid succession. If it ever matters,
  ;; the fix is a request queue or per-request callbacks.
  "Current response handler function.")

(progn ;; Length-prefixed (Content-Length) receiver state
  (defvar skg-lp--buf (unibyte-string)
    "Unibyte byte accumulator for length-prefixed responses.")
  (defvar skg-lp--bytes-left nil
    "If nil, expecting header. If an integer, number of body bytes remaining."))

(defvar skg-config-dir nil
  "Directory containing the skgconfig.toml file.
Set by `skg-client-init'. Used to resolve relative paths
returned by the server (e.g. for get-file-path responses).")

(defvar skg-id-stack nil
  "Stack of (id title) pairs for navigation history.
Each element is a list of two strings.")

(provide 'skg-state)
