;;; -*- lexical-binding: t; -*-
;;;
;;; TODO: Can these globals be avoided?

(defvar skg-rust-tcp-proc nil
  "Persistent TCP connection to the Rust backend. See
https://www.gnu.org/software/emacs/manual/html_node/elisp/Network-Processes.html")

(defvar skg-doc--response-handler nil
  ;; TODO: Should this exist?
  "Current response handler function.")

(progn ;; Length-prefixed (Content-Length) receiver state
  (defvar skg-lp--buf (unibyte-string)
    "Unibyte byte accumulator for length-prefixed responses.")
  (defvar skg-lp--bytes-left nil
    "If nil, expecting header. If an integer, number of body bytes remaining.") )

(defvar skg-id-stack nil
  "Stack of (id title) pairs for navigation history.
Each element is a list of two strings.")

(provide 'skg-state)
