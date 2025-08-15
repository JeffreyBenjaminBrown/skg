;; TODO: Can these globals be avoided?

(defvar skg-rust-tcp-proc nil
  "Persistent TCP connection to the Rust backend. See
https://www.gnu.org/software/emacs/manual/html_node/elisp/Network-Processes.html")

(defvar skg-doc--response-handler nil
  ;; TODO: Should this exist?
  "Current response handler function.")

(provide 'state)
