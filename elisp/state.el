;; TODO: Can these globals be avoided?

(defvar skg-rust-tcp-proc nil
  "Persistent TCP connection to the Rust backend. See
https://www.gnu.org/software/emacs/manual/html_node/elisp/Network-Processes.html")

(defvar skg-content-buffer-name "*skg-content-view*"
  "A buffer name for displaying a content view.")

(defvar skg-search-buffer-name "*skg-title-search*"
  "A buffer name for displaying the results of a title search.")

(defvar skg-doc--response-handler nil
  ;; TODO: Should this exist?
  "Current response handler function.")

(provide 'state)
