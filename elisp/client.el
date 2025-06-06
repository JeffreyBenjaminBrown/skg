;; USAGE:
;; First start the TYpeDB server
;; (run `typedb server` from a shell)
;; and after that start the Rust server
;; (evaluate `cargo run` from a shell, from the root of this project).
;; Next evaluate this buffer (`M-x eval-buffer`).
;; Now these commands can be run:
;;
;;   (skg-doc-connect) ;; surprisingly, does not need to be run separately, as the `request-*` functions call it
;;   (request-document-from-node "a") ;; try 1, 1a, or a
;;   (request-title-matches "match") ;; try match, title, second
;;   (skg-doc-disconnect)
;;
;; The second of those asks Rust to ask TypeDB for
;; the document containing the node with the specified ID,
;; builds an s-expression representing an org-document
;; based on the result, and sends that to Emacs,
;; causing Emacs to open an org-mode buffer
;; displaying the results.
;;
;; The third asks Rust to search the Tantivy index
;; for titles matching the search terms and displays
;; the results in a buffer.

(require 'title-search)
(require 'get-document)

(defvar skg-doc--proc nil
  "Persistent TCP connection to the Rust backend.")

(defvar skg-doc-buffer-name "*skg-document*"
  "Buffer name for displaying s-expressions from the Rust server.")

(defvar skg-search-buffer-name "*skg-search-results*"
  "Buffer name for displaying title search results.")

(defvar skg-doc--response-handler nil
  "Current response handler function.")

(defun skg-doc-connect ()
  "Connect, persistently, to the Rust TCP server."
  (unless ;; create a new connection only if one doesn't exist or the existing one is dead
      (and skg-doc--proc
           (process-live-p skg-doc--proc))
    (setq skg-doc--proc
          (make-network-process
           :name "skg-doc"
           :buffer "*skg-doc-raw*"
           :host "127.0.0.1"
           :service 1730
           :filter ;; handles the response
           #'skg-handle-rust-response
           :coding 'utf-8
           :nowait nil)))
  skg-doc--proc)

(defun skg-handle-rust-response (proc string)
  "Route the response from Rust to the appropriate handler."
  (if skg-doc--response-handler
      (funcall skg-doc--response-handler proc string)
    (error "skg-doc--response-handler is nil; no handler defined for incoming data")))

(defun skg-doc-disconnect ()
  "Manually close the connection to the Rust server."
  (interactive)
  (when (process-live-p skg-doc--proc)
    (delete-process skg-doc--proc)
    (setq skg-doc--proc nil)))
