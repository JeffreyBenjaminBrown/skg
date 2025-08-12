;; USAGE:
;; First start the TYpeDB server
;; (run `typedb server` from a shell)
;; and after that start the Rust server
;; (evaluate `cargo run` from a shell, from the root of this project).
;; Next evaluate `elisp/init.el` (visit, then run `M-x eval-buffer`).
;; Finally, evaluate this file.
;; Now these commands can be run:
;;
;;   (skg-tcp-connect-to-rust) ;; surprisingly, does not need to be run separately, as the `request-*` functions call it
;;   (request-document-from-node "a") ;; try 1, 1a, or a
;;   (request-title-matches "second") ;; try match, title, second
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

(require 'get-document)
(require 'state)
(require 'title-search)

(defun skg-tcp-connect-to-rust ()
  "Connect, persistently, to the Rust TCP server."
  (unless ;; create a new connection only if one doesn't exist or the existing one is dead
      (and                 skg-rust-tcp-proc
           (process-live-p skg-rust-tcp-proc ))
    (setq skg-rust-tcp-proc
          (make-network-process
           :name "skg-doc"
           :buffer "*skg-doc-raw*"
           :host "127.0.0.1"
           :service 1730
           :filter ;; handles the response
           #'skg-handle-rust-response
           :coding 'utf-8
           :nowait nil)))
  skg-rust-tcp-proc)

(defun skg-handle-rust-response (proc string)
  "Route the response from Rust to the appropriate handler."
  (if skg-doc--response-handler
      (funcall skg-doc--response-handler proc string)
    (error "skg-doc--response-handler is nil; no handler defined for incoming data")))

(defun skg-doc-disconnect ()
  "Manually close the connection to the Rust server."
  (interactive)
  (when (process-live-p skg-rust-tcp-proc)
    (delete-process skg-rust-tcp-proc)
    (setq skg-rust-tcp-proc nil)))
