;; For instructions see USAGE.md, in this same folder.

(require 'cl-lib)

(require 'get-document)
(require 'heralds)
(require 'state)
(require 'title-search)

(defun skg-tcp-connect-to-rust ()
  "Connect, persistently, to the Rust TCP server."
  (unless ;; create a new connection only if one doesn't exist
          ;; or the existing one is dead
      (and                 skg-rust-tcp-proc
           (process-live-p skg-rust-tcp-proc ))
    (setq                  skg-rust-tcp-proc
          (make-network-process
           :name "skg-doc"
           :buffer "*skg-doc-raw*"
           :host "127.0.0.1"
           :service 1730
           :filter ;; handles the response
           #'skg-handle-rust-response
           :coding 'utf-8
           :nowait nil )) )
  skg-rust-tcp-proc)

(defun skg-doc-disconnect ()
  "Manually close the connection to the Rust server."
  (interactive)
  (when (process-live-p skg-rust-tcp-proc)
    (delete-process skg-rust-tcp-proc)
    (setq skg-rust-tcp-proc nil )) )

(defun skg-handle-rust-response (proc string)
  "Route the response from Rust to the appropriate handler.
.
PITFALL: The handler is already determined before the client receives a response to handle. The client only ever receives something from Rust because the client asked for it. The client sets the handler when it asks.
.
TODO: So far the only response handlers are these:
  skg-display-search-results
  skg-open-org-buffer-from-rust-s-exp
and neither of them uses the `proc` argument. Unless it will be used by later handlers, it should be deleted."
  (if skg-doc--response-handler
      (funcall skg-doc--response-handler proc string)
    (error "skg-doc--response-handler is nil; no handler defined for incoming data")) )

(provide 'client)
