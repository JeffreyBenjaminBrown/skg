;;; -*- lexical-binding: t; -*-

;; For instructions see USAGE.md, in this same folder.

(require 'cl-lib)

(require 'heralds-minor-mode)
(require 'skg-metadata)
(require 'skg-request-containerward-view)
(require 'skg-request-node-aliases)
(require 'skg-request-save)
(require 'skg-request-single-root-content-view)
(require 'skg-request-title-matches)
(require 'skg-request-verify-connection)
(require 'skg-state)

(defun skg-client-init (file)
  (defvar skg-port (skg-port-from-toml file))
  (skg-tcp-connect-to-rust))

(defun skg-port-from-toml (file)
  "Return the integer value of `port = ...` from FILE (a TOML config)."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (if (re-search-forward
         "^[ \t]*port[ \t]*=[ \t]*\\([0-9]+\\)"
         nil t)
        (string-to-number (match-string 1))
      (error "No port setting found in %s" file)) ))

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
           :service skg-port
           :filter ;; handles the response
           #'skg-handle-rust-response
           :coding 'binary
           :nowait nil )) )
  skg-rust-tcp-proc)

(defun skg-handle-rust-response (tcp-proc string)
  "Route the response from Rust to the appropriate handler.
.
PITFALL: The handler is already determined before the client receives a response to handle. The client only ever receives something from Rust because the client asked for it. The client sets the handler when it asks.
.
TODO: So far the only response handlers are these:
  skg-display-search-results
  skg-open-org-buffer-from-rust-s-exp
and neither of them uses the `tcp-proc` argument. Unless it will be used by later handlers, it should be deleted."
  (if skg-doc--response-handler
      (funcall skg-doc--response-handler tcp-proc string)
    (error "skg-doc--response-handler is nil; no handler defined for incoming data")) )

(defun skg-doc-disconnect ()
  "Manually close the connection to the Rust server."
  (interactive)
  (when (process-live-p skg-rust-tcp-proc)
    (delete-process skg-rust-tcp-proc)
    (setq skg-rust-tcp-proc nil )) )

(provide 'skg-client)
