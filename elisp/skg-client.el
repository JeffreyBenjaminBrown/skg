;;; -*- lexical-binding: t; -*-
;;;
;;; For instructions see USAGE.md, in this same folder.
;;;
;;; USER-FACING FUNCTIONS
;;;   skg-connection-end

(require 'cl-lib)

(require 'heralds-minor-mode)
(require 'skg-length-prefix)
(require 'skg-lock-buffers)
(require 'skg-metadata)
(require 'skg-view-new-empty)
(require 'skg-request-file-path)
(require 'skg-request-git-diff-mode)
(require 'skg-request-save)
(require 'skg-request-single-root-content-view)
(require 'skg-request-verify-connection)
(require 'skg-request-views)
(require 'skg-state)
(require 'skg-user-facing-aliases)

(defun skg-client-init (file)
  (defvar skg-port (skg-port-from-toml file))
  (setq skg-config-dir (file-name-directory (expand-file-name file)))
  (skg-tcp-connect-to-rust)
  (skg-connection-verify))

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
           :nowait nil ))
    (set-process-sentinel
     ;; What sentinels do: When Emacs detects that a process changes state — it exits, is killed, the TCP connection closes (maybe abnormally), etc. — Emacs calls that process's sentinel function with the process and a string describing the event (e.g. "deleted\n", "connection broken by remote peer\n").
     ;; What this sentinel does: If the server crashes or the connection drops mid-save, skg--tcp-sentinel fires and unlocks all save-locked buffers. Without it, a server crash would leave buffers permanently locked.
     skg-rust-tcp-proc
     #'skg--tcp-sentinel) )
  skg-rust-tcp-proc)

(defun skg-handle-rust-response (tcp-proc string)
  "Route the response from Rust to the LP handler.
All server responses are length-prefixed with response-type tags.
The client's LP machine reassembles each message and dispatches by type
via `skg-response-handler-map'."
  (let ((trimmed (string-trim-left string)))
    (if (string-prefix-p "((busy" trimmed) ;; the busy signal
        (let ((parsed (car (read-from-string trimmed))))
          (message "%s" (cdr (assq 'busy parsed)))
          (skg--unlock-all-save-locked)
          (setq skg-response-handler-map nil
                skg-lp--pending-count     0)
          (skg-lp-reset))
      (skg-lp-handle-generic-chunk tcp-proc string) )) )

(defun skg--tcp-sentinel (_proc event)
  "Unlock all save-locked buffers when the TCP connection closes.
Prevents permanently locked buffers if the server crashes mid-save."
  (when (not (string-prefix-p "open" event))
    (skg--unlock-all-save-locked)) )

(defun skg-connection-end ()
  "Manually close the connection to the Rust server."
  (interactive)
  (when (process-live-p skg-rust-tcp-proc)
    (delete-process skg-rust-tcp-proc)
    (setq skg-rust-tcp-proc nil )) )

(provide 'skg-client)
