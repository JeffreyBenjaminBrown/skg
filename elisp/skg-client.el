;;; -*- lexical-binding: t; -*-
;;;
;;; For instructions see USAGE.md, in this same folder.

(require 'cl-lib)

(require 'heralds-minor-mode)
(require 'skg-config)
(require 'skg-length-prefix)
(require 'skg-lock-buffers)
(require 'skg-metadata)
(require 'skg-modify-graph)
(require 'skg-view-new-empty)
(require 'skg-request-file-path)
(require 'skg-request-herald-rules)
(require 'skg-maintenance)
(require 'skg-pull)
(require 'skg-request-diff-analysis)
(require 'skg-request-edge-source-info)
(require 'skg-request-export-org)
(require 'skg-request-git-diff-mode)
(require 'skg-request-rebuild-dbs)
(require 'skg-request-recompute-cyclicroots)
(require 'skg-request-rerender-all-views)
(require 'skg-request-save)
(require 'skg-request-source-sets)
(require 'skg-request-stage-moves)
(require 'skg-request-strip-body-whitespace)
(require 'skg-search-make-link)
(require 'skg-request-single-root-content-view)
(require 'skg-request-verify-connection)
(require 'skg-request-views)
(require 'skg-state)
(require 'skg-keymaps-and-aliases)

(defun skg-client-init (file)
  (defvar skg-port (skg-port-from-toml file))
  (setq skg-config-dir (file-name-directory (expand-file-name file)))
  (skg-tcp-connect-to-rust)
  ;; The server grants ordinary request authority only after verification and
  ;; the complete client census.  Herald rules are ordinary requests: waiting
  ;; here keeps them behind the whole startup barrier, rather than merely
  ;; behind the first verification frame.
  (let ((handshake-result (skg-connection-handshake-ensure)))
    (if (eq handshake-result 'busy-initializing)
        ;; Busy is an expected, immediately retryable startup state, not a
        ;; failed reconciliation.  The exceptional signal has already closed
        ;; this request coordinator; the next init opens a fresh connection.
        (message "%s" (or skg--connection-busy-message
                          "Server is initializing, please wait."))
      (unless handshake-result
        (user-error "%s" (skg--client-initialization-failure-message)))
      ;; Re-fetch the herald rule table from the (possibly rebuilt) server,
      ;; DISCARDING any cached table first and WAITING for the reply. So a
      ;; reconnect -- including one right after `skg-reload', which
      ;; deliberately preserves the previous table across the unload -- ends
      ;; up with the CURRENT server's table, ready before this returns. The
      ;; bare async `skg-request-herald-rules' left the stale table in place
      ;; until its reply happened to land, so a server whose rule table had
      ;; changed (e.g. the styled-span heralds) rendered raw metadata.
      (setq heralds--transform-rules nil)
      (let ((rules (skg-herald-rules-ensure)))
        ;; Report the outcome in a SHORT string, rather than returning the
        ;; rule table: `skg-herald-rules-ensure' returns the (large) table,
        ;; and as the last form it would otherwise become
        ;; `skg-client-init''s value -- which an interactive eval echoes in
        ;; full into the minibuffer. This returned a bare nil for that
        ;; reason, but nil in the echo area reads as failure when in fact
        ;; everything worked. `message' returns the string it prints, so
        ;; the echoed value and the echoed message now agree, and they say
        ;; which port was reached -- worth stating, since several skg
        ;; instances (different configs, different ports) can be up at once.
        ;; The rule count is the evidence that the round trip landed: the
        ;; table is `(skg RULE ...)', hence the 1-.
        (if rules
            (message "skg ready on port %s -- %d herald rules loaded."
                     skg-port (1- (length rules)))
          (user-error "%s" (skg--client-initialization-failure-message)))))))
  ;; Skg, magit and global keybindings are in skg-keymaps-and-aliases.el.

(defun skg-tcp-connect-to-rust ()
  "Connect, persistently, to the Rust TCP server."
  (unless ;; create a new connection only if one doesn't exist
          ;; or the existing one is dead
      (and                 skg-rust-tcp-proc
           (process-live-p skg-rust-tcp-proc)
           skg--connection-handshake-state
           (not (memq skg--connection-handshake-state
                      '(busy-initializing failed))))
    (when (and skg-rust-tcp-proc
               (process-live-p skg-rust-tcp-proc))
      (delete-process skg-rust-tcp-proc))
    (skg-clear-request-coordinator)
    (skg-lp-reset)
    (setq skg--connection-handshake-state nil
          skg--connection-handshake-error nil
          skg--connection-busy-message nil
          skg--server-session-id nil
          skg--owner-publication-revision nil
          skg--graph-write-admission nil
          skg--client-constructor-admission
          (cond
           ((and (boundp 'skg--maintenance-client-incident)
                 skg--maintenance-client-incident) 'closed)
           ((eq skg--client-constructor-admission 'closing) 'closing)
           (t 'open))
          skg--git-diff-mode-enabled
          ;; The server starts each connection with diff mode off.
          nil
          skg--server-source-inventory nil
          skg--server-store-state nil)
    (condition-case err
        (setq skg-rust-tcp-proc
              (make-network-process
               :name "skg-doc"
               :buffer nil
               :host "127.0.0.1"
               :service skg-port
               :filter ;; handles the response
               #'skg-handle-rust-response
               :coding 'binary
               :nowait nil ))
      (error
       (setq skg-rust-tcp-proc nil)
       (user-error "%s" (skg--server-unavailable-message err))))
    (set-process-sentinel
     ;; What sentinels do: When Emacs detects that a process changes state — it exits, is killed, the TCP connection closes (maybe abnormally), etc. — Emacs calls that process's sentinel function with the process and a string describing the event (e.g. "deleted\n", "connection broken by remote peer\n").
     ;; What this sentinel does: If the server crashes or the connection drops mid-save, skg--tcp-sentinel fires and unlocks all save-locked buffers. Without it, a server crash would leave buffers permanently locked.
     skg-rust-tcp-proc
     #'skg--tcp-sentinel)
    (skg--submit-connection-handshake skg-rust-tcp-proc))
  skg-rust-tcp-proc)

(defun skg--server-log-files ()
  "Return the user and watcher logs for the active Skg configuration."
  (let* ((logs-dir (expand-file-name
                    "logs"
                    (or skg-config-dir default-directory)))
         (user-log (expand-file-name "server-to-user.log" logs-dir))
         (watch-log (expand-file-name "cargo-watch.log" logs-dir)))
    (list user-log watch-log)))

(defun skg--client-initialization-failure-message ()
  "Explain why a connection without herald rules is not ready."
  (let* ((logs (skg--server-log-files))
         (reason
         (cond
           (skg--connection-handshake-error
            (format "the server rejected startup reconciliation: %s"
                    skg--connection-handshake-error))
           ((not (and skg-rust-tcp-proc
                      (process-live-p skg-rust-tcp-proc)))
            "the server connection closed before initialization completed")
           ((not (eq skg--connection-handshake-state 'verified))
            (format
             "the TCP connection did not complete its handshake within %.1f seconds"
             skg-connection-handshake-timeout))
           (t
            (format
             (concat "the server handshake completed, but no valid herald "
                     "rule table was installed after %d attempts")
             skg-herald-rules-max-attempts)))))
    (format
     (concat
      "SKG client initialization failed on port %s: %s.\n"
      "The client is not ready.\n"
      "Look for the startup error in:\n"
      "  %s\n"
      "  %s")
     (if (boundp 'skg-port) skg-port "[unknown]") reason
     (car logs) (cadr logs))))

(defun skg--server-unavailable-message (err)
  "Return a user-facing message for a failed Rust server connection ERR."
  (let ((logs (skg--server-log-files)))
    (format
     (concat
      "Could not connect to the SKG server on port %s.\n"
      "The server may have failed during startup.\n"
      "Look for the startup error in:\n"
      "  %s\n"
      "  %s\n"
      "Connection error: %s")
     (if (boundp 'skg-port) skg-port "[unknown]")
     (car logs) (cadr logs) (error-message-string err))))

(defun skg-handle-rust-response (tcp-proc string)
  "Route the response from Rust to the LP handler.
All ordinary server responses are length-prefixed request frames.
The client's LP machine reassembles each frame and dispatches it to
the request record named by its request-id."
  (let ((trimmed (string-trim-left string)))
    (if (string-prefix-p "((busy-initializing" trimmed)
        (let* ((parsed (car (read-from-string trimmed)))
               (status (format "%s"
                               (cdr (assq 'busy-initializing parsed)))))
          (message "%s" status)
          (setq skg--connection-handshake-state 'busy-initializing
                skg--connection-handshake-error nil
                skg--server-session-id nil
                skg--owner-publication-revision nil
                skg--graph-write-admission nil
                skg--client-constructor-admission
                (if (and (boundp 'skg--maintenance-client-incident)
                         skg--maintenance-client-incident)
                    'closed 'open)
                skg--connection-busy-message status)
          (skg-clear-request-coordinator)
          (skg-lp-reset))
      (skg-lp-handle-generic-chunk tcp-proc string) )) )

(defun skg--tcp-sentinel (proc event)
  "Clean up when the TCP connection closes."
  (when (and (eq proc skg-rust-tcp-proc)
             (not (string-prefix-p "open" event)))
    (unless (or skg--connection-handshake-error
                (memq skg--connection-handshake-state
                      '(busy-initializing verified)))
      (setq skg--connection-handshake-error
            (format "the server connection closed: %s"
                    (string-trim event))))
    (unless (eq skg--connection-handshake-state 'busy-initializing)
      (setq skg--connection-handshake-state nil))
    (setq skg--server-session-id nil
          skg--owner-publication-revision nil
          skg--graph-write-admission nil
          skg--client-constructor-admission
          (if (and (boundp 'skg--maintenance-client-incident)
                   skg--maintenance-client-incident)
              'closed 'open))
    (skg-clear-request-coordinator)
    (skg-lp-reset)) )

(defun skg-connection-end ()
  "Manually close the connection to the Rust server."
  (interactive)
  (when (process-live-p skg-rust-tcp-proc)
    (delete-process skg-rust-tcp-proc)
    (setq skg-rust-tcp-proc nil )) )

(provide 'skg-client)
