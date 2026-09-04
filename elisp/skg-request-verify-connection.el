;;; -*- lexical-binding: t; -*-
;;;
;;; DATA USED/ASSUMED: See /api.md.

(require 'cl-lib)
(require 'skg-length-prefix)

(defconst skg--maintenance-archive-format-version 1)

(defun skg--installed-undo-fu-session-version ()
  "Return the installed undo-fu-session version string, or nil.
This inspects the public package header without enabling any package mode."
  (when-let ((library (locate-library "undo-fu-session")))
    (require 'lisp-mnt)
    (with-temp-buffer
      (insert-file-contents library)
      (lm-header "version"))))

(defun skg--connection-handshake-request ()
  "Return the role-bearing handshake for this Emacs process."
  (let* ((undo-version (skg--installed-undo-fu-session-version))
         (supported (equal undo-version "0.8")))
    (concat
     (prin1-to-string
      `((request . "verify connection")
        (role . "interactive")
        (client-kind . "emacs")
        (client-version . ,emacs-version)
        (client-session-id . ,skg--client-session-id)
        (archive-format-version . ,skg--maintenance-archive-format-version)
        (native-undo-kind . ,(if supported
                                 "undo-fu-session"
                               "unavailable"))
        (native-undo-version . ,(or undo-version "unavailable"))
        (source-set . ,skg--active-source-set-name)))
     "\n")))

(defun skg--show-handshake-telescope-warnings (response)
  "Display structured load WARNINGS carried by RESPONSE."
  (let ((warnings (cadr (assoc 'telescope-warnings response))))
    (when warnings
      (let ((content
             (concat
              "* WARNING: Telescope load warnings\n"
              (mapconcat
               (lambda (warning)
                 (let ((pid (cadr (assoc 'pid warning)))
                       (message-text (cadr (assoc 'message warning)))
                       (winners (cadr (assoc 'winning-paths warning)))
                       (losers (cadr (assoc 'ignored-paths warning))))
                   (concat
                    (format "** %s\n%s\n" pid message-text)
                    (when winners
                      (concat "*** retained owned files\n"
                              (mapconcat (lambda (path)
                                           (format "**** %s" path))
                                         winners "\n") "\n"))
                    (when losers
                      (concat "*** ignored foreign files\n"
                              (mapconcat (lambda (path)
                                           (format "**** %s" path))
                                         losers "\n") "\n")))))
               warnings ""))))
        (skg-big-nonfatal-message
         "*SKG Telescope Warnings*"
         (format "WARNING: Skg loaded with %d telescope warning(s)."
                 (length warnings))
         content)))))

(defun skg--install-connection-verification (tcp-proc payload)
  "Install authoritative server state from handshake PAYLOAD."
  (let* ((response (read payload))
         (content (cadr (assoc 'content response))))
    (skg-install-source-inventory response)
    (setq skg--active-source-set-name
          (format "%s" (cadr (assoc 'active-source-set response)))
          skg--maintenance-archive-folder
          (cadr (assoc 'maintenance-archive-folder response))
          skg--maintenance-archive-identity
          (cadr (assoc 'maintenance-archive-identity response))
          skg--maintenance-state
          `((epoch . ,(cadr (assoc 'maintenance-epoch response)))
            (state . ,(cadr (assoc 'maintenance-state response)))
            (census-required . ,(cadr (assoc 'census-required response))))
          skg--server-store-state
          `((graph-generation
             . ,(cadr (assoc 'graph-generation response)))
            (manifest-revision
             . ,(cadr (assoc 'manifest-revision response)))
            (typedb-health
             . ,(cadr (assoc 'typedb-health response)))
            (tantivy-health
             . ,(cadr (assoc 'tantivy-health response)))
            ))
    (when (fboundp 'skg-maintenance-adopt-handshake-epoch)
      (skg-maintenance-adopt-handshake-epoch))
    (setq skg--connection-handshake-state 'census)
    (skg--show-handshake-telescope-warnings response)
    (when (fboundp 'skg-install-pending-recovery-incidents)
      (skg-install-pending-recovery-incidents response))
    (message "%s" (or (and content (format "%s" content))
                       "connected; reconciling buffer census"))
    (skg--submit-buffer-census tcp-proc)))

(defun skg--submit-buffer-census (tcp-proc)
  "Send compact descriptors without embedding any complete view text."
  (require 'skg-buffer-registry)
  (skg-submit-priority-request
   tcp-proc
   "((request . \"client census\"))\n"
   `((client-census ,#'skg--handle-buffer-census-response . t))
   (prin1-to-string (skg-buffer-census))))

(defun skg--handle-buffer-census-response (tcp-proc payload)
  "Complete census or answer the server's targeted text request."
  (let* ((response (read payload))
         (required (mapcar (lambda (value) (format "%s" value))
                           (or (cadr (assoc 'text-required-buffer-ids
                                           response))
                               nil)))
         (stale (or (cadr (assoc 'stale-buffer-ids response)) nil)))
    (if (fboundp 'skg-maintenance-handle-census-stale)
        (skg-maintenance-handle-census-stale stale)
      (skg-mark-census-buffers-stale stale))
    (if required
        (progn
          (setq skg--connection-handshake-state 'census-texts)
          (skg-submit-priority-request
           tcp-proc
           "((request . \"client census texts\"))\n"
           `((client-census ,#'skg--finish-buffer-census . t))
           (prin1-to-string (skg-buffer-census-texts required))))
      (setq skg--connection-handshake-state 'verified)
      (when (fboundp 'skg-resume-maintenance-after-census)
        (run-at-time 0 nil #'skg-resume-maintenance-after-census)))))

(defun skg--finish-buffer-census (_tcp-proc payload)
  "Install the terminal disposition of requested census texts."
  (let* ((response (read payload))
         (stale (or (cadr (assoc 'stale-buffer-ids response)) nil)))
    (if (fboundp 'skg-maintenance-handle-census-stale)
        (skg-maintenance-handle-census-stale stale)
      (skg-mark-census-buffers-stale stale))
    (unless (equal (format "%s" (cadr (assoc 'census-complete response)))
                   "true")
      (error "Skg server did not complete the buffer census"))
    (setq skg--connection-handshake-state 'verified)
    (when (fboundp 'skg-resume-maintenance-after-census)
      (run-at-time 0 nil #'skg-resume-maintenance-after-census))))

(defun skg--submit-connection-handshake (tcp-proc)
  "Put the mandatory handshake first without consuming an ordinary draft."
  (unless skg--connection-handshake-state
    (setq skg--connection-handshake-state 'sent)
    (skg-submit-priority-request
     tcp-proc
     (skg--connection-handshake-request)
     `((verify-connection
        ,#'skg--install-connection-verification . t)))))

(defun skg-connection-verify ()
  "Verify connection to the Rust server,
by sending a simple ping to the Rust server
to verify the connection is working. The server responds with a
confirmation message that is displayed in the minibuffer.

Surprisingly, the TCP connection
does not need to be explicitly launched,
because each of the client's `request-*` functions
calls `(skg-tcp-connect-to-rust)`
(which is idempotent and cheap to rerun)."
  (interactive)
  (let* ((already-connected (and skg-rust-tcp-proc
                                 (process-live-p skg-rust-tcp-proc)))
         (tcp-proc (skg-tcp-connect-to-rust)))
    (unless already-connected
      ;; `skg-tcp-connect-to-rust' installed the mandatory handshake.
      (cl-return-from skg-connection-verify nil))
    (skg-register-response-handler
     'verify-connection
     #'skg--install-connection-verification
     t)
    (skg-submit-request tcp-proc (skg--connection-handshake-request))))

(provide 'skg-request-verify-connection)
