;;; skg-pending-save.el --- Durable client identity for ordinary saves -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'org-id)
(require 'subr-x)
(require 'skg-recovery-archive)
(require 'skg-state)

(define-error 'skg-pending-save-error "Skg pending save failed")

(defconst skg-pending-save-format-version 1)
(defconst skg-pending-save--states
  '(prepared uncertain terminal refused acknowledged))

(defun skg-pending-save--fail (format-string &rest arguments)
  (signal 'skg-pending-save-error
          (list (apply #'format format-string arguments))))

(defun skg-pending-save-new-operation-id ()
  "Return a new opaque ordinary-save operation identity."
  (org-id-uuid))

(defun skg-pending-save-fingerprint (request-intent content)
  "Hash exact UTF-8 REQUEST-INTENT and CONTENT bytes."
  (secure-hash
   'sha256
   (concat (encode-coding-string request-intent 'utf-8 t)
           (unibyte-string 0)
           (encode-coding-string content 'utf-8 t))))

(defun skg-pending-save--root ()
  (let* ((archive-root (skg-recovery-resolve-archive-root))
         (root (expand-file-name "pending-saves" archive-root)))
    (skg-recovery--make-private-directory root t)
    root))

(defun skg-pending-save--strict-uuid-p (value)
  (and (stringp value)
       (string-match-p
        "\\`[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}\\'"
        value)))

(defun skg-pending-save--path (root operation-id)
  (unless (skg-pending-save--strict-uuid-p operation-id)
    (skg-pending-save--fail "invalid save operation id: %S" operation-id))
  (expand-file-name (format "operation-%s.sexp" operation-id) root))

(defun skg-pending-save--field (record name)
  (cadr (assq name record)))

(defun skg-pending-save--validate (record &optional expected-id)
  (unless (and (listp record)
               (equal (skg-pending-save--field record 'format-version)
                      skg-pending-save-format-version))
    (skg-pending-save--fail "unsupported pending-save record"))
  (let ((operation-id (skg-pending-save--field record 'operation-id))
        (fingerprint
         (skg-pending-save--field record 'request-base-fingerprint))
        (state (skg-pending-save--field record 'state)))
    (unless (and (skg-pending-save--strict-uuid-p operation-id)
                 (or (null expected-id) (equal operation-id expected-id)))
      (skg-pending-save--fail "pending-save operation identity changed"))
    (unless (and (stringp fingerprint)
                 (string-match-p "\\`[0-9a-f]\\{64\\}\\'" fingerprint))
      (skg-pending-save--fail "invalid pending-save fingerprint"))
    (unless (memq state skg-pending-save--states)
      (skg-pending-save--fail "invalid pending-save state: %S" state))
    (when (memq state '(prepared uncertain terminal))
      (unless (and (stringp (skg-pending-save--field record 'request))
                   (stringp (skg-pending-save--field record 'content)))
        (skg-pending-save--fail "unresolved pending save lacks exact bytes")))
    record))

(defun skg-pending-save--read-path (path &optional expected-id)
  (let* ((bytes (skg-recovery--read-bytes path))
         (text (decode-coding-string bytes 'utf-8 t))
         parsed)
    (condition-case err
        (setq parsed (car (read-from-string text)))
      (error (skg-pending-save--fail "cannot parse %s: %s"
                                     path (error-message-string err))))
    (skg-pending-save--validate parsed expected-id)))

(defun skg-pending-save--write (record)
  (setq record (skg-pending-save--validate record))
  (let* ((root (skg-pending-save--root))
         (operation-id (skg-pending-save--field record 'operation-id))
         (path (skg-pending-save--path root operation-id))
         (bytes (encode-coding-string
                 (concat (skg-recovery-canonical-sexpr record) "\n")
                 'utf-8 t)))
    (skg-recovery--replace-private-file
     path bytes root
     (substring (secure-hash 'sha256
                             (format "%s:%s:%s" operation-id
                                     (float-time) (random)))
                0 24))
    (set-file-modes path #o600)
    path))

(cl-defun skg-pending-save-prepare
    (&key operation-id request-base-fingerprint request content buffer-id)
  "Durably retain one exact authored save before it may be submitted."
  (let* ((root (skg-pending-save--root))
         (path (skg-pending-save--path root operation-id)))
    (when (file-exists-p path)
      (let ((existing (skg-pending-save--read-path path operation-id)))
        (unless (and (equal request-base-fingerprint
                            (skg-pending-save--field
                             existing 'request-base-fingerprint))
                     (equal request (skg-pending-save--field existing 'request))
                     (equal content (skg-pending-save--field existing 'content)))
          (skg-pending-save--fail
           "operation id already binds a different save: %s" operation-id))
        (cl-return-from skg-pending-save-prepare existing)))
    (let ((record
           `((format-version ,skg-pending-save-format-version)
             (operation-id ,operation-id)
             (request-base-fingerprint ,request-base-fingerprint)
             (client "emacs")
             (client-session-id ,skg--client-session-id)
             (buffer-id ,(or buffer-id ""))
             (state prepared)
             (request ,request)
             (content ,content))))
      (skg-pending-save--write record)
      record)))

(defun skg-pending-save-records ()
  "Read every valid client save record in deterministic order."
  (let ((root (skg-pending-save--root)) records)
    (dolist (path (sort (directory-files root t
                                         "\\`operation-[0-9a-f-]+\\.sexp\\'")
                        #'string<))
      (when (file-symlink-p path)
        (skg-pending-save--fail "pending-save record is a symlink: %s" path))
      (push (skg-pending-save--read-path path) records))
    (nreverse records)))

(defun skg-pending-save-unresolved-records ()
  "Return records which still forbid allocating a new save identity."
  (cl-remove-if-not
   (lambda (record)
     (memq (skg-pending-save--field record 'state)
           '(prepared uncertain terminal)))
   (skg-pending-save-records)))

(defun skg-pending-save-assert-none-unresolved ()
  "Refuse a new ordinary save while an earlier identity is unresolved."
  (when-let ((record (car (skg-pending-save-unresolved-records))))
    (user-error
     "Save %s is unresolved; inspect it with M-x skg-pending-save-status"
     (skg-pending-save--field record 'operation-id))))

(defun skg-pending-save--replace-field (record name value)
  (let ((entry (assq name record)))
    (if entry (setcdr entry (list value))
      (setq record (append record (list (list name value))))))
  record)

(defun skg-pending-save--transition (record state &optional response server-state)
  (let* ((operation-id (skg-pending-save--field record 'operation-id))
         (path (skg-pending-save--path (skg-pending-save--root) operation-id))
         (current (skg-pending-save--read-path path operation-id)))
    (unless (equal (skg-pending-save--field current 'request-base-fingerprint)
                   (skg-pending-save--field record 'request-base-fingerprint))
      (skg-pending-save--fail "pending-save fingerprint changed"))
    (setq current (skg-pending-save--replace-field current 'state state))
    (when response
      (setq current (skg-pending-save--replace-field
                     current 'terminal-response response)))
    (when server-state
      (setq current (skg-pending-save--replace-field
                     current 'server-state server-state)))
    (skg-pending-save--write current)
    current))

(defun skg-pending-save-mark-uncertain (record)
  "Preserve RECORD for explicit resolution after transport uncertainty."
  (let ((state (skg-pending-save--field
                (skg-pending-save--read-path
                 (skg-pending-save--path
                  (skg-pending-save--root)
                  (skg-pending-save--field record 'operation-id)))
                'state)))
    (if (eq state 'prepared)
        (skg-pending-save--transition record 'uncertain)
      record)))

(defun skg-pending-save-mark-terminal (record response &optional refused)
  "Record exact terminal RESPONSE before any destructive client application."
  (skg-pending-save--transition record (if refused 'refused 'terminal)
                                response))

(defun skg-pending-save-mark-acknowledged (record)
  "Replace a terminal record with a compact retained identity marker."
  (let* ((operation-id (skg-pending-save--field record 'operation-id))
         (current
          (skg-pending-save--read-path
           (skg-pending-save--path (skg-pending-save--root) operation-id)
           operation-id))
         (compact
         `((format-version ,skg-pending-save-format-version)
           (operation-id ,operation-id)
           (request-base-fingerprint
            ,(skg-pending-save--field record 'request-base-fingerprint))
           (client ,(or (skg-pending-save--field current 'client) "emacs"))
           (client-session-id
            ,(or (skg-pending-save--field current 'client-session-id)
                 skg--client-session-id))
           (buffer-id ,(or (skg-pending-save--field current 'buffer-id) ""))
           (state acknowledged))))
    (unless (equal (skg-pending-save--field
                    current 'request-base-fingerprint)
                   (skg-pending-save--field
                    record 'request-base-fingerprint))
      (skg-pending-save--fail "pending-save fingerprint changed"))
    (unless (eq (skg-pending-save--field current 'state) 'terminal)
      (skg-pending-save--fail
       "cannot acknowledge a save without a terminal result"))
    (skg-pending-save--write compact)
    compact))

(defun skg-pending-save-apply-status (record response)
  "Persist an explicit save-operation status RESPONSE for RECORD."
  (skg-pending-save-verify-response record response)
  (let ((server-state (cadr (assq 'state response)))
        (terminal-response (cadr (assq 'terminal-response response))))
    (pcase server-state
      ('committed
       (unless (stringp terminal-response)
         (skg-pending-save--fail
          "committed save status lacks its exact terminal response"))
       (let* ((updated
               (skg-pending-save--transition
                record 'terminal terminal-response 'committed))
              (terminal (car (read-from-string terminal-response)))
              (requires-fresh
               (cadr (assq 'requires-fresh-view terminal))))
         (when (memq requires-fresh '(t true))
           (setq updated (skg-pending-save--replace-field
                          updated 'fresh-view-required 'true))
           (skg-pending-save--write updated))
         updated))
      ('refused
       (unless (stringp terminal-response)
         (skg-pending-save--fail
          "refused save status lacks its exact terminal response"))
       (skg-pending-save--transition
        record 'refused terminal-response 'refused))
      ((or 'unknown 'prepared 'authorized 'applied 'blocked)
       (skg-pending-save--transition record 'uncertain nil server-state))
      (_ (skg-pending-save--fail
          "invalid save-operation status: %S" server-state)))))

(defun skg-pending-save-retry-material (record)
  "Return exact (REQUEST CONTENT) only after a safe explicit status."
  (let ((server-state (skg-pending-save--field record 'server-state)))
    (unless (memq server-state '(unknown prepared))
      (skg-pending-save--fail
       "save must have explicit unknown/prepared status before retry")))
  (list (skg-pending-save--field record 'request)
        (skg-pending-save--field record 'content)))

(defun skg-pending-save-verify-response (record response)
  "Require RESPONSE to echo RECORD's durable identity."
  (let ((operation-id (format "%s" (cadr (assq 'operation-id response))))
        (fingerprint
         (format "%s" (cadr (assq 'request-base-fingerprint response)))))
    (unless (and (equal operation-id
                        (skg-pending-save--field record 'operation-id))
                 (equal fingerprint
                        (skg-pending-save--field
                         record 'request-base-fingerprint)))
      (skg-pending-save--fail "save response identity mismatch")))
  t)

(provide 'skg-pending-save)
