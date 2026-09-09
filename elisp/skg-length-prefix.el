;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Read length-prefixed frames and dispatch by request identity.
;;; (This file does not handle adding length prefixes to outgoing
;;; messages. That's easier, and done inline where messages are sent,
;;; e.g., in skg-request-save.el.)

(require 'skg-log)
(require 'skg-state)

(defun skg-lp-handle-generic-chunk (tcp-proc chunk)
  "Consumes the message stream in chunks. When a message is completed, dispatches it and continues for the remaining chunks. In more detail:
.
Top-level filter. Accumulate CHUNK bytes, then step the LP machine until we must wait or we finish one message.
After :done, dispatches to the request record named by request-id.
If there is buffered data and a handler matched, continues the loop."
  ;; Append bytes
  (setq skg-lp--buf (skg-lp-append-chunk skg-lp--buf chunk))
  ;; Repeatedly advance the state machine; stop when it returns a terminal result.
  (cl-loop
   for step = (skg-lp-step skg-lp--buf skg-lp--bytes-left)
   do (pcase step

        ;; Header parsed → install BYTES-LEFT, update buffer to the post-header remainder, and continue.
        (`(:header ,len ,remainder)
         (setq skg-lp--bytes-left len
               skg-lp--buf        remainder))

        ;; Need more bytes to proceed → just exit the loop (filter returns).
        (`(:need-more ,buf ,left)
         (setq skg-lp--buf        buf
               skg-lp--bytes-left left)
         (cl-return nil))

        ;; Completed a full body → dispatch by response-type.
        (`(:done ,payload ,remainder)
         (setq skg-lp--buf        remainder
               skg-lp--bytes-left nil)
         (skg-lp--dispatch-frame tcp-proc payload)
         (if (> (length skg-lp--buf) 0)
             nil ; continue loop -- more data may contain another LP message
           (cl-return nil)))

        ;; Artifact frames keep the descriptor textual and the following
        ;; bytes opaque.  They share one Content-Length so a disconnect can
        ;; never make a partial artifact look like a complete response.
        (`(:artifact-done ,descriptor ,artifact-bytes ,remainder)
         (setq skg-lp--buf        remainder
               skg-lp--bytes-left nil)
         (skg-lp--dispatch-frame tcp-proc descriptor artifact-bytes)
         (if (> (length skg-lp--buf) 0)
             nil
           (cl-return nil)))

        ;; Hard error → reset state and signal.
        (`(:error ,msg)
         (setq skg-lp--buf                (unibyte-string)
               skg-lp--bytes-left         nil)
         (skg-fail-all-requests msg)
         (error "%s" msg)))))

(defun skg-lp--dispatch-frame (tcp-proc payload &optional artifact-bytes)
  "Dispatch PAYLOAD and optional opaque ARTIFACT-BYTES, cleaning up once."
  (condition-case err
      (let* ((response (read payload))
             (request-id (cadr (assoc 'request-id response)))
             (incident-id (cadr (assoc 'incident-id response)))
             (frame-kind (or (cadr (assoc 'frame-kind response))
                             (cadr (assoc 'response-type response))))
             (server-push (cadr (assoc 'server-push response)))
             (terminal-status (cadr (assoc 'terminal-status response)))
             (record (and request-id
                          (gethash (format "%s" request-id)
                                   skg--request-records))))
        (cond
         ((and (not request-id) server-push)
          (if-let ((handler
                    (gethash (format "%s" frame-kind)
                             skg--server-push-handlers)))
              (progn
                (skg-update-global-server-status response)
                (if artifact-bytes
                    (funcall handler tcp-proc payload artifact-bytes)
                  (funcall handler tcp-proc payload)))
            (skg-log 'warn 'dispatch
                     "no server-push handler for frame %s" frame-kind)))
         ((not request-id)
          (skg-log 'warn 'dispatch "response missing request-id: %s"
                   (substring payload 0 (min 80 (length payload)))))
         ((not record)
          (skg-log 'warn 'dispatch "unknown/stale request-id: %s" request-id))
         ((and (not (equal (and incident-id (format "%s" incident-id))
                           (skg--request-record-incident-id record)))
               (not (and (eq frame-kind 'query-wait-status)
                         (null (skg--request-record-incident-id record)))))
          (ding)
          (skg-log 'error 'dispatch
                   "incident-id mismatch on request %s" request-id)
          (when-let ((failure
                      (skg--request-record-failure-handler record)))
            (funcall failure "incident identity mismatch"))
          (skg--finish-request (format "%s" request-id)
                               'protocol-failed))
         (t
          (setq request-id (format "%s" request-id))
          ;; Verification binds the new server session in its handler;
          ;; consume current-session fields there after that binding.
          (unless (eq frame-kind 'verify-connection)
            (skg-update-global-server-status response))
          (let ((handler-entry
                 (assoc frame-kind
                        (skg--request-record-handlers record)))
                (skg--dispatching-request-id request-id))
            (unwind-protect
                (cond
                 ((and handler-entry artifact-bytes)
                  (funcall (cadr handler-entry)
                           tcp-proc payload artifact-bytes))
                 (handler-entry
                  (funcall (cadr handler-entry) tcp-proc payload))
                 ((eq frame-kind 'error)
                  (ding)
                  (message "SKG request failed: %s"
                           (or (cadr (assoc 'content response)) payload)))
                 (t
                  (skg-log 'warn 'dispatch
                           "no handler for frame %s on request %s"
                           frame-kind request-id)))
              (when (and handler-entry (cddr handler-entry))
                (setf (skg--request-record-handlers record)
                      (assoc-delete-all
                       frame-kind (skg--request-record-handlers record)))
                (setq skg-lp--pending-count
                      (max 0 (1- skg-lp--pending-count))))
              (when terminal-status
                (skg--finish-request request-id terminal-status)))))))
    (error
     (let ((reason
            (format "response dispatch failed: %s"
                    (error-message-string err))))
       (skg-log 'error 'dispatch "dispatch error: %S for payload: %s"
                err (substring payload 0 (min 80 (length payload))))
       (message "SKG response handling failed: %s"
                (error-message-string err))
       ;; A failed verification/census callback leaves the socket alive but
       ;; without ordinary-request authority.  Mark it unusable so the next
       ;; command reconnects instead of accumulating behind a census which can
       ;; no longer complete.
       (when (memq skg--connection-handshake-state
                   '(sent census census-texts))
         (setq skg--connection-handshake-error reason
               skg--connection-handshake-state 'failed))
       (skg-fail-all-requests reason)))))

(defun skg-lp-step (buf bytes-left)
  "One pure(ish) step of the LP machine.
Inputs: BUF (unibyte accumulator), BYTES-LEFT (nil → need header; N → need N bytes).
Returns one of:
  (:need-more BUF LEFT)
  (:header LEN-OR-ARTIFACT-SPEC REMAINDER)
  (:done ORG-TEXT REMAINDER)
  (:artifact-done DESCRIPTOR OPAQUE-BYTES REMAINDER)
  (:error MESSAGE)"
  (if (null bytes-left)
      ;; Need a header
      (pcase (skg-lp-try-parse-header buf)
        (`(:incomplete)                `(:need-more ,buf nil))
        (`(:error ,msg)                `(:error ,msg))
        (`(:ok ,len ,remainder)        `(:header ,len ,remainder)))
    ;; Need BYTES-LEFT bytes of body
    (pcase (skg-lp-try-consume-body buf bytes-left)
      (`(:incomplete)                  `(:need-more ,buf ,bytes-left))
      (`(:done ,org-text ,remainder)   `(:done ,org-text ,remainder))
      (`(:artifact-done ,descriptor ,bytes ,remainder)
       `(:artifact-done ,descriptor ,bytes ,remainder))
      (`(:error ,msg)                  `(:error ,msg)))))

(defun skg-lp-append-chunk (buf chunk)
  "Return BUF with CHUNK (UTF-8 encoded bytes) appended."
  (let ((bytes
         (if (multibyte-string-p chunk)
             (encode-coding-string chunk 'utf-8)
           chunk)) )
    (concat buf bytes)) )

(defun skg-lp-try-parse-header (response)
  "Extract a normal length or artifact frame spec from RESPONSE.
Returns one of:
  (:incomplete)
  (:ok LEN-OR-ARTIFACT-SPEC BYTES-SO-FAR)
  (:error MESSAGE)"
  (let ((sep (string-match "\r\n\r\n" response)) )
    (if (not sep)
        '(:incomplete)
      (let* ((header (substring response 0 sep))
             (bytes-so-far (substring response (+ sep 4)))
             (lines (split-string header "\r\n" t))
             (lengths (skg-lp--header-values lines "Content-Length"))
             (content-types (skg-lp--header-values lines "Content-Type"))
             (descriptor-lengths
              (skg-lp--header-values lines "Descriptor-Length")))
        (cond
         ((or (/= (length lengths) 1)
              (not (string-match-p "\\`[0-9]+\\'" (car lengths))))
          (list :error "Malformed header in length-prefixed response"))
         ((or descriptor-lengths
              (member "application/x-skg-artifact-bundle" content-types))
          (let ((total (string-to-number (car lengths))))
            (if (or (not (equal content-types
                                '("application/x-skg-artifact-bundle")))
                    (/= (length descriptor-lengths) 1)
                    (not (string-match-p
                          "\\`[0-9]+\\'" (car descriptor-lengths)))
                    (> (string-to-number (car descriptor-lengths)) total))
                (list :error "Malformed artifact-bundle header")
              (list :ok
                    (list :artifact total
                          (string-to-number (car descriptor-lengths)))
                    bytes-so-far))))
         (t (list :ok (string-to-number (car lengths)) bytes-so-far)))))))

(defun skg-lp--header-values (lines name)
  "Return all values for exact header NAME among LINES."
  (let ((prefix (concat name ": "))
        values)
    (dolist (line lines (nreverse values))
      (when (string-prefix-p prefix line)
        (push (substring line (length prefix)) values)))))

(defun skg-lp-try-consume-body (byte-acc bytes-left)
  "Consume one normal body or one descriptor-plus-artifacts body."
  (if (and (consp bytes-left) (eq (car bytes-left) :artifact))
      (let ((total (nth 1 bytes-left))
            (descriptor-length (nth 2 bytes-left)))
        (if (< (length byte-acc) total)
            '(:incomplete)
          (let* ((body (substring byte-acc 0 total))
                 (descriptor-bytes (substring body 0 descriptor-length))
                 (artifact-bytes (substring body descriptor-length))
                 (remainder (substring byte-acc total)))
            (pcase (skg-lp--decode-utf8-exact descriptor-bytes)
              (`(:ok ,descriptor)
               (list :artifact-done descriptor artifact-bytes remainder))
              (`(:error ,message) (list :error message))))))
    (let ((have (length byte-acc)))
      (if (< have bytes-left)
          '(:incomplete)
        (let* ((payload-bytes (substring byte-acc 0 bytes-left))
               (remainder (substring byte-acc bytes-left))
               (org-text (decode-coding-string payload-bytes 'utf-8 t)))
          (list :done org-text remainder))))))

(defun skg-lp--decode-utf8-exact (bytes)
  "Decode BYTES as UTF-8 only when the round trip is byte-exact."
  (condition-case nil
      (let ((text (decode-coding-string bytes 'utf-8-unix)))
        (if (equal bytes (encode-coding-string text 'utf-8-unix))
            (list :ok text)
          (list :error "Artifact descriptor is not valid UTF-8")))
    (error (list :error "Artifact descriptor is not valid UTF-8"))))

(provide 'skg-length-prefix)
