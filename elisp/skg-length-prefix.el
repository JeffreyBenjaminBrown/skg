;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Read length-prefixed messages from the server
;;; and dispatch by response-type via skg-response-handler-map.
;;; (This file does not handle adding length prefixes to outgoing
;;; messages. That's easier, and done inline where messages are sent,
;;; e.g., in skg-request-save.el.)
;;;
;;; USER-FACING FUNCTIONS
;;;   (none)

(require 'skg-state)

(defun skg-lp-handle-generic-chunk (tcp-proc chunk)
  "Consumes the message stream in chunks. When a message is completed, dispatches it and continues for the remaining chunks. In more detail:
.
Top-level filter. Accumulate CHUNK bytes, then step the LP machine until we must wait or we finish one message.
After :done, dispatches by response-type via `skg-response-handler-map'.
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
         (skg-lp--dispatch-by-type tcp-proc payload)
         (if (> (length skg-lp--buf) 0)
             nil ; continue loop -- more data may contain another LP message
           (cl-return nil)))

        ;; Hard error → reset state and signal.
        (`(:error ,msg)
         (setq skg-lp--buf                (unibyte-string)
               skg-lp--bytes-left         nil)
         (error "%s" msg)))))

(defun skg-lp--dispatch-by-type (tcp-proc payload)
  "Parse PAYLOAD as s-exp, extract response-type, and dispatch
via `skg-response-handler-map'.
Keys in the map and response-type values are symbols
because the sexp crate emits simple strings unquoted."
  (condition-case err
      (let* ((response (read payload))
             (type-entry (assoc 'response-type response))
             (response-type (cadr type-entry)))
        (if (not response-type)
            (message "skg: response missing response-type: %s"
                     (substring payload 0 (min 80 (length payload))))
          (let ((handler-entry (assoc response-type skg-response-handler-map)))
            (if (not handler-entry)
                (message "skg: no handler for response type: %s" response-type)
              (let ((handler  (cadr handler-entry))
                    (one-shot (cddr handler-entry)))
                (funcall handler tcp-proc payload)
                (when one-shot ;; It shot, so remove it. If instead the funcall errors, this 'when' statement will not fire, so the (stale? recoverable?) handler will not have been removed.
                  (setq skg-response-handler-map
                        (assoc-delete-all response-type
                                         skg-response-handler-map))
                  (setq skg-lp--pending-count
                        (max 0 (1- skg-lp--pending-count))))
                ) ))))
    (error
     (message "skg: dispatch error: %S for payload: %s"
              err (substring payload 0 (min 80 (length payload)))) )))

(defun skg-lp-step (buf bytes-left)
  "One pure(ish) step of the LP machine.
Inputs: BUF (unibyte accumulator), BYTES-LEFT (nil → need header; N → need N bytes).
Returns one of:
  (:need-more BUF LEFT)
  (:header LEN REMAINDER)
  (:done ORG-TEXT REMAINDER)
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
      (`(:done ,org-text ,remainder)   `(:done ,org-text ,remainder)))))

(defun skg-lp-append-chunk (buf chunk)
  "Return BUF with CHUNK (UTF-8 encoded bytes) appended."
  (let ((bytes (encode-coding-string chunk 'utf-8)) )
    (concat buf bytes)) )

(defun skg-lp-try-parse-header (response)
  "Extracts the length of the response bytes-so-far,
and the bytes-so-far itself, from `response`.
Returns one of:
  (:incomplete)
  (:ok LEN BYTES-SO-FAR)
  (:error MESSAGE)"
  (let ((sep (string-match "\r\n\r\n" response)) )
    (if (not sep)
        '(:incomplete)
      (let* ((header
              (substring response 0 (+ sep 4)) )
             (bytes-so-far ;; might be partial
              (substring response (+ sep 4)) )
             (len (and (string-match "Content-Length: \\([0-9]+\\)"
                                     header)
                       (string-to-number
                        (match-string 1 header)) )) )
        (if len
            (list :ok len bytes-so-far)
          (list :error
                "Malformed header in length-prefixed response")) )) ))

(defun skg-lp-try-consume-body (byte-acc bytes-left)
  "If BYTE-ACC contains BYTES-LEFT (or more, but it shouldn't),
then return (:done ORG-TEXT REMAINDER),
else return (:incomplete)."
  (let ((have (length byte-acc)) )
    (if (< have bytes-left)
        '(:incomplete)
      (let* ((payload-bytes (substring byte-acc 0 bytes-left))
             (remainder     (substring byte-acc bytes-left))
             (org-text      (decode-coding-string payload-bytes
                                                  'utf-8 t)) )
        (list :done org-text remainder)) )) )

(provide 'skg-length-prefix)
