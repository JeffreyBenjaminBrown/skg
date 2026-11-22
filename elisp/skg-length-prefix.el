;;; skg-length-prefix.el --- Split server replies, dispatch each by type  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; Every answer from the server passes through here on its way to the
;;; command that asked for it. 'skg-lp-handle-generic-chunk' (which
;;; 'skg-handle-rust-response' in 'skg-client.el' calls with each chunk
;;; read) splits the byte stream into messages, each a
;;; "Content-Length: N" header and N bytes of sexp;
;;; 'skg-lp--dispatch-by-type' hands each to the handler that
;;; 'skg-response-handler-map' holds for its response-type, or else
;;; 'skg-unprompted-response-handler-map' (both in 'skg-state.el').
;;; Registering the handlers is each command's own.
;;; 'skg-tell-answer-not-applied' tells the user a handler failed, and
;;; 'skg--register-overPrivateText-confirmation-instead-of' asks before
;;; a request shows overPrivateText telescope text.
;;;
;;; The handler maps are keyed by symbols because the server's sexp
;;; crate prints simple strings, response-types among them, unquoted.
;;;
;;; This file does not add length prefixes to outgoing messages. That
;;; is easier, and done inline where messages are sent (as in
;;; 'skg-request-save.el').
;;;
;;; Code:

(require 'skg-log)
(require 'skg-state)

(defconst skg-answer-not-applied-default-consequence
  "the server may have saved, but the buffer was not updated"
  "What `skg-tell-answer-not-applied' says happened, unless told otherwise.")

(defun skg-tell-answer-not-applied (answer err &optional consequence)
  "Say in the echo area that applying the server's ANSWER (its
response type) failed with ERR, and what that means: CONSEQUENCE, by
default `skg-answer-not-applied-default-consequence'.
A handler that catches an error while applying an answer calls this as
well as `skg-log', which is off by default (`skg-log-file'): an error
only logged leaves the user thinking the command did nothing.
This lives here, not in skg-log.el, because `skg-reload' keeps
`skg-log' as first loaded, so a function new to that file would be
void until Emacs restarts."
  (message "%s"
           (format "skg: %s: applying the server's %s answer failed (%s)"
                   (or consequence skg-answer-not-applied-default-consequence)
                   answer
                   (error-message-string err))))

(defun skg--when-overPrivateText-approved (payload on-approval)
  "Ask the prompt of PAYLOAD, an overPrivateText-telescope-confirmation,
and if the user approves, call ON-APPROVAL with the PIDs it lists, as
strings: the telescopes whose text the retried request may show."
  (let* ((response (read payload))
         (prompt (format "%s" (cadr (assoc 'prompt response))))
         (pids (mapcar (lambda (pid) (format "%s" pid))
                       (cadr (assoc 'pids response)))))
    (when (y-or-n-p (concat prompt " "))
      (funcall on-approval pids))))

(defun skg--register-overPrivateText-confirmation-instead-of (answer-type retry)
  "Register the handler of an overPrivateText-telescope-confirmation,
which the server sends instead of ANSWER-TYPE, a one-shot answer, when
that answer would show text from overPrivateText telescopes. It is not
one-shot, so only ANSWER-TYPE counts toward the pending responses.
When it fires, both handlers are removed (balancing ANSWER-TYPE's
count), and if the user approves (`skg--when-overPrivateText-approved'),
RETRY is called with the TCP-PROC that brought the confirmation and
the PIDs approved."
  (skg-register-response-handler
   'overPrivateText-telescope-confirmation
   (lambda (tcp-proc payload)
     (skg--unregister-response-handlers
      (list 'overPrivateText-telescope-confirmation answer-type))
     (skg--when-overPrivateText-approved
      payload
      (lambda (pids) (funcall retry tcp-proc pids))))
   nil))

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
via `skg-response-handler-map', or else, for a message the server
sends unprompted, `skg-unprompted-response-handler-map'.
Keys in the map and response-type values are symbols
because the sexp crate emits simple strings unquoted."
  (condition-case err
      (let* ((response (read payload))
             (type-entry (assoc 'response-type response))
             (response-type (cadr type-entry)))
        (if (not response-type)
            (skg-log 'warn 'dispatch "response missing response-type: %s"
                     (substring payload 0 (min 80 (length payload))))
          (let ((handler-entry (assoc response-type skg-response-handler-map))
                (unprompted-handler
                 (cdr (assoc response-type skg-unprompted-response-handler-map))))
            (cond
             ((not handler-entry)
              (if unprompted-handler
                  (funcall unprompted-handler tcp-proc payload)
                (skg-log 'warn 'dispatch "no handler for response type: %s" response-type)))
             (t
              (let ((handler  (cadr handler-entry))
                    (one-shot (cddr handler-entry)))
                (funcall handler tcp-proc payload)
                (when (and (memq response-type
                                 '(save-result rerender-done skgrepo-restriction
                                   delete-references-result
                                   rebuild-session-lifetime-stores))
                           (fboundp 'skg-link-annotations-invalidate-all))
                  (skg-link-annotations-invalidate-all))
                (when one-shot ;; It shot, so remove it. If instead the funcall errors, this 'when' statement will not fire, so the (stale? recoverable?) handler will not have been removed.
                  (setq skg-response-handler-map
                        (assoc-delete-all response-type
                                         skg-response-handler-map))
                  (setq skg-lp--pending-count
                        (max 0 (1- skg-lp--pending-count))))
                ) ))) ))
    (error
     (skg-log 'error 'dispatch "dispatch error: %S for payload: %s"
              err (substring payload 0 (min 80 (length payload))))
     (skg-tell-answer-not-applied
      (or (ignore-errors
            (cadr (assoc 'response-type (read payload))))
          "unreadable")
      err
      "the server may have acted (saved, say), but the client did not finish handling its answer") )))

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
  (let ((bytes
         (if (multibyte-string-p chunk)
             (encode-coding-string chunk 'utf-8)
           chunk)) )
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
