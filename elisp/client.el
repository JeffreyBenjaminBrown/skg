;; For instructions see USAGE.md, in this same folder.

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

;; TODO: Rewrite this nightmare without throw-catch or flags.
(defun skg-handle-length-prefixed-org (proc chunk)
  "Accumulate CHUNK from PROC, parse a Content-Length frame, and open an org buffer containing it."

  (ignore proc)
  (progn
    ;; Encode CHUNK as UTF-8 bytes and append to our byte accumulator.
    ;; Content-Length is in BYTES, so we must accumulate bytes, not chars.
    (setq skg-lp--buf (skg-lp-append-chunk skg-lp--buf chunk)))
  (catch 'again
    ;; Tiny state machine: keep parsing as long as we can.
    ;; Exit the loop (and return from the filter) with (throw 'again nil)
    ;; either when we need more bytes OR after we finish one full message.
    (while t
      (if (not skg-lp--bytes-left)
          (let* ((res ;; (:incomplete) | (:error MSG) | (:ok LEN BODY)
                  (skg-lp-try-parse-header skg-lp--buf))
                 (parsed-label (nth 0 res)))
            (progn
              (cond
               ((eq parsed-label :incomplete)
                (progn ;; Header delimiter not found; await more bytes.
                  (throw 'again nil)))
               ((eq parsed-label :error)
                (progn ;; Malformed header. Clear accumulator/state, drop handler to avoid misrouting, and signal error.
                  (setq skg-lp--buf (unibyte-string)
                        skg-lp--bytes-left nil
                        skg-doc--response-handler nil)
                  (error "%s" (nth 1 res))))
               ((eq parsed-label :ok)
                (progn
                  (setq skg-lp--bytes-left (nth 1 res)
                        skg-lp--buf        (nth 2 res)))))))
        (let* ((res ;; (:incomplete) | (:done ORG-TEXT REMAINDER)
                (skg-lp-try-consume-body
                 skg-lp--buf skg-lp--bytes-left))
               (consume-label (nth 0 res)))
          (progn
            (cond
             ((eq consume-label :incomplete)
              (progn ;; Await more bytes.
                (throw 'again nil)))
             ((eq consume-label :done)
              (let ((org-text (nth 1 res))
                    (remainder (nth 2 res)))
                (progn ;; Completion.
                  (setq skg-lp--buf        remainder
                        skg-lp--bytes-left nil)
                  (skg-open-org-buffer-from-rust-org nil org-text)
                  (setq ;; One response per request: clear handler so stray bytes won't call us again for this request.
                   skg-doc--response-handler nil)
                  ;; Stop parsing now. Any leftover bytes (e.g., start of next message) stay in the accumulator.
                  (throw 'again nil)) )) )) )) )) )

(defun skg-lp-append-chunk (buf chunk)
  "Return BUF with CHUNK (UTF-8 encoded bytes) appended."
  (let ((bytes (encode-coding-string chunk 'utf-8)) )
    (concat buf bytes)) )

(defun skg-lp-try-parse-header (response)
  "Extracts the length of the response bytes-so-far, and the bytes-so-far itself, from `response`.
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

(provide 'client)
