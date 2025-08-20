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

(defun skg-handle-length-prefixed-org (proc chunk)
  "Accumulate CHUNK from PROC.
Parse a Content-Length frame.
Open the resulting org-mode buffer."

  (ignore proc)
  (let ;; Encode CHUNK as UTF-8 bytes and accumulate so counts match Content-Length.
      ((bytes (encode-coding-string chunk 'utf-8)) )
    (setq skg-lp--buf (concat skg-lp--buf bytes)) )
  (catch 'again
    (while t
      (if (not skg-lp--bytes-left)
          (let ;; Expecting header: look for \r\n\r\n
              ((sep (string-match "\r\n\r\n" skg-lp--buf)))
            (if (not sep)
                (throw 'again nil)
              (let* ((header (substring skg-lp--buf 0 (+ sep 4)))
                     (rest   (substring skg-lp--buf (+ sep 4)))
                     (len    (and (string-match "Content-Length: \\([0-9]+\\)" header)
                                  (string-to-number (match-string 1 header)) )) )
                (unless len ;; TODO `if else` would read more naturally than `unless (and otherwise keep evaluating)`.
                  (setq skg-lp--buf (unibyte-string)
                        skg-lp--bytes-left nil
                        skg-doc--response-handler nil)
                  (error "Malformed header in length-prefixed response"))
                (setq skg-lp--bytes-left len
                      skg-lp--buf  rest)) ))
        (let ;; Expecting body bytes
            ((have (length skg-lp--buf)))
          (when (< have skg-lp--bytes-left)
            (throw 'again nil))
          (let* ((payload-bytes (substring skg-lp--buf 0 skg-lp--bytes-left))
                 (remainder     (substring skg-lp--buf skg-lp--bytes-left))
                 (org-text      (decode-coding-string payload-bytes 'utf-8 t)))
            ;; Display and reset for next request
            (setq skg-lp--buf  remainder
                  skg-lp--bytes-left nil)
            (skg-open-org-buffer-from-rust-org nil org-text)
            ;; We consider one response per request; clear handler.
            (setq skg-doc--response-handler nil)
            (throw 'again nil)))))))

(defun skg-doc-disconnect ()
  "Manually close the connection to the Rust server."
  (interactive)
  (when (process-live-p skg-rust-tcp-proc)
    (delete-process skg-rust-tcp-proc)
    (setq skg-rust-tcp-proc nil )) )

(provide 'client)
