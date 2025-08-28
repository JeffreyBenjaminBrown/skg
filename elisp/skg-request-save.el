(require 'skg-length-prefix)

(defun skg-request-save-buffer ()
  "Send the current buffer contents to Rust for processing.
Rust will prepend a line and send the modified content back,
which will replace the current buffer contents."
  (interactive)
  (let* ((tcp-proc (skg-tcp-connect-to-rust))
         (buffer-contents (buffer-string))
         (request-sexp (format "((request . \"save buffer\"))\n"))
         (content-bytes (encode-coding-string buffer-contents 'utf-8))
         (content-length (length content-bytes))
         (header (format "Content-Length: %d\r\n\r\n" content-length)))

    ;; Prepare LP state and handler for the response
    (setq skg-lp--buf (unibyte-string)
          skg-lp--bytes-left nil
          skg-doc--response-handler
          (lambda (tcp-proc chunk)
            (skg-lp-handle-generic-chunk
             #'skg-replace-buffer-with-processed-content tcp-proc chunk)))

    ;; Send the request line first
    (process-send-string tcp-proc request-sexp)

    ;; Send the length-prefixed buffer contents
    (process-send-string tcp-proc header)
    (process-send-string tcp-proc buffer-contents)))

(defun skg-replace-buffer-with-processed-content (_tcp-proc processed-content)
  "Replace the current buffer contents with PROCESSED-CONTENT from Rust."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert processed-content)
    (set-buffer-modified-p t)  ; Mark as modified since content changed
    (goto-char (point-min))
    (message "Buffer updated with processed content from Rust")))

(provide 'skg-request-save)
