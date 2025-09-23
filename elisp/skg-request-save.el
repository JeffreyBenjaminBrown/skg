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
             #'skg-replace-buffer-with-new-content tcp-proc chunk)))

    ;; Send the request line first
    (process-send-string tcp-proc request-sexp)

    ;; Send the length-prefixed buffer contents
    (process-send-string tcp-proc header)
    (process-send-string tcp-proc buffer-contents)))

(defun skg-replace-buffer-with-new-content (_tcp-proc new-content)
  "Replace the current buffer contents with NEW-CONTENT from Rust."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert new-content)
    ( set-buffer-modified-p nil
      ;; TODO: Use this in other places, too.
      ;; TODO: Don't call this here, but at the call site, because from the call site we can see whether the content was just saved (in which case buffer-modified should be nil) or perhaps was changed in some other way for which it should be t instead of nil.
      )
    (goto-char (point-min))
    (message "Buffer updated with processed content from Rust")))

(provide 'skg-request-save)
