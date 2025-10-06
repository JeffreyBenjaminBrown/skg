(require 'skg-length-prefix)

(defun skg-request-save-buffer ()
  "Send the current buffer contents to Rust for processing.
Rust will prepend a line and send the modified content back,
which will replace the current buffer contents."
  (interactive)
  (let* ((tcp-proc (skg-tcp-connect-to-rust))
         (buffer-contents (buffer-string))
         (request-s-exp (concat (prin1-to-string
                                 `((request . "save buffer")))
                                "\n"))
         (content-bytes (encode-coding-string buffer-contents 'utf-8))
         (content-length (length content-bytes))
         (header (format "Content-Length: %d\r\n\r\n" content-length)))

    ;; Prepare LP state and handler for the response
    (setq skg-lp--buf (unibyte-string)
          skg-lp--bytes-left nil
          skg-doc--response-handler
          (lambda (tcp-proc chunk)
            (skg-handle-save-chunk tcp-proc chunk)))

    ;; Send the request line first
    (process-send-string tcp-proc request-s-exp)

    ;; Send the length-prefixed buffer contents
    (process-send-string tcp-proc header)
    (process-send-string tcp-proc buffer-contents)))

(defun skg-handle-save-chunk (tcp-proc chunk)
  "Handle save response chunks with new format: success/failure indicator followed by optional length-prefixed content."
  ;; Append to buffer
  (setq skg-lp--buf (skg-lp-append-chunk skg-lp--buf chunk))

  ;; Check if we have enough data to determine success or failure
  (cond
   ((string-prefix-p "save: success\n" (decode-coding-string skg-lp--buf 'utf-8 t))
    ;; Success case - extract the length-prefixed content after "save: success\n"
    (let ((remaining (substring (decode-coding-string skg-lp--buf 'utf-8 t) 13)))
      ;; Reset LP state and handle the remaining as length-prefixed
      (setq skg-lp--buf (encode-coding-string remaining 'utf-8)
            skg-lp--bytes-left nil)
      (skg-lp-handle-generic-chunk
       (lambda (tcp-proc payload)
         (skg-replace-buffer-with-new-content tcp-proc payload)
         (setq skg-doc--response-handler nil))
       tcp-proc ""))) ; Empty chunk since we're processing buffered data

   ((string-prefix-p "save: failure\n" (decode-coding-string skg-lp--buf 'utf-8 t))
    ;; Failure case - extract the length-prefixed content after "save: failure\n"
    (let ((remaining (substring (decode-coding-string skg-lp--buf 'utf-8 t) 13)))
      ;; Reset LP state and handle the remaining as length-prefixed
      (setq skg-lp--buf (encode-coding-string remaining 'utf-8)
            skg-lp--bytes-left nil)
      (skg-lp-handle-generic-chunk
       (lambda (tcp-proc payload)
         (skg-show-save-errors payload)
         (setq skg-doc--response-handler nil))
       tcp-proc ""))) ; Empty chunk since we're processing buffered data

   ;; Not enough data yet or old format - wait for more
   (t nil)))

(defun skg-handle-save-response (_tcp-proc new-content)
  "Handle response from save buffer request, routing to success or error handling."
  (cond
   ((string-prefix-p "save: success\n" new-content)
    ;; Extract the actual content after the success prefix
    (let ((actual-content (substring new-content 13))) ; "save: success\n" is 13 chars
      (skg-replace-buffer-with-new-content _tcp-proc actual-content)))
   ((string-prefix-p "save: failure\n" new-content)
    ;; Extract the error content after the failure prefix
    (let ((error-content (substring new-content 13))) ; "save: failure\n" is 13 chars
      (skg-show-save-errors error-content)))
   (t
    ;; Fallback for old format - check for error content pattern
    (if (string-prefix-p "* NOTHING WAS SAVED" new-content)
        (skg-show-save-errors new-content)
      (skg-replace-buffer-with-new-content _tcp-proc new-content)))))

(defun skg-replace-buffer-with-new-content (_tcp-proc new-content)
  "Replace the current buffer contents with NEW-CONTENT from Rust."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert new-content)
    (set-buffer-modified-p nil
      ;; TODO: Use this in other places, too.
      ;; TODO: Don't call this here, but at the call site, because from the call site we can see whether the content was just saved (in which case buffer-modified should be nil) or perhaps was changed in some other way for which it should be t instead of nil.
      )
    (goto-char (point-min))
    (message "Buffer updated with processed content from Rust")))

(defun skg-show-save-errors (error-content)
  "Show save errors in a new buffer with org-mode."
  (let ((error-buffer-name "*SKG Save Errors - Inconsistencies Found*"))
    (with-current-buffer (get-buffer-create error-buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert error-content)
        (org-mode)
        (set-buffer-modified-p nil)
        (goto-char (point-min)))
      (display-buffer error-buffer-name)
      (message "Save failed - errors shown in %s" error-buffer-name))))

(provide 'skg-request-save)
