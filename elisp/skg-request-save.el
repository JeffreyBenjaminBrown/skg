;;; -*- lexical-binding: t; -*-

(require 'skg-length-prefix)
(require 'skg-org-fold)
(require 'skg-focus)
(require 'skg-metadata)

(defun skg-request-save-buffer ()
  "Send the current buffer contents to Rust for processing.
Before sending, adds 'folded' markers to folded headlines and 'focused' marker to current headline.
Rust will prepend a line and send the modified content back,
which will replace the current buffer contents."
  (interactive)
  ;; Add metadata markers before capturing buffer contents
  (skg-add-folded-markers)
  (skg-add-focused-marker)
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
            (skg-lp-handle-generic-chunk
             (lambda (_tcp-proc payload)
               (skg-handle-save-sexp payload)
               (setq skg-doc--response-handler nil))
             tcp-proc chunk)))

    ;; Send the request line first
    (process-send-string tcp-proc request-s-exp)

    ;; Send the length-prefixed buffer contents
    (process-send-string tcp-proc header)
    (process-send-string tcp-proc buffer-contents)))

(defun skg-handle-save-sexp (sexp-string)
  "Parse and handle save response s-exp: ((content ...) (errors ...))."
  (condition-case err
      (let* ((response (read sexp-string))
             (content-pair (assoc 'content response))
             (errors-pair (assoc 'errors response))
             (content-value (cadr content-pair))
             (errors-list (cadr errors-pair)))
        ;; If content is not nil, update the buffer
        (when content-value
          (skg-replace-buffer-with-new-content nil content-value))
        ;; If there are errors, show them
        (when errors-list
          (let ((errors-text (if (listp errors-list)
                                 (mapconcat 'identity errors-list "\n\n")
                               errors-list)))
            (if content-value
                ;; Success with warnings
                (skg-show-save-warnings errors-text)
              ;; Failure with errors
              (skg-show-save-errors errors-text)))))
    (error
     (message "ERROR parsing save response: %S" err)
     (message "Sexp string was: %S" sexp-string))))

(defun skg-replace-buffer-with-new-content (_tcp-proc new-content)
  "Replace the current buffer contents with NEW-CONTENT from Rust.
After inserting content, folds marked headlines, removes fold markers,
moves point to focused headline, and removes focus marker."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert new-content)
    (set-buffer-modified-p nil
      ;; TODO: Use this in other places, too.
      ;; TODO: Don't call this here, but at the call site, because from the call site we can see whether the content was just saved (in which case buffer-modified should be nil) or perhaps was changed in some other way for which it should be t instead of nil.
      )
    ;; Process folding markers
    (skg-fold-marked-headlines)
    (skg-remove-folded-markers)
    ;; Process focus marker
    (skg-goto-focused-headline)
    (skg-remove-focused-marker)
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

(defun skg-show-save-warnings (warning-content)
  "Show save warnings in a new buffer."
  (let ((warning-buffer-name "*SKG Save Warnings*"))
    (with-current-buffer (get-buffer-create warning-buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert warning-content)
        (org-mode)
        (set-buffer-modified-p nil)
        (goto-char (point-min)))
      (display-buffer warning-buffer-name)
      (message "Save succeeded with warnings - see %s" warning-buffer-name))))

(provide 'skg-request-save)
