;;; -*- lexical-binding: t; -*-
;;;
;;; USER-FACING FUNCTIONS
;;;   skg-request-save-buffer

(require 'skg-length-prefix)
(require 'skg-org-fold)
(require 'skg-focus)
(require 'skg-metadata)
(require 'skg-buffer)
(require 'skg-lock-buffers)

(defun skg-request-save-buffer ()
  "Send the current buffer contents to Rust for processing.
Before sending, adds 'folded' markers to folded headlines and 'focused' marker to current headline.
The server sends two LP messages:
  1. Early lock message with collateral view URIs.
  2. Full save response (content + errors + collateral updates).
All skg buffers are locked immediately; non-collateral buffers are
unlocked when the early response arrives."
  (interactive)
  ;; Add metadata markers before capturing buffer contents
  (skg-add-folded-markers)
  (skg-add-focused-marker)
  (let* ((tcp-proc (skg-tcp-connect-to-rust))
         (save-buffer (current-buffer))
         (saved-uri skg-view-uri)
         (buffer-contents (buffer-string))
         (request-s-exp (concat (prin1-to-string
                                 `((request . "save buffer")
                                   (view-uri . ,skg-view-uri)))
                                "\n"))
         (content-bytes (encode-coding-string buffer-contents 'utf-8))
         (content-length (length content-bytes))
         (header (format "Content-Length: %d\r\n\r\n" content-length)))

    ;; Lock ALL skg content-view buffers immediately, before sending.
    ;; This eliminates the race window between the send and the
    ;; server's early response.
    (skg--lock-all-skg-buffers)

    (setq
     skg-lp--buf (unibyte-string)
     skg-lp--bytes-left nil
     skg-lp--completion-handler
     (lambda (_tcp-proc payload)
       (skg--save-phase-1-handler saved-uri save-buffer payload))
     skg-doc--response-handler
     #'skg-lp-handle-generic-chunk)

    ;; Send the request line first
    (process-send-string tcp-proc request-s-exp)

    ;; Send the length-prefixed buffer contents
    (process-send-string tcp-proc header)
    (process-send-string tcp-proc buffer-contents)))

(defun skg--save-phase-1-handler (saved-uri save-buffer payload)
  "Auto-detecting handler for the first LP message of a save response.
If PAYLOAD contains 'lock-collateral-views', it is the early lock
message: unlock non-collateral buffers and install phase 2.
Otherwise it is a direct save response (e.g. server error before
the early message was sent): handle it as the phase 2 response.
(There are only two phases.)"
  (condition-case err
      (let* ((response (read payload))
             (collateral-entry (assoc 'lock-collateral-views response)))
        (if collateral-entry
            (let ((collateral-uris (cadr collateral-entry)))
              (skg--unlock-non-collateral-buffers
               saved-uri collateral-uris)
              (setq skg-lp--completion-handler
                    (lambda (_tcp-proc payload)
                      (skg--save-phase-2-handler
                       save-buffer payload)) ))
          (skg--save-phase-2-handler save-buffer payload)))
    (error
     (skg--unlock-all-save-locked)
     (setq skg-lp--completion-handler nil
           skg-doc--response-handler  nil)
     (message "ERROR in save phase 1 handler: %S" err)) ))

(defun skg--save-phase-2-handler (save-buffer payload)
  "Handle the full save response.
Unlocks all save-locked buffers, then processes the save response.
Unlock MUST happen before `skg-handle-save-sexp' because
`skg-replace-buffer-with-new-content' calls erase-buffer + insert,
which would trigger overlay modification-hooks if still present."
  (unwind-protect
      (progn
        (skg--unlock-all-save-locked)
        (with-current-buffer save-buffer
          (skg-handle-save-sexp payload)))
    (skg--unlock-all-save-locked)
    (setq skg-lp--completion-handler nil
          skg-doc--response-handler  nil)) )

(defun skg-handle-save-sexp (sexp-string)
  "Parse and handle save response s-exp: ((content ...) (errors (...)) (other-views-to-update ((URI_1 CONTENT_1) ...)))."
  (condition-case err
      (let* ((response (read sexp-string))
             (content-value         (cadr (assoc 'content response)))
             (errors-list           (cadr (assoc 'errors response)))
             (other-views-to-update (cadr (assoc 'other-views-to-update response))))
        (when ;; If content is not nil, update the saved buffer
            content-value
          (skg-replace-buffer-with-new-content nil content-value))
        (when other-views-to-update ;; Maybe update other views too
          (dolist (entry other-views-to-update)
            (let* ((uri (car entry))
                   (new-content (cadr entry))
                   (buf (skg-find-buffer-by-uri uri)))
              (when buf
                (with-current-buffer buf
                  (skg-replace-buffer-with-new-content
                   nil new-content)) )) ))
        (when errors-list ;; If there are errors, show them
          (let ((errors-text
                 (if (listp errors-list)
                     (mapconcat 'identity errors-list "\n\n")
                   errors-list)))
            (if content-value
                (skg-show-save-warnings ;; Success with warnings
                 errors-text)
              (skg-show-save-errors ;; Failure with errors
               errors-text)))))
    (error (message "ERROR parsing save response: %S" err)
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
