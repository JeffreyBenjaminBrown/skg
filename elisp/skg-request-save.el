;;; -*- lexical-binding: t; -*-
;;;
;;; USER-FACING FUNCTIONS
;;;   skg-request-save-buffer

(require 'skg-log)
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
  (let ((focused-had-metadata ;; Whether the focused headline already has metadata. Storing this lets us clean up the bare (skg) that removal leaves behind.
         (save-excursion
           (org-back-to-heading t)
           (looking-at "\\*+ (skg"))))
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
      (progn ;; Rust needs these markers, but the user doesn't.
        (skg-remove-focused-marker)
        (skg-remove-folded-markers))
      (unless focused-had-metadata
        (skg-strip-bare-skg-at-focused-headline))

      (unless skg-view-uri
        ;; Guard: refuse to save when skg-view-uri is nil.
        ;; A nil view-uri causes an unfiltered save (all instructions
        ;; sent to TypeDB even if unchanged) AND the server won't update
        ;; its memory, so the work is both slow and wasted.
        (error "Cannot save: skg-view-uri is nil in buffer '%s' (content-view-mode=%s). Re-open the view."
               (buffer-name)
               (if (bound-and-true-p skg-content-view-mode) "on" "off")))

      ;; Lock ALL skg content-view buffers immediately, before sending.
      ;; This eliminates the race window between the send and the
      ;; server's early response.
      (skg--lock-all-skg-buffers)

      ;; Register handlers in the dispatch map
      (skg-register-response-handler
       'save-lock
       (lambda (_tcp-proc payload)
         (skg--save-lock-handler saved-uri save-buffer payload))
       t)
      (skg-register-response-handler
       'save-result
       (lambda (_tcp-proc payload)
         (skg--save-result-handler save-buffer payload))
       t)

      (skg-lp-reset)

      ;; Send the request line first
      (process-send-string tcp-proc request-s-exp)

      ;; Send the length-prefixed buffer contents
      (process-send-string tcp-proc header)
      (process-send-string tcp-proc buffer-contents))))

(defun skg-strip-bare-skg-at-focused-headline ()
  "Remove bare (skg) from the current headline if that is its only metadata.
Used after marker removal to clean up headlines that had no metadata
before the add/remove cycle."
  (save-excursion
    (org-back-to-heading t)
    (when (looking-at "\\(\\*+ \\)(skg) ")
      (replace-match "\\1"))))

(defun skg--save-lock-handler (saved-uri save-buffer payload)
  "Handle the save-lock LP message (tagged with response-type).
Unlocks non-collateral buffers."
  (condition-case err
      (let* ((response (read payload))
             (collateral-entry (assoc 'lock-collateral-views response)))
        (when collateral-entry
          (let ((collateral-uris (cadr collateral-entry)))
            (skg--unlock-non-collateral-buffers
             saved-uri collateral-uris))))
    (error
     (skg--unlock-all-save-locked)
     (skg-log 'error 'save "save-lock handler error: %S" err)) ))

(defun skg--save-result-handler (save-buffer payload)
  "Handle the full save-result LP message (tagged with response-type).
Unlocks all save-locked buffers, then processes the save response.
Unlock MUST happen before `skg-handle-save-sexp' because
`skg-replace-buffer-with-new-content' calls erase-buffer + insert,
which would trigger overlay modification-hooks if still present."
  (unwind-protect
      (progn
        (skg--unlock-all-save-locked)
        (with-current-buffer save-buffer
          (skg-handle-save-sexp payload)))
    (skg--unlock-all-save-locked)) )

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
    (error (skg-log 'error 'save "parsing save response: %S" err)
           (skg-log 'error 'save "sexp string was: %S" sexp-string))))

(defun skg-replace-buffer-with-new-content (_tcp-proc new-content)
  "Replace the current buffer contents with NEW-CONTENT from Rust.
After inserting content, folds marked headlines, removes fold markers,
moves point to focused headline, and removes focus marker."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert new-content)
    ;; Process folding markers
    (skg-fold-marked-headlines)
    (skg-remove-folded-markers)
    ;; Process focus marker
    (skg-goto-focused-headline)
    (skg-remove-focused-marker)
    ;; Clear modified flag and re-register the one-shot hook
    ;; AFTER all buffer modifications are done.
    (set-buffer-modified-p nil)
    (add-hook 'first-change-hook
              #'skg-warn-if-other-buffer-modified nil t)
    (message "Buffer updated with processed content from Rust")))

(defun skg-big-nonfatal-message (buffer-name message-text content)
  "Display CONTENT in BUFFER-NAME and show MESSAGE-TEXT in minibuffer."
  (with-current-buffer (get-buffer-create buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert content)
      (skg--org-mode-with-options)
      (set-buffer-modified-p nil)
      (goto-char (point-min)))
    (display-buffer buffer-name)
    (message "%s" message-text)))

(defun skg-show-save-errors (error-content)
  "Show save errors in a new buffer with org-mode."
  (skg-big-nonfatal-message
   "*SKG Save Errors - Inconsistencies Found*"
   "Save failed - errors shown in *SKG Save Errors - Inconsistencies Found*"
   error-content))

(defun skg-show-save-warnings (warning-content)
  "Show save warnings in a new buffer."
  (skg-big-nonfatal-message
   "*SKG Save Warnings*"
   "Save succeeded with warnings - see *SKG Save Warnings*"
   warning-content))

(provide 'skg-request-save)
