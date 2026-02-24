;;; -*- lexical-binding: t; -*-
;;;
;;; USER-FACING FUNCTIONS
;;;   skg-open-empty-content-view

(require 'org-id)
(require 'cl-lib)
(require 'skg-sexpr-search)

(defvar-local skg-view-uri nil
  "Unique view URI for this skg buffer.")
(put 'skg-view-uri
     'permanent-local ; to survive major-mode changes
     t)

(defconst skg-fallback-buffer-name
  "*skg*")

(defun skg-content-view-buffer-name (org-text)
  "Generate buffer name for content view from ORG-TEXT."
  (let ((title (skg-extract-top-headline-title org-text)))
    (if title
        (concat "*skg: " (skg-sanitize-buffer-name title) "*")
      skg-fallback-buffer-name)))

(defun skg-search-buffer-name (search-terms)
  "Generate buffer name for title search with SEARCH-TERMS."
  (concat "*skg: search: " (skg-sanitize-buffer-name search-terms) "*"))

(defun skg-extract-top-headline-title (org-text)
  "Extract the title from the first headline in ORG-TEXT.
Strips any leading (skg ...) metadata from the title.
Returns nil if no headline is found."
  (when (and org-text (string-match "^\\*+ +\\(.+\\)$" org-text))
    (let ((after-stars (match-string 1 org-text)))
      (if (string-prefix-p "(skg" after-stars)
          (let ((sexp-end-pos (skg-find-sexp-end after-stars)))
            (if sexp-end-pos
                (string-trim (substring after-stars sexp-end-pos))
              after-stars))
        after-stars))))

(defun skg-sanitize-buffer-name (name)
  "Sanitize NAME for use as a buffer name.
Removes null characters and newlines, trims whitespace,
and truncates to a reasonable length."
  (let* ((no-nulls (replace-regexp-in-string "\0" "" name))
         (no-newlines (replace-regexp-in-string "[\n\r]" " " no-nulls))
         (trimmed (string-trim no-newlines))
         (max-len 80))
    (if (> (length trimmed) max-len)
        (concat (substring trimmed 0 (- max-len 3)) "...")
      trimmed)))

(defun skg-open-empty-content-view ()
  "Open a new, empty skg content view buffer."
  (interactive)
  (skg-open-org-buffer-from-text
   nil "" skg-fallback-buffer-name))

(defun skg-open-org-buffer-from-text (_tcp-proc org-text buffer-name &optional view-uri)
  "Open a new buffer and insert ORG-TEXT, enabling org-mode.
If VIEW-URI is provided, set it as the buffer's skg-view-uri;
otherwise generate a new UUID."
  (let ((buffer (get-buffer-create buffer-name))
        (uri (or view-uri (org-id-uuid))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert org-text)
        (org-mode))
      (setq skg-view-uri uri)
      (add-hook 'kill-buffer-hook #'skg-send-close-view nil t)
      (add-hook 'first-change-hook
                #'skg-warn-if-other-buffer-modified nil t)
      (set-buffer-modified-p nil)
      (goto-char (point-min)))
    (switch-to-buffer buffer)))

(defun skg-send-close-view ()
  "Send a close-view message to the server for this buffer's view URI."
  (when (and skg-view-uri
             (boundp 'skg-rust-tcp-proc)
             skg-rust-tcp-proc
             (process-live-p skg-rust-tcp-proc))
    (let ((request (concat (prin1-to-string
                            `((request . "close view")
                              (view-uri . ,skg-view-uri)))
                           "\n")))
      (process-send-string skg-rust-tcp-proc request))))

(defun skg-warn-if-other-buffer-modified ()
  "Warn if another skg buffer has unsaved modifications."
  (let ((other-modified
         (cl-some (lambda (buf)
                    (and (not (eq buf (current-buffer)))
                         (buffer-local-value 'skg-view-uri buf)
                         (buffer-modified-p buf)))
                  (buffer-list))))
    (when other-modified
      (message "WARNING: Another skg buffer has unsaved modifications. Saving is ill-defined when multiple buffers have unsaved edits.")) ))

(defun skg-find-buffer-by-uri (uri)
  "Find the buffer whose skg-view-uri matches URI."
  (cl-find-if (lambda (buf)
                (string= uri (buffer-local-value 'skg-view-uri buf)))
              (buffer-list)))

(provide 'skg-buffer)
