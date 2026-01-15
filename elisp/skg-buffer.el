;;; -*- lexical-binding: t; -*-
;;;
;;; USER-FACING FUNCTIONS
;;;   skg-open-empty-content-view

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
Returns nil if no headline is found."
  (when (and org-text (string-match "^\\*+ +\\(.+\\)$" org-text))
    (match-string 1 org-text)))

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

(defun skg-open-org-buffer-from-text (_tcp-proc org-text buffer-name)
  "Open a new buffer and insert ORG-TEXT, enabling org-mode."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert org-text)
        (org-mode))
      (set-buffer-modified-p nil)
      (goto-char (point-min)))
    (switch-to-buffer buffer)))

(provide 'skg-buffer)
