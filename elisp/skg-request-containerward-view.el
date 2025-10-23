;;; -*- lexical-binding: t; -*-
;;; skg-request-containerward-view.el --- Request containerward view via save

;;; Commentary:
;; This function adds a containerward-view request to the current headline's
;; metadata and saves the buffer, allowing Rust to process the request
;; during completion.

;;; Code:

(require 'skg-metadata)
(require 'skg-request-save)

(defun skg-request-containerward-view ()
  "Add containerward-view request to current headline's metadata and save.
If point is in a headline body, navigates to the owning headline.
Merges '(skg (code (requests containerwardView)))' into existing metadata,
then saves the buffer to trigger Rust-side processing."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (skg-edit-metadata-at-point
     '(skg (code (requests containerwardView)))))
  (skg-request-save-buffer))

(provide 'skg-request-containerward-view)
;;; skg-request-containerward-view.el ends here
