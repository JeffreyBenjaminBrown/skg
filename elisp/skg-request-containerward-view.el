;;; -*- lexical-binding: t; -*-
;;; skg-request-containerward-view.el --- Request containerward or sourceward view via save

;;; Commentary:
;; This function adds a containerward-view request to the current headline's
;; metadata and saves the buffer, allowing Rust to process the request
;; during completion.

;;; Code:

(require 'skg-metadata)
(require 'skg-request-save)
(require 'org)
(require 'org-element)

(defun skg-request-containerward-view ()
  "Request containerward view for the headline at point."
  (interactive)
  (skg--request-view 'containerwardView))

(defun skg-request-sourceward-view ()
  "Request sourceward view for the headline at point."
  (interactive)
  (skg--request-view 'sourcewardView))

(defun skg--request-view
    (view-token) ;; 'containerwardView or 'sourcewardView
  "Common helper to request VIEW-TOKEN for the headline at point."
  (progn
    (save-excursion
      (org-back-to-heading t)
      (skg-edit-metadata-at-point
       `(skg (code (requests ,view-token)))) )
    (skg-request-save-buffer)))

(provide 'skg-request-containerward-view)
(provide 'skg-request-sourceward-view)
;;; skg-request-containerward-view.el ends here
