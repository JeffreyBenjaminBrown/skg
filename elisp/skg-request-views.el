;;; -*- lexical-binding: t; -*-

;; Functions for requesting views (aliases, containerward, sourceward)
;; by adding metadata to the current headline and saving the buffer,
;; allowing Rust to process the request during completion.

(require 'skg-metadata)
(require 'skg-request-save)
(require 'org)
(require 'org-element)

(defun skg-request-aliases-view ()
  "Request aliases view for the headline at point."
  (interactive)
  (skg--request-view 'aliases))

(defun skg-request-containerward-view ()
  "Request containerward view for the headline at point."
  (interactive)
  (skg--request-view 'containerwardView))

(defun skg-request-sourceward-view ()
  "Request sourceward view for the headline at point."
  (interactive)
  (skg--request-view 'sourcewardView))

(defun skg--request-view
    (view-token) ;; 'aliases, 'containerwardView, or 'sourcewardView
  "Common helper to request VIEW-TOKEN for the headline at point."
  (progn
    (save-excursion
      (org-back-to-heading t)
      (skg-edit-metadata-at-point
       `(skg (code (viewRequests ,view-token)))) )
    (skg-request-save-buffer)))

(provide 'skg-request-views)
