;;; -*- lexical-binding: t; -*-
;;;
;;; Functions for requesting views (aliases, containerward, sourceward)
;;; by adding metadata to the current headline and saving the buffer,
;;; allowing Rust to process the request during completion.
;;;
;;; USER-FACING FUNCTIONS
;;;   skg-request-aliases-view ()
;;;   skg-request-containerward-view ()
;;;   skg-request-sourceward-view ()


(require 'skg-metadata)
(require 'skg-request-save)
(require 'org)
(require 'org-element)

(defun skg-request-aliases-view ()
  "Edit metadata to request an aliases view for the headline at point."
  (interactive)
  (skg--request-view 'aliases))

(defun skg-request-containerward-view ()
  "Edit metadata to request a containerward view for the headline at point." ;
  (interactive)
  (skg--request-view 'containerwardView))

(defun skg-request-sourceward-view ()
  "Edit metadata to request a sourceward view for the headline at point."
  (interactive)
  (skg--request-view 'sourcewardView))

(defun skg--request-view
    (view-token) ;; 'aliases, 'containerwardView, or 'sourcewardView
  "Common helper to request VIEW-TOKEN for the headline at point.
Edits the metadata but does NOT save,
until the user calls `skg-request-save-buffer'."
  (save-excursion
    (org-back-to-heading t)
    (skg-edit-metadata-at-point
     `(skg (code (viewRequests ,view-token)))) ))

(provide 'skg-request-views)
