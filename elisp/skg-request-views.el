;;; -*- lexical-binding: t; -*-
;;;
;;; Functions for requesting views (aliases, containerward, sourceward, definitive)
;;; by adding metadata to the current headline and saving the buffer,
;;; allowing Rust to process the request during completion.

(require 'skg-metadata)
(require 'skg-request-save)
(require 'org)
(require 'org-element)

(defun skg-request-aliases-view ()
  "Edit metadata to request an aliases view for the headline at point."
  (interactive)
  (skg--request-view '(col aliases)))

(defun skg-request-containerward-view ()
  "Request containerward view and (to apply it) save."
  (interactive)
  (skg--request-view-and-save '(path container)))

(defun skg-request-sourceward-view ()
  "Request sourceward view and (to apply it) save."
  (interactive)
  (skg--request-view-and-save '(path linkSource)))

(defun skg-request-definitive-view ()
  "Edit metadata to request a definitive view for the headline at point.
The node must be indefinitive and childless."
  (interactive)
  (skg--request-view 'definitiveView))

(defun skg--request-view-and-save (view-token)
  "Request VIEW-TOKEN and immediately save."
  (skg--request-view view-token)
  (skg-request-save-buffer))

(defun skg--request-view
    (view-token) ;; '(col RELNAME), '(path ROLENAME), or 'definitiveView
  "Common helper to request VIEW-TOKEN for the headline at point.
Edits the metadata but does NOT save,
until the user calls `skg-request-save-buffer'."
  (save-excursion
    (org-back-to-heading t)
    (skg-edit-metadata-at-point
     `(skg (node (viewRequests ,view-token)))) ))

(provide 'skg-request-views)
