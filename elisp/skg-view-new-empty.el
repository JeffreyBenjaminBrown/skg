;;; skg-view-new-empty.el --- Open a blank content view  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; 'skg-view-new-empty' (C-c v e) opens a blank skg content view,
;;; *skg-empty* (or *skg-empty*<N> when that name is taken), for
;;; creating new nodes; saving it creates them. Its baseline is the
;;; empty text it opens with. Setting up the buffer is 'skg-buffer.el's
;;; ('skg-open-empty-content-view').
;;;
;;; Code:

(require 'skg-buffer)

(defun skg-view-new-empty ()
  "Open a blank skg content view for creating new nodes. Its baseline
is the empty text it opens with."
  (interactive)
  (skg-open-empty-content-view))

(provide 'skg-view-new-empty)
