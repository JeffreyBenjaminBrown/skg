;;; -*- lexical-binding: t; -*-

(require 'skg-buffer)

(defun skg-view-new-empty ()
  "Open a blank skg content view for creating new nodes."
  (interactive)
  (skg-open-empty-content-view))

(provide 'skg-view-new-empty)
