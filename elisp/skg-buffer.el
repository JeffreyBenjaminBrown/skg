;;; -*- lexical-binding: t; -*-
;;;
;;; USER-FACING FUNCTIONS
;;;   skg-open-empty-content-view

(defconst skg-content-view-buffer-name
  "*skg-content-view*")

(defun skg-open-empty-content-view ()
  "Open a new, empty skg content view buffer."
  (interactive)
  (skg-open-org-buffer-from-text
   nil "" skg-content-view-buffer-name))

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
