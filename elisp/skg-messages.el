;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Show what the server reports beside an answer: its errors
;;; and warnings as one org buffer, and a buffer of them that does not
;;; stop the user. Content views, saves, the diff view and the stream
;;; consumers all share these.

(require 'skg-buffer)              ; for skg--org-mode-with-options
(require 'skg-keymaps-and-aliases) ; for skg-report-mode-map

(defun skg-errors-and-warnings-to-org-string (errors warnings)
  "Convert ERRORS and WARNINGS to one org buffer with two sections."
  (let ((sections nil))
    (when (skg--message-list-nonempty-p errors)
      (push (concat "* errors\n"
                    (mapconcat (lambda (message)
                                 (concat "** " message))
                               errors "\n"))
            sections))
    (when (skg--message-list-nonempty-p warnings)
      (push (concat "* warnings\n"
                    (mapconcat (lambda (message)
                                 (concat "** " message))
                               warnings "\n"))
            sections))
    (mapconcat #'identity (nreverse sections) "\n")))

(defun skg--message-list-nonempty-p (message-list)
  "Return non-nil when MESSAGE-LIST has at least one message."
  (and (listp message-list)
       message-list))

(define-minor-mode skg-report-mode
  "Minor mode for read-only skg report buffers, such as an unknown node's
investigation and the import result: links in them can be followed,
and IDs used."
  :lighter " skg-report"
  :keymap skg-report-mode-map)

(defun skg-big-nonfatal-message (buffer-name message-text content)
  "Display CONTENT in BUFFER-NAME and show MESSAGE-TEXT in minibuffer.
Return the buffer."
  (with-current-buffer (get-buffer-create buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert content)
      (skg--org-mode-with-options)
      (set-buffer-modified-p nil)
      (goto-char (point-min)))
    (display-buffer buffer-name)
    (message "%s" message-text)
    (current-buffer)))

(provide 'skg-messages)
