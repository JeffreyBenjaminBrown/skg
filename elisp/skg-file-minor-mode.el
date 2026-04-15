;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Minor mode enabled in buffers visiting .skg files.
;;; Provides skg keybindings that operate on IDs (bare UUIDs,
;;; UUID.skg filenames, pid: lines, etc.) in the file.
;;; All the action (so far) is in the definition of
;;;   skg-file-minor-mode-map.
;;;
;;; USER-FACING FUNCTIONS
;;;   skg-file-minor-mode

(require 'skg-id-search)

(defvar skg-file-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c v v") #'skg-view)
    map)
  "Keymap for `skg-file-minor-mode'.")

;;;###autoload
(define-minor-mode skg-file-minor-mode
  "Minor mode for buffers visiting .skg files.
Enables skg navigation keys like \\[skg-view] on UUIDs
and .skg filenames."
  :lighter " skg"
  :keymap skg-file-minor-mode-map)

(defun skg-file-minor-mode--maybe-enable ()
  "Enable `skg-file-minor-mode' if the current buffer visits a .skg file."
  (when (and buffer-file-name
             (string-match-p "\\.skg\\'" buffer-file-name))
    (skg-file-minor-mode 1)))

(add-hook 'find-file-hook #'skg-file-minor-mode--maybe-enable)

(provide 'skg-file-minor-mode)
