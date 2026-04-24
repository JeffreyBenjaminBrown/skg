;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Minor mode enabled in buffers visiting .skg files.
;;; Provides skg keybindings that operate on IDs (bare UUIDs,
;;; UUID.skg filenames, pid: lines, etc.) in the file.
;;; The keymap `skg-file-minor-mode-map' is defined in
;;; skg-keymaps-and-aliases.el alongside the other skg keymaps,
;;; so require that explicitly — the `:keymap' slot of
;;; `define-minor-mode' below consults the variable at load time.

(require 'skg-keymaps-and-aliases)
(require 'skg-id-search)

;;;###autoload
(define-minor-mode skg-file-minor-mode
  "Minor mode for buffers visiting .skg files.
Enables skg navigation keys like \\[skg-goto] on UUIDs
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
