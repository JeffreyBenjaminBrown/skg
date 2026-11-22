;;; skg-file-minor-mode.el --- skg ID keys in buffers visiting .skg files  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; 'skg-file-minor-mode', which 'find-file-hook' turns on in every
;;; buffer visiting a .skg file ('skg-file-minor-mode--maybe-enable'),
;;; lets the user act on the IDs in the raw file (bare UUIDs, UUID.skg
;;; filenames): visit one ('skg-goto', C-c g RET), hop between them
;;; ('skg-id-next', C-c i n), and paste or pop the linkstack (C-c o,
;;; C-c O). What those commands do is 'skg-id-search.el's and
;;; 'skg-linkstack.el's.
;;;
;;; The keymap 'skg-file-minor-mode-map' is defined in
;;; 'skg-keymaps-and-aliases.el' alongside the other skg keymaps, so
;;; this file requires that one explicitly: the ':keymap' slot of
;;; 'define-minor-mode' below consults the variable at load time.
;;;
;;; Code:

(require 'skg-keymaps-and-aliases)
(require 'skg-id-search)
(require 'skg-linkstack)

;;;###autoload
(define-minor-mode skg-file-minor-mode
  "Minor mode for buffers visiting .skg files.
Enables skg navigation keys like \\[skg-goto] on UUIDs
and .skg filenames."
  :lighter " skg-file"
  :keymap skg-file-minor-mode-map)

(defun skg-file-minor-mode--maybe-enable ()
  "Enable `skg-file-minor-mode' if the current buffer visits a .skg file."
  (when (and buffer-file-name
             (string-match-p "\\.skg\\'" buffer-file-name))
    (skg-file-minor-mode 1)))

(add-hook 'find-file-hook #'skg-file-minor-mode--maybe-enable)

(provide 'skg-file-minor-mode)
