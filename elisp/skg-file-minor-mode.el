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
(require 'skg-buffer-registry)
(require 'skg-id-search)
(require 'skg-worktree-guard)

;;;###autoload
(define-minor-mode skg-file-minor-mode
  "Minor mode for buffers visiting .skg files.
Enables skg navigation keys like \\[skg-goto] on UUIDs
and .skg filenames.  The server-owned watcher observes a plain save."
  :lighter " skg"
  :keymap skg-file-minor-mode-map
  (if skg-file-minor-mode
      (progn
        (add-hook 'before-save-hook #'skg--guard-raw-skg-save nil t)
        (skg-register-raw-file-buffer-if-configured (current-buffer)))
    (remove-hook 'before-save-hook #'skg--guard-raw-skg-save t)
    (when (and skg--buffer-record
               (eq (skg--buffer-record-kind skg--buffer-record)
                   'raw-skg-file))
      (skg-unregister-current-buffer))))

(defun skg-register-raw-file-buffer-if-configured (&optional buffer)
  "Register BUFFER when it visits a direct child of a configured source."
  (with-current-buffer (or buffer (current-buffer))
    (when (and buffer-file-name
               (skg--configured-skg-file-p buffer-file-name)
               (not (and skg--buffer-record
                         (eq (skg--buffer-record-kind skg--buffer-record)
                             'raw-skg-file))))
      (skg-register-buffer
       (current-buffer) 'raw-skg-file
       :lifecycle 'ordinary-file :disposable nil
       :recipe `((kind . "raw-skg-file")
                 (name . ,(file-name-nondirectory buffer-file-name)))
       :last-fetched (skg-buffer-raw-text)))))

(defun skg-register-open-raw-file-buffers ()
  "Register configured raw files which predate verified source inventory."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (skg-register-raw-file-buffer-if-configured buffer))))

(defun skg-file-minor-mode--maybe-enable ()
  "Enable `skg-file-minor-mode' if the current buffer visits a .skg file."
  (when (and buffer-file-name
             (string-match-p "\\.skg\\'" buffer-file-name))
    (skg-file-minor-mode 1)))

(add-hook 'find-file-hook #'skg-file-minor-mode--maybe-enable)

(provide 'skg-file-minor-mode)
