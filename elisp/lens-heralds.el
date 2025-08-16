;;; lensed-glyph.el --- Render <<...>> passages across buffer -*- lexical-binding: t; -*-

(defface lensed-glyph-blue-face
  '((t :foreground "white" :background "blue"))
  "White-on-blue for blue values.")

(defface lensed-glyph-green-face
  '((t :foreground "white" :background "#006400"))
  "White-on-green for green values.")

(defface lensed-glyph-red-face
  '((t :foreground "white" :background "red"))
  "White-on-red for REP (repeated).")

(defvar-local lensed-glyph--overlays nil
  "List of overlays created by `lensed-glyph-mode'.")

(defun lensed-glyph--parse (content)
  "Given CONTENT (string inside << >>), return a propertized glyph string.
Whitespace is ignored."
  (let* ((s (replace-regexp-in-string "[ \t\n]+" "" content))
         (parts (split-string s "," t))
         (out '()))
    (dolist (tok parts)
      (if (string-match ":" tok)
          (let* ((kv (split-string tok ":" t))
                 (k (car kv))
                 (v (cadr kv)))
            (cond
             ((string-equal k "id")
              (push (propertize "⅄" 'face 'lensed-glyph-green-face) out))
             ((string-equal k "blue")
              (push (propertize (or v "") 'face 'lensed-glyph-blue-face) out))
             ((string-equal k "green")
              (push (propertize (or v "") 'face 'lensed-glyph-green-face) out))))
        (when (string-equal tok "repeated")
          (push (propertize "REP" 'face 'lensed-glyph-red-face) out))))
    (when out
      (mapconcat #'identity (nreverse out) " "))))

(defun lensed-glyph--clear-overlays ()
  "Remove all overlays from buffer."
  (mapc #'delete-overlay lensed-glyph--overlays)
  (setq lensed-glyph--overlays nil))

(defun lensed-glyph--apply ()
  "Scan buffer for <<...>> and overlay them with glyph display."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "<<\\([^<>]*\\)>>" nil t)
      (let* ((beg (match-beginning 0))
             (end (match-end 0))
             (inner (match-string-no-properties 1))
             (glyphs (lensed-glyph--parse inner)))
        (when glyphs
          (let ((ov (make-overlay beg end)))
            (overlay-put ov 'display glyphs)
            (overlay-put ov 'evaporate t)
            (push ov lensed-glyph--overlays)))))))

;;;###autoload
(define-minor-mode lensed-glyph-mode
  "Toggle glyph rendering of <<...>> passages across the buffer.
- id: show “⅄”
- blue: show its value white-on-blue
- green: show its value white-on-green
- value \"repeated\": show “REP” white-on-red
Other keys/values ignored. Only display is changed."
  :lighter " ⟪Y⟫"
  (if lensed-glyph-mode
      (progn
        (lensed-glyph--apply)
        (add-hook 'after-change-functions #'lensed-glyph--after-change nil t))
    (remove-hook 'after-change-functions #'lensed-glyph--after-change t)
    (lensed-glyph--clear-overlays)))

(defun lensed-glyph--after-change (&rest _)
  "Refresh overlays after any buffer change."
  (when lensed-glyph-mode
    (lensed-glyph--clear-overlays)
    (lensed-glyph--apply)))

(provide 'lensed-glyph)
;;; lensed-glyph.el ends here
