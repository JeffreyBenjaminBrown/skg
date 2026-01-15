;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Interactive s-expression editing utilities.
;;;
;;; User-facing functions:
;;;   skg-edit-first-sexpr-on-line
;;;   skg-edit-sexp-at-or-after-point

(require 'skg-sexpr-org-bijection)
(require 'skg-sexpr-search)

;;
;; Buffer-local variables for the edit buffer
;;

(defvar-local skg-edit--source-buffer nil
  "The buffer containing the sexp being edited.")

(defvar-local skg-edit--sexp-start nil
  "Start position of the sexp in the source buffer.")

(defvar-local skg-edit--sexp-end nil
  "End position of the sexp in the source buffer.")

;;
;; Minor mode for the edit buffer
;;

(defvar skg-sexpr-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'skg-edit--commit)
    map)
  "Keymap for skg-sexpr-edit-mode.")

(define-minor-mode skg-sexpr-edit-mode
  "Minor mode for editing sexps as org text.
\\<skg-sexpr-edit-mode-map>
\\[skg-edit--commit] to save changes back to the source buffer.
Kill the buffer to cancel without saving."
  :lighter " SExp-Edit"
  :keymap skg-sexpr-edit-mode-map)

;;
;; Core implementation
;;

(defun skg-edit--commit ()
  "Save changes from org buffer back to the source sexp."
  (interactive)
  (let* ((org-text (buffer-substring-no-properties (point-min) (point-max)))
         (new-sexp (org-to-sexp org-text))
         (new-text (prin1-to-string new-sexp))
         (source-buffer skg-edit--source-buffer)
         (start skg-edit--sexp-start)
         (end skg-edit--sexp-end))
    (kill-buffer)
    (switch-to-buffer source-buffer)
    (goto-char start)
    (delete-region start end)
    (insert new-text)))

(defconst skg-edit--help-text
  "Press 'C-c C-c' to save changes, or 'C-x k' to cancel.\n\n"
  "Help text shown at the top of the edit buffer.")

;;
;; User-facing functions
;;

(defun skg-edit-first-sexpr-on-line ()
  "Edit the first sexp on the current line in an org buffer.
Opens a temporary org buffer with the sexp converted to org headlines.
Use C-c C-c to save changes back to the source buffer.
Kill the buffer to cancel without saving."
  (interactive)
  (skg-edit-sexp-with-finder #'skg-first-sexpr-on-line))

(defun skg-edit-sexp-at-or-after-point ()
  "Edit the sexp at or after point in an org buffer.
Opens a temporary org buffer with the sexp converted to org headlines.
Use C-c C-c to save changes back to the source buffer.
Kill the buffer to cancel without saving."
  (interactive)
  (skg-edit-sexp-with-finder #'skg-sexp-at-or-after-point))

(defun skg-edit-sexp-with-finder (finder-fn)
  "Edit a sexp found by FINDER-FN in an org buffer.
FINDER-FN should be a function that returns a sexp from the current
buffer, leaving point at the end of the sexp."
  (let* ((source-buffer (current-buffer))
         (sexp (funcall finder-fn)))
    (unless sexp
      (error "No sexp found"))
    (let* ((sexp-end (point))
           (sexp-start (save-excursion (backward-sexp 1) (point)))
           (org-text (sexp-to-org sexp))
           (edit-buffer (generate-new-buffer "*skg-edit*")))
      (switch-to-buffer edit-buffer)
      (insert skg-edit--help-text)
      (insert org-text)
      (goto-char (point-min))
      (org-mode)
      (skg-sexpr-edit-mode 1)
      (setq-local skg-edit--source-buffer source-buffer)
      (setq-local skg-edit--sexp-start sexp-start)
      (setq-local skg-edit--sexp-end sexp-end))))

(provide 'skg-sexpr-edit)
