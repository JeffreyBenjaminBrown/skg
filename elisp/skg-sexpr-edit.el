;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Interactive s-expression editing utilities.
;;; From a 'source buffer', the user can call skg-view-metadata
;;; to open a new 'sexp edit' buffer,
;;; where the sexp is rendered as an org-tree.
;;;
;;; User-facing functions:
;;;   skg-edit-metadata

(require 'skg-metadata)
(require 'skg-sexpr-org-bijection)


;;
;; Buffer-local variables for the edit buffer
;;

(defvar-local skg-sexp-edit--source-buffer nil
  "The buffer containing the sexp being edited.")

(defvar-local skg-sexp-edit--start nil
  "Start position of the sexp in the source buffer.")

(defvar-local skg-sexp-edit--end nil
  "End position of the sexp in the source buffer.")

;;
;; Minor mode for the edit buffer
;;

(defvar skg-sexp-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'skg-sexp-edit--commit)
    map)
  "Keymap for skg-sexp-edit-mode.")

(define-minor-mode skg-sexp-edit-mode
  "Minor mode for editing sexps as org text.
\\<skg-sexp-edit-mode-map>
\\[skg-sexp-edit--commit] to save changes back to the source buffer.
Kill the buffer to cancel without saving."
  :lighter " SExp-Edit"
  :keymap skg-sexp-edit-mode-map)

;;
;; Core implementation
;;

(defun skg-sexp-edit--commit ()
  "Save changes from sexp-edit buffer back to the source sexp."
  (interactive)
  (let* ((org-text (buffer-substring-no-properties (point-min) (point-max)))
         (new-sexp (org-to-sexp org-text))
         (new-text (prin1-to-string new-sexp))
         (source-buffer skg-sexp-edit--source-buffer)
         (start skg-sexp-edit--start)
         (end skg-sexp-edit--end))
    (kill-buffer)
    (switch-to-buffer source-buffer)
    (goto-char start)
    (delete-region start end)
    (insert new-text)))

(defconst skg-edit--help-text
  "Press 'C-c C-c' to save changes, or 'C-x k' to cancel.\n\n"
  "Help text shown at the top of the edit buffer.")

;;
;; User-facing function
;;

(defun skg-edit-metadata ()
  "Edit the metadata sexp on the current headline in an org buffer.
Opens a temporary org buffer with the sexp converted to org headlines.
Use C-c C-c to save changes back to the source buffer.
Kill the buffer to cancel without saving."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Not on a headline"))
  (let* (( headline (skg-get-current-headline-text) )
         ( split (skg-split-as-stars-metadata-title headline) ))
    (unless split
      (user-error "Not on a headline"))
    (let (( metadata-str (cadr split) ))
      (when (string-empty-p metadata-str)
        (user-error "No metadata on this headline"))
      (let* (( source-buffer (current-buffer) )
             ( sexp (read metadata-str) )
             ( sexp-start (+ (line-beginning-position)
                             (length (car split)) ))
             ( sexp-end (+ sexp-start (length metadata-str)) )
             ( org-text (sexp-to-org sexp) )
             ( edit-buffer (generate-new-buffer "*skg-edit*") ))
      (switch-to-buffer edit-buffer)
      (insert skg-edit--help-text)
      (insert org-text)
      (org-mode)
      (org-fold-show-all)
      (goto-char (point-min))
      (progn ;; skip past help text and 'skg', land at viewnode kind
        (outline-next-heading)
        (outline-next-heading))
      (skg-sexp-edit-mode 1)
      (setq-local skg-sexp-edit--source-buffer source-buffer)
      (setq-local skg-sexp-edit--start sexp-start)
      (setq-local skg-sexp-edit--end sexp-end)))))

(provide 'skg-sexpr-edit)
