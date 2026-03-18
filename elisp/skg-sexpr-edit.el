;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Interactive s-expression editing utilities.
;;; From a 'source buffer', the user can call skg-local-metadata
;;; to open a new 'sexp edit' buffer,
;;; where the sexp is rendered as an org-tree.
;;;
;;; User-facing functions:
;;;   skg-edit-metadata

(require 'skg-metadata)
(require 'skg-sexpr-org-bijection)
(require 'skg-truenode-defaults)
(require 'skg-sexpr-cycling)
(require 'skg-state)


;;
;; Buffer-local variables for the edit buffer
;;

(defvar-local skg-sexp-edit--source-buffer nil
  "The buffer containing the sexp being edited.")

(defvar-local skg-sexp-edit--start nil
  "Start position of the sexp in the source buffer.")

(defvar-local skg-sexp-edit--end nil
  "End position of the sexp in the source buffer.")

(defvar-local skg-sexp-edit--is-truenode nil
  "Non-nil if the sexp being edited is a TrueNode.")

;;
;; Minor mode for the edit buffer
;;

(defvar skg-sexp-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'skg-sexp-edit--commit)
    (define-key map (kbd "S-<left>") #'skg-sexp-edit-cycle-left)
    (define-key map (kbd "S-<right>") #'skg-sexp-edit-cycle-right)
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
         (final-org ;; last version of the org text in the sexp-edit buffer
          (if skg-sexp-edit--is-truenode
              (skg-truenode-strip-defaults-from-org org-text)
            org-text))
         (new-sexp (org-to-sexp final-org))
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
  "Press C-c C-c to save, C-x k to cancel, S-left/S-right to cycle values.\n\n"
  "Help text shown at the top of the edit buffer.")

(defun skg-sexp-edit--default-source ()
  "Return the default source name from config, or nil."
  (when skg-config-dir
    (let ((config-file
           (expand-file-name "skgconfig.toml" skg-config-dir)))
      (when (file-exists-p config-file)
        (car (skg-owned-sources-from-toml config-file))))))

(defun skg-sexp-edit--open-edit-buffer (org-text source-buffer
                                       sexp-start sexp-end is-truenode)
  "Open a sexp-edit buffer with ORG-TEXT from SOURCE-BUFFER.
SEXP-START and SEXP-END delimit the sexp in SOURCE-BUFFER."
  (let (( edit-buffer (generate-new-buffer "*skg-edit*") ))
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
    (setq-local skg-sexp-edit--end sexp-end)
    (setq-local skg-sexp-edit--is-truenode is-truenode)))

(defun skg-sexp-edit--goto-field-value (field-name)
  "Navigate point to the first child headline under FIELD-NAME."
  (goto-char (point-min))
  (when (re-search-forward
         (concat "^\\*+ " (regexp-quote field-name) "$") nil t)
    (outline-next-heading)))

(defun skg-sexp-edit--insert-default-metadata ()
  "Insert (skg (node)) metadata at the headline at point.
Returns (SEXP-START . SEXP-END) for the inserted text."
  (let (( sexp-text "(skg (node)) " ))
    (beginning-of-line)
    (search-forward " " nil t) ;; past the stars + space
    (insert sexp-text)
    (cons (- (point) (length sexp-text))
          (- (point) 1)))) ;; before the trailing space

;;
;; Advice: after org-insert-heading-respect-content in skg buffers
;;

(defun skg-sexp-edit--after-insert-heading (&rest _)
  "After `org-insert-heading-respect-content', open metadata editor for new roots.
Only acts in skg-content-view-mode buffers when a level-1 heading was created."
  (when (and (bound-and-true-p skg-content-view-mode)
             (org-at-heading-p)
             (= (org-outline-level) 1))
    (skg-edit-metadata)))

(advice-add 'org-insert-heading-respect-content :after
            #'skg-sexp-edit--after-insert-heading)

;;
;; User-facing function
;;

(defun skg-edit-metadata ()
  "Edit the metadata sexp on the current headline in an org buffer.
Opens a temporary org buffer with the sexp converted to org headlines.
If the headline has no metadata, inserts (skg (node)) first.
Use C-c C-c to save changes back to the source buffer.
Kill the buffer to cancel without saving."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Not on a headline"))
  (let* (( headline (skg-get-current-headline-text) )
         ( split (skg-split-as-stars-metadata-title headline) ))
    (unless split
      (user-error "Not on a headline"))
    (let* (( metadata-str (cadr split) )
           ( no-metadata (string-empty-p metadata-str) )
           ( positions (when no-metadata
                         (skg-sexp-edit--insert-default-metadata)) )
           ( source-buffer (current-buffer) ))
      (when no-metadata
        (setq metadata-str "(skg (node))"))
      (let* ((sexp (read metadata-str) )
             (is-truenode (skg-truenode-sexp-p sexp) )
             (sexp-start (if no-metadata
                             (car positions)
                           (+ (line-beginning-position)
                              (length (car split)))) )
             (sexp-end (if no-metadata
                           (cdr positions)
                         (+ sexp-start (length metadata-str))) )
             (default-source (when no-metadata
                               (skg-sexp-edit--default-source)) )
             (org-text (let ((sexp-as-org (sexp-to-org sexp)))
                          (if is-truenode
                              (skg-truenode-expand-defaults-in-org
                               sexp-as-org default-source)
                            sexp-as-org)) ))
        (skg-sexp-edit--open-edit-buffer
         org-text source-buffer sexp-start sexp-end is-truenode)
        (when no-metadata
          (skg-sexp-edit--goto-field-value "source")
          (message "Choose a source."))))))

(provide 'skg-sexpr-edit)
