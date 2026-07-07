;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Interactive s-expression editing utilities.
;;; From a 'source buffer', the user can call skg-view-metadata
;;; to open a new 'sexp edit' buffer,
;;; where the sexp is rendered as an org-tree.

(require 'heralds-minor-mode) ;; rule table + token rendering, for the per-field herald hints
(require 'skg-buffer)
(require 'skg-config)
(require 'skg-lens)
(require 'skg-metadata)
(require 'skg-sexpr-org-bijection)
(require 'skg-activeNode-defaults)
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

(defvar-local skg-sexp-edit--is-activeNode nil
  "Non-nil if the sexp being edited is an ActiveNode.")

;;
;; Minor mode for the edit buffer
;;

;; skg-sexp-edit-mode-map is defined in skg-keymaps-and-aliases.el.

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
  (let* ((org-text (buffer-substring-no-properties
                    (point-min) (point-max)))
         (final-org ;; last version of the org text in the sexp-edit buffer
          (if skg-sexp-edit--is-activeNode
              (skg-activeNode-strip-defaults-from-org org-text)
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


(defun skg-sexp-edit--open-edit-buffer (org-text source-buffer
                                       sexp-start sexp-end is-activeNode)
  "Open a sexp-edit buffer with ORG-TEXT from SOURCE-BUFFER.
SEXP-START and SEXP-END delimit the sexp in SOURCE-BUFFER."
  (let (( edit-buffer (generate-new-buffer "*skg-metadata-edit*") ))
    (switch-to-buffer edit-buffer)
    (insert skg-edit--help-text)
    (insert org-text)
    (skg--org-mode-with-options)
    (org-fold-show-all)
    (when is-activeNode
      (skg-sexp-edit--make-title-headlines-read-only))
    (goto-char (point-min))
    (progn ;; move point
      (outline-next-heading)
      (when (looking-at "^\\* title$")
        (outline-next-heading)
        (outline-next-heading))
      (outline-next-heading))
    (skg-sexp-edit-mode 1)
    (skg-sexp-edit--decorate-with-heralds)
    (setq-local skg-sexp-edit--source-buffer source-buffer)
    (setq-local skg-sexp-edit--start sexp-start)
    (setq-local skg-sexp-edit--end sexp-end)
    (setq-local skg-sexp-edit--is-activeNode is-activeNode)))

(defun skg-sexp-edit--decorate-with-heralds ()
  "Append, per headline, the herald its metadata path produces:
colored per herald rules, inside (uncolored) parentheses, after the
rest of the title (TODO/more.org). Implemented as overlays, so the
hints never become buffer text -- committing reads titles, and a
herald baked into a title would corrupt the sexp. A no-op without a
rule table (e.g. no server connection). Hints reflect the values at
open time; cycling a value does not refresh them."
  (when (bound-and-true-p heralds--transform-rules)
    (save-excursion
      (goto-char (point-min))
      (let (( path nil )) ;; ((level . title) ...), outermost first
        (while (re-search-forward "^\\(\\*+\\) \\(.*\\)$" nil t)
          (let* (( level (length (match-string 1)) )
                 ( title (string-trim
                          (substring-no-properties (match-string 2)) )))
            (setq path
                  (append (cl-remove-if (lambda (entry)
                                          (>= (car entry) level))
                                        path)
                          (list (cons level title))))
            (let (( herald (skg-sexp-edit--herald-for-label-path
                            (mapcar #'cdr path)) ))
              (when herald
                (let (( ov (make-overlay (line-end-position)
                                         (line-end-position)) ))
                  (overlay-put ov 'after-string
                               (concat " (" herald ")"))
                  (overlay-put ov 'skg-sexp-edit-herald t))))))))))

(defun skg-sexp-edit--herald-for-label-path (labels)
  "The herald display string for the metadata-view headline whose
label path from the buffer's root is LABELS (title strings, outermost
first), or nil when that path produces none. Builds a minimal object
sexp along the path and runs the same lens engine the heralds view
uses, so the hint matches what the content view would show."
  (when (and (bound-and-true-p heralds--transform-rules)
             labels)
    (let* (( object (skg-sexp-edit--nest-label-path labels) )
           ( tokens (ignore-errors
                      (skg-transform-sexp-flat
                       object heralds--transform-rules)) )
           ( text (and tokens (heralds--tokens->text tokens)) ))
      (when (and text (not (string-empty-p text)))
        text))))

(defun skg-sexp-edit--nest-label-path (labels)
  "Nest LABELS (title strings, outermost first) into an object sexp:
the labels skg, node, indef become (skg (node indef))."
  (let* (( syms (mapcar #'intern labels) )
         ( acc (car (last syms)) ))
    (dolist (label (reverse (butlast syms)))
      (setq acc (list label acc)))
    (if (listp acc) acc (list acc))))

(defun skg-sexp-edit--make-title-headlines-read-only ()
  "Make the display-only title group read-only, if it is present."
  (save-excursion
    (goto-char (point-min))
    (outline-next-heading)
    (when (looking-at "^\\* title$")
      (let ((start (line-beginning-position))
            (end (save-excursion
                   (forward-line 1)
                   (if (looking-at "^\\*\\* .+$")
                       (progn
                         (forward-line 1)
                         (point))
                     (line-end-position)))))
        (add-text-properties
         start end
         '(read-only t
           rear-nonsticky (read-only)))))))

(defun skg-sexp-edit--goto-field-value (field-name)
  "Navigate point to the first child headline under FIELD-NAME."
  (goto-char (point-min))
  (when (re-search-forward
         (concat "^\\*+ " (regexp-quote field-name) "$") nil t)
    (outline-next-heading)))

(defun skg-edit-metadata--open-empty-node-view (title)
  "Populate minimal metadata on the metadata-less headline at point,
then open its metadata view.  TITLE is the headline's existing title.
The view pre-fills the chosen source (Q2: saving needs a source, so the
user confronts that choice immediately) and leaves every other editable
field childless, so an untouched save yields just (skg (node (source X)))."
  (skg--populate-minimal-node-metadata)
  (let* (( source-buffer (current-buffer) )
         ( headline (skg-get-current-headline-text) )
         ( split (skg-split-as-stars-metadata-title headline) )
         ( metadata-str (cadr split) )
         ( source (skg--node-source (read metadata-str)) )
         ( sexp-start (+ (line-beginning-position)
                         (length (car split))) )
         ( sexp-end (+ sexp-start (length metadata-str)) )
         ( org-text (skg-edit-metadata--empty-node-org-text
                     source title) ))
    (skg-sexp-edit--open-edit-buffer
     org-text source-buffer sexp-start sexp-end t)
    (skg-sexp-edit--goto-field-value "source")))

(defun skg-edit-metadata--empty-node-org-text (source title)
  "Build org text for the empty-node metadata view.
SOURCE is pre-filled under the `source' field.  TITLE, if non-blank,
appears under a display-only `title' group; otherwise `title' is shown
childless.  Every editable field other than source appears childless,
so the strip step (`skg-activeNode-strip-defaults-from-org') drops the
ones the user never populates -- key and all."
  (let* (( title-group
           (if (string-empty-p (string-trim (or title "")))
               (list (cons 1 "title"))
             (list (cons 1 "title")
                   (cons 2 title)) ))
         ( node-skeleton
           (append (list (cons 1 "skg")
                         (cons 2 "node")
                         (cons 3 "source")
                         (cons 4 source))
                   (mapcar (lambda (field)
                             (cons 3 field))
                           (mapcar #'car
                                   skg-activeNode--editable-defaults))) ))
    (skg-headlines-to-org
     (append title-group node-skeleton))))

;;
;; Advice: after org-insert-heading-respect-content in skg buffers
;;

(defun skg-sexp-edit--after-insert-heading (&rest _)
  "After `org-insert-heading-respect-content', give new roots a source.
Only acts in skg-content-view-mode buffers when a level-1 heading was
created.  Routes through `skg-set-source', which on a metadata-less
headline prompts for an owned source and writes minimal node metadata
-- the quick path, no edit buffer.  (The full metadata view is reserved
for an explicit `skg-edit-metadata'/C-c v m.)"
  (when (and (derived-mode-p 'skg-content-view-mode)
             (org-at-heading-p)
             (= (org-outline-level) 1))
    (skg-set-source)))

(advice-add 'org-insert-heading-respect-content :after
            #'skg-sexp-edit--after-insert-heading)

;;
;; User-facing function
;;

(defun skg-edit-metadata ()
  "Edit the metadata sexp on the current headline in an org buffer.
Opens a temporary org buffer with the sexp converted to org headlines.
If the headline has no metadata, populates a minimal (skg (node (source X)))
in place and opens the empty-node view over it -- source pre-filled, the
other editable fields childless (see `skg-edit-metadata--open-empty-node-view').
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
           ( no-metadata (string-empty-p metadata-str) ))
      (if no-metadata
          (skg-edit-metadata--open-empty-node-view (caddr split))
        (let* (( source-buffer (current-buffer) )
               (sexp (read metadata-str) )
               (is-activeNode (skg-activeNode-sexp-p sexp) )
               (sexp-start (+ (line-beginning-position)
                              (length (car split))) )
               (sexp-end (+ sexp-start (length metadata-str)) )
               (title (caddr split))
               (org-text (let ((sexp-as-org (sexp-to-org sexp)))
                           (if is-activeNode
                               (skg-activeNode-expand-defaults-in-org
                                sexp-as-org nil title)
                             sexp-as-org)) ))
          (skg-sexp-edit--open-edit-buffer
           org-text source-buffer sexp-start sexp-end is-activeNode)
          (skg-sexp-edit--goto-field-value "source"))))))

(provide 'skg-sexpr-edit)
