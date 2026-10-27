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
(require 'skg-metadata-fields)
(require 'skg-shared)
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

(defvar-local skg-sexp-edit--kind nil
  "The kind of the headline being edited, from
'shared/metadata-fields.json', or nil.")

;;
;; Minor mode for the edit buffer
;;

;; skg-sexp-edit-mode-map is defined in skg-keymaps-and-aliases.el.

(define-minor-mode skg-sexp-edit-mode
  "Minor mode for editing sexps as org text.
\\<skg-sexp-edit-mode-map>
\\[skg-sexp-edit--apply] to save changes back to the source buffer.
Kill the buffer to cancel without saving."
  :lighter " SExp-Edit"
  :keymap skg-sexp-edit-mode-map)

;;
;; Core implementation
;;

(defun skg-sexp-edit--apply ()
  "Save changes from sexp-edit buffer back to the repo sexp."
  (interactive)
  (let* ((new-text (skg-sexp-edit--metadata-text-to-save
                    (buffer-substring-no-properties (point-min) (point-max))
                    skg-sexp-edit--kind))
         (source-buffer skg-sexp-edit--source-buffer)
         (start skg-sexp-edit--start)
         (end skg-sexp-edit--end))
    (kill-buffer)
    (switch-to-buffer source-buffer)
    (goto-char start)
    (delete-region start end)
    (insert new-text)))

(defun skg-sexp-edit--metadata-text-to-save (org-text kind)
  "The metadata text that applying the edit buffer's ORG-TEXT writes
into the source headline: ORG-TEXT stripped of the fields of KIND (a
kind from 'shared/metadata-fields.json', or nil) at their defaults,
as a sexp."
  (prin1-to-string
   (org-to-sexp (if kind
                    (skg-metadata-fields-strip-defaults-from-org
                     org-text kind)
                  org-text))))

(defconst skg-edit--help-text
  "Press C-c C-c to save, C-x k to cancel, S-left/S-right to cycle values.\n\n"
  "Help text shown at the top of the edit buffer.")


(defun skg-sexp-edit--open-edit-buffer (org-text source-buffer
                                       sexp-start sexp-end kind)
  "Open a sexp-edit buffer with ORG-TEXT from SOURCE-BUFFER.
SEXP-START and SEXP-END delimit the sexp in SOURCE-BUFFER. KIND is the
headline's kind from 'shared/metadata-fields.json', or nil."
  (let (( edit-buffer (generate-new-buffer "*skg-metadata-edit*") ))
    (switch-to-buffer edit-buffer)
    (insert skg-edit--help-text)
    (insert org-text)
    (skg--org-mode-with-options)
    (org-fold-show-all)
    (skg-sexp-edit--make-title-headlines-read-only)
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
    (setq-local skg-sexp-edit--kind kind)))

(defun skg-sexp-edit--decorate-with-heralds ()
  "Append, per headline, the herald its metadata path produces:
colored per herald rules, inside (uncolored) parentheses, after the
rest of the title. Implemented as overlays, so the
hints never become buffer text -- applying reads titles, and a
herald baked into a title would corrupt the sexp. A no-op without a
rule table (e.g. no server connection). Hints reflect the values at
open time; cycling a value does not refresh them."
  (when (bound-and-true-p heralds--transform-rules)
    (save-excursion
      (goto-char (point-min))
      (let (( path nil ) ;; ((level . title) ...), outermost first
            ( kind-atoms (skg-sexp-edit--kind-atoms-in-buffer) ))
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
                            (mapcar #'cdr path) kind-atoms) ))
              (when herald
                (let (( ov (make-overlay (line-end-position)
                                         (line-end-position)) ))
                  (overlay-put ov 'after-string
                               (concat " (" herald ")"))
                  (overlay-put ov 'skg-sexp-edit-herald t))))))))))

(defun skg-sexp-edit--kind-atoms-in-buffer ()
  "The kind atoms (symbols) the metadata view in the current buffer
shows: each level-2 headline whose title is an atom of a kind in
'shared/metadata-fields.json'. The heralds of a kind's parts fire
only beside its atom, so each hint's object carries them."
  (save-excursion
    (goto-char (point-min))
    (let (( atoms nil ))
      (while (re-search-forward "^\\*\\* \\(.*\\)$" nil t)
        (let (( title (string-trim
                       (substring-no-properties (match-string 1))) ))
          (when (skg-shared-metadata-kind title)
            (push (intern title) atoms))))
      (nreverse atoms))))

(defun skg-sexp-edit--herald-for-label-path (labels &optional kind-atoms)
  "The herald display string for the metadata-view headline whose
label path from the buffer's root is LABELS (title strings, outermost
first), or nil when that path produces none. Builds a minimal object
sexp along the path, with KIND-ATOMS beside it at the top level, and
runs the same lens engine the heralds view uses, so the hint matches
what the content view would show."
  (when (and (bound-and-true-p heralds--transform-rules)
             labels)
    (let* (( path-object (skg-sexp-edit--nest-label-path labels) )
           ( object (append (list (car path-object))
                            (cl-remove-if
                             (lambda (atom) (memq atom (cdr path-object)))
                             kind-atoms)
                            (cdr path-object)) )
           ( tokens (ignore-errors
                      (skg-transform-sexp-flat
                       object heralds--transform-rules)) )
           ( text (and tokens (heralds--tokens->text tokens)) ))
      (when (and text (not (string-empty-p text)))
        text))))

(defun skg-sexp-edit--nest-label-path (labels)
  "Nest LABELS (title strings, outermost first) into an object sexp:
the labels skg, writeProtected, true become (skg (writeProtected true))."
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

(defun skg-view-metadata--open-empty-node-view (title)
  "Populate minimal metadata on the metadata-less headline at point,
then open its metadata view.  TITLE is the headline's existing title.
The view pre-fills the chosen repo (saving needs a repo, so the
user confronts that choice immediately) and leaves every other editable
field childless, so an untouched save yields just
\(skg node (skgRepos (nodeRepo X)))."
  (skg--populate-minimal-node-metadata)
  (let* (( source-buffer (current-buffer) )
         ( headline (skg-get-current-headline-text) )
         ( split (skg-split-as-stars-metadata-title headline) )
         ( metadata-str (cadr split) )
         ( skgrepo (skg--node-repo (read metadata-str)) )
         ( sexp-start (+ (line-beginning-position)
                         (length (car split))) )
         ( sexp-end (+ sexp-start (length metadata-str)) )
         ( org-text (skg-view-metadata--empty-node-org-text
                     skgrepo title) ))
    (skg-sexp-edit--open-edit-buffer
     org-text source-buffer sexp-start sexp-end
     (skg-shared-metadata-kind "node"))
    (skg-sexp-edit--goto-field-value "nodeRepo")))

(defun skg-view-metadata--empty-node-org-text (skgrepo title)
  "Build org text for the empty-node metadata view.
REPO is pre-filled under the `nodeRepo' of `skgRepos'.  TITLE, if non-blank,
appears under a display-only `title' group; otherwise `title' is shown
childless.  Every editable field other than repo appears childless,
so the strip step (`skg-metadata-fields-strip-defaults-from-org') drops the
ones the user never populates -- key and all."
  (let* (( title-group
           (if (string-empty-p (string-trim (or title "")))
               (list (cons 1 "title"))
             (list (cons 1 "title")
                   (cons 2 title)) ))
         ( node-skeleton
           (append (list (cons 1 "skg")
                         (cons 2 "node")
                         (cons 2 "skgRepos")
                         (cons 3 "nodeRepo")
                         (cons 4 skgrepo))
                   (mapcar (lambda (field)
                             (cons 2 field))
                           (skg-metadata-fields-field-names
                            (skg-shared-metadata-kind "node")))) ))
    (skg-headlines-to-org
     (append title-group node-skeleton))))

;;
;; Advice: after org-insert-heading-respect-content in skg buffers
;;

(defun skg-sexp-edit--after-insert-headline (&rest _)
  "After `org-insert-heading-respect-content', give the new headline a
repo if it cannot inherit one.
Only acts in skg-content-view-mode buffers, and only when
`skg--headline-can-inherit-skgrepo-p' says no.  Routes through
`skg-set-repo', which on a metadata-less headline prompts for an owned
repo and writes minimal node metadata -- the quick path, no edit
buffer.  (The full metadata view is reserved for an explicit
`skg-view-metadata'/C-c v m.)"
  (when (and (derived-mode-p 'skg-content-view-mode)
             (org-at-heading-p)
             (not (skg--headline-can-inherit-skgrepo-p)))
    (skg-set-repo)))

(defun skg--headline-can-inherit-skgrepo-p ()
  "Whether the save can give the headline at point a skgrepo by
inheritance, as the server's walk rootward decides
('skgrepo_of_nearest_occurrence_ancestor'): it passes through
ghosts, unknowns, Restricted vognodes and folders, and through
headlines without
metadata, which inherit the same way; the first other ancestor must
be an occurrence.  A root cannot inherit, nor can a headline whose
walk ends at a tombstone or a property."
  (save-excursion
    (catch 'answer
      (while (org-up-heading-safe)
        (let (( kind (alist-get 'kind (skg-metadata-fields-kind-of-sexp
                                      (skg--metadata-sexp-at-point-or-nil))) ))
          (cond ((equal kind "occurrence") (throw 'answer t))
                ((member kind '(nil "ghost" "unknown" "restricted" "folder")))
                (t (throw 'answer nil)))))
      nil)))

(advice-add 'org-insert-heading-respect-content :after
            #'skg-sexp-edit--after-insert-headline)

;;
;; User-facing function
;;

(defun skg-view-metadata ()
  "Edit the metadata sexp on the current headline in an org buffer.
Opens a temporary org buffer with the sexp converted to org headlines,
showing each field a user may set on the headline's kind
('shared/metadata-fields.json'), at its default when it is absent.
If the headline has no metadata, populates a minimal
\(skg node (skgRepos (nodeRepo X)))
in place and opens the empty-node view over it -- repo pre-filled, the
other editable fields childless (see `skg-view-metadata--open-empty-node-view').
A readiness request in its '(editRequest ...)' is shown beside the
other request, so that deleting it there clears it.
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
          (skg-view-metadata--open-empty-node-view (caddr split))
        (let* (( source-buffer (current-buffer) )
               (sexp (read metadata-str))
               (sexp-start (+ (line-beginning-position)
                              (length (car split))) )
               (sexp-end (+ sexp-start (length metadata-str)) ))
          (skg-sexp-edit--open-edit-buffer
           (skg-sexp-edit--org-text-to-show sexp (caddr split))
           source-buffer sexp-start sexp-end
           (skg-metadata-fields-kind-of-sexp sexp))
          (skg-sexp-edit--goto-field-value "nodeRepo"))))))

(defun skg-sexp-edit--org-text-to-show (metadata-sexp title)
  "The org text the edit buffer shows for METADATA-SEXP, a headline's
metadata, whose title is TITLE: each field its kind may set filled in,
under a read-only title group when it is an occurrence's."
  (let (( kind (skg-metadata-fields-kind-of-sexp metadata-sexp) )
        ( sexp-as-org (sexp-to-org metadata-sexp) ))
    (if kind
        (skg-metadata-fields-fillIn-defaults-in-org
         sexp-as-org kind
         (when (skg--occurrence-sexp-p metadata-sexp) title))
      sexp-as-org)))

(provide 'skg-sexpr-edit)
