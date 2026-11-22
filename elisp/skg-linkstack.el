;;; skg-linkstack.el --- The linkstack: push, paste, pop, view  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; The linkstack, 'skg-linkstack' (defined in 'skg-state.el'), is a
;;; stack of trees of (ID LABEL) notes. 'skg-push-id' (C-c u) pushes
;;; the link or headline at point as a tree of one; 'skg-push-ancestry'
;;; ('skg-push-ancestry.el', C-c U) pushes a chain. C-c o then i, l
;;; or n ('skg-paste-id', 'skg-paste-link', 'skg-paste-node') inserts
;;; the top's ID, a link (a chain of links for a tree), or a
;;; write-protected headline; C-c O ('skg-pop-id', 'skg-pop-link',
;;; 'skg-pop-node') does the same and pops. 'skg-view-linkstack'
;;; (C-c v s) opens the stack as the editable buffer *skg-linkstack*,
;;; whose C-x C-s ('skg--save-linkstack-buffer') replaces the stack.
;;; Finding the ID or link at point is 'skg-id-search.el's
;;; ('skg--point-in-link-p', 'skg--extract-id-from-metadata-sexp').
;;; Its Neovim twin is 'nvim/lua/skg/linkstack.lua'.
;;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'skg-buffer)
(require 'skg-id-search)
(require 'skg-metadata)
(require 'skg-sexpr-search)
(require 'skg-state)

(defun skg-push-id ()
  "Push an ID and its title/label to `skg-linkstack', as a tree of one
note (see `skg-linkstack').
If point is on an inline [[id:X][label]] link, push that link's
ID and label. Otherwise push the headline's metadata ID and title.
`skg-push-ancestry' pushes the headline's whole view-ancestry instead."
  (interactive)
  (let (( link-hit (skg--point-in-link-p) ))
    (if link-hit
        (let (( gnid    (car link-hit) )
              ( label (cdr link-hit) ))
          (push (list gnid label) skg-linkstack)
          (message "pushed to stack: %s" label) )
      (let* (( sexp (skg--metadata-sexp-at-point-or-nil) )
             ( title (caddr (skg-split-as-stars-metadata-title
                             (skg-get-current-headline-text))) ))
        (if sexp
            (if (skg--metadata-sexp-contains-id-p sexp)
                (let (( gnid (skg--extract-id-from-metadata-sexp sexp) ))
                  (push (list gnid title) skg-linkstack)
                  (message "pushed to stack: %s" title) )
              (message "No ID in metadata on this line") )
          (message "No metadata on this line") )) )))

(define-obsolete-function-alias 'skg-id-push #'skg-push-id "2026-11-22")

;;
;; The linkstack's trees
;;

(defun skg--linkstack-tree (gnid label &optional edge children)
  "A tree of `skg-linkstack': the note GNID labelled LABEL, joined to
its tree-parent by EDGE (nil at a root), with the trees CHILDREN.
Trailing nils are dropped, so a tree of one note is (GNID LABEL)."
  (cond (children (list gnid label edge children))
        (edge     (list gnid label edge))
        (t        (list gnid label))))

(defun skg--linkstack-tree-edge (tree)
  "The edge joining TREE's root to its tree-parent, or nil."
  (nth 2 tree))

(defun skg--linkstack-tree-children (tree)
  "The trees under TREE's root."
  (nth 3 tree))

(defun skg--linkstack-tree-chain (tree)
  "TREE's chain: its root, then each first child, down to a leaf."
  (cons tree (let (( first-child (car (skg--linkstack-tree-children tree)) ))
               (when first-child
                 (skg--linkstack-tree-chain first-child)) )))

(defun skg--linkstack-tree-leaf (tree)
  "The last note of TREE's chain: the one its push started from."
  (car (last (skg--linkstack-tree-chain tree))))

(defconst skg--linkstack-relation-letters
  '((contains . "C") (linksTo . "L") (forks . "K") (hides . "H"))
  "Each relation an ancestry edge can be, and its herald letter
\='docs/heralds.org', \"Letters and sides\").")

(defun skg--linkstack-edge-herald (edge)
  "EDGE, a (RELATION SIDE...), as the herald-like token the stack view
shows before a child's label: the relation's letter, with the
tree-parent's ancestor flag \='a' before it when the parent relates
to the child (side \='in') and after it when the child relates to
the parent (side \='out'), as the heralds of a view do. So \='aC':
the parent contains the child; \='Ka': the child forks the parent;
\='aKa': each forks the other."
  (concat (if (memq 'in (cdr edge)) "a" "")
          (or (alist-get (car edge) skg--linkstack-relation-letters)
              (error "No herald letter for the relation %s" (car edge)))
          (if (memq 'out (cdr edge)) "a" "")))

(defun skg--linkstack-edge-from-herald (herald)
  "The edge (RELATION SIDE...) HERALD writes (see
`skg--linkstack-edge-herald'), or nil if it writes none."
  (when (string-match "\\`\\(a?\\)\\([CLKH]\\)\\(a?\\)\\'" herald)
    (let (( relation (car (rassoc (match-string 2 herald)
                                  skg--linkstack-relation-letters)) )
          ( sides (append (and (> (length (match-string 1 herald)) 0) '(in))
                          (and (> (length (match-string 3 herald)) 0) '(out)) )))
      (when sides (cons relation sides)) )))

(defun skg--linkstack-edge-text (edge)
  "What EDGE, joining a tree-parent (written first) to its child, reads
as between their links in a pasted chain: \"/\" the parent contains
the child, \"\\\" the child contains the parent; \"~\" one forks the
other; \"->\" the parent links to the child, \"<-\" the child to
the parent; \"hides\" the parent hides the child, \"is hidden by\"
the child hides the parent."
  (let (( in (memq 'in (cdr edge)) ))
    (pcase (car edge)
      ('contains (if in "/" "\\"))
      ('forks    "~")
      ('linksTo  (if in "->" "<-"))
      ('hides    (if in "hides" "is hidden by"))
      (_ (error "No text for the relation %s" (car edge))) )))

(defun skg--linkstack-tree-text (tree leaf-label)
  "TREE as a paste inserts it: each note of its chain a link, joined by
its edge's text (`skg--linkstack-edge-text'), the root first, the leaf
labelled LEAF-LABEL. A tree of one note is just its link."
  (let (( chain (skg--linkstack-tree-chain tree) ))
    (mapconcat
     (lambda (node)
       (let (( link (format "[[id:%s][%s]]"
                            (car node)
                            (if (eq node (car (last chain)))
                                leaf-label (cadr node))) )
             ( edge (skg--linkstack-tree-edge node) ))
         (if edge
             (concat (skg--linkstack-edge-text edge) " " link)
           link) ))
     chain " " )))

(defun skg--linkstack-tree-prefix-for-prompt (tree)
  "TREE's chain but its leaf, as labels and edge texts, ending in the
leaf's edge text and a space: \"a / b ~ \". Empty for a tree of one note."
  (mapconcat
   (lambda (node)
     (let (( edge (skg--linkstack-tree-edge node) ))
       (concat (if edge (concat (skg--linkstack-edge-text edge) " ") "")
               (if (eq node (skg--linkstack-tree-leaf tree))
                   ""
                 (concat (cadr node) " ")) )))
   (skg--linkstack-tree-chain tree) "" ))

;;
;; Choosing the tree to paste, and its leaf's label
;;

(defun skg--linkstack-label-prompt (index)
  "The prompt for the leaf label of the INDEXth tree of `skg-linkstack'
(0 is the top): its position when the stack holds more than one, then
its chain but the leaf (`skg--linkstack-tree-prefix-for-prompt')."
  (let (( count (length skg-linkstack) ))
    (concat "Link label"
            (if (> count 1)
                (format " (%d/%d; S-left/right: another tree)" (1+ index) count)
              "")
            ": "
            (skg--linkstack-tree-prefix-for-prompt (nth index skg-linkstack)) )))

(defun skg--linkstack-read-leaf-label (index)
  "Read, in the minibuffer, the leaf label of the INDEXth tree of
`skg-linkstack', its own as the initial input. S-left and S-right
throw -1 or 1 to the tag `skg--linkstack-cycle', for
`skg--linkstack-choose-tree-and-label' to show another tree."
  (minibuffer-with-setup-hook
      (lambda ()
        (let (( map (copy-keymap (current-local-map)) ))
          (define-key map (kbd "S-<left>")
                      (lambda () (interactive) (throw 'skg--linkstack-cycle -1)))
          (define-key map (kbd "S-<right>")
                      (lambda () (interactive) (throw 'skg--linkstack-cycle 1)))
          (use-local-map map) ))
    (read-string (skg--linkstack-label-prompt index)
                 (cadr (skg--linkstack-tree-leaf (nth index skg-linkstack)))) ))

(defun skg--linkstack-choose-tree-and-label ()
  "Return (INDEX . LABEL): which tree of `skg-linkstack' to paste (0 is
the top) and its leaf's label. In the minibuffer, the top tree and its
leaf's own label, unasked. Elsewhere it asks for the label, starting at
the top tree; S-left and S-right show the tree above or below it,
cycling, and start the label over from that tree's leaf."
  (if (minibufferp)
      (cons 0 (cadr (skg--linkstack-tree-leaf (car skg-linkstack))))
    (let (( index 0 )
          ( chosen nil ))
      (while (not chosen)
        (let (( answer (catch 'skg--linkstack-cycle
                         (list (skg--linkstack-read-leaf-label index))) ))
          (if (consp answer)
              (setq chosen (cons index (car answer)))
            (setq index (mod (+ index answer) (length skg-linkstack)))) ))
      chosen )))

(defun skg--linkstack-top-or-message ()
  "If possible, return the leaf (id title) of the top tree of
`skg-linkstack': for a tree of one note, that note.
Othewrise (because the stack is empty) print a message and return nil."
  (if (null skg-linkstack)
      (progn (message "Linkstack is empty") nil)
    (skg--linkstack-tree-leaf (car skg-linkstack)) ))

(defun skg--insert-link-from-stack (pop)
  "Insert at point a tree of `skg-linkstack' as links
\=(`skg--linkstack-tree-text'), asking which tree and its leaf's label
\=(`skg--linkstack-choose-tree-and-label'). If POP, remove that tree
from the stack."
  (let* (( choice (skg--linkstack-choose-tree-and-label) )
         ( tree (nth (car choice) skg-linkstack) ))
    (when pop
      (setq skg-linkstack (append (seq-take skg-linkstack (car choice))
                                  (nthcdr (1+ (car choice)) skg-linkstack))))
    (insert (skg--linkstack-tree-text tree (cdr choice))) ))

(defun skg--org-stars-for-node-insertion ()
  "Return headline stars for inserting a node at point."
  (if (derived-mode-p 'org-mode)
      (save-excursion
        (condition-case nil
            (progn
              (org-back-to-heading t)
              (make-string (org-outline-level) ?*))
          (error "*")))
    "*"))

(defun skg--insert-node-from-entry (entry)
  "Insert a write-protected occurrence headline from ENTRY, an (id title) pair.
In a view whose buffer has no writable occurrence of the node, the
headline also requests a editable view, so the next save makes it
writable with its real title, body and content.  (Saving a bare
writable headline would instead erase that body and content.)
If point is already after headline stars at the start of a line,
insert only the metadata and title.  Otherwise insert a full same-level
headline."
  (let* (( gnid (car entry) )
         ( title (cadr entry) )
         ( request-editable-view
           (and (bound-and-true-p skg-view-id)
                (if (skg--buffer-has-editable-occurrence-p gnid)
                    (progn (message "NOTE: Pasting node write-protected because a writable occurrence is already present in this same buffer.")
                           nil)
                  t )) )
         ( node-text
           (format (if request-editable-view
                       "(skg node (gnid %s) writeProtected (viewRequests editableView)) %s"
                     "(skg node (gnid %s) writeProtected) %s")
                   gnid title )) )
    (if (save-excursion
          (let ((pos (point)))
            (beginning-of-line)
            (and (looking-at "\\*+[ \t]*")
                 (<= (match-end 0) pos)
                 (string-blank-p
                  (buffer-substring-no-properties (match-end 0) pos)))))
        (insert node-text)
      (insert (format "%s %s\n"
                      (skg--org-stars-for-node-insertion)
                      node-text)))))

(defun skg--buffer-has-editable-occurrence-p (gnid)
  "Return non-nil if the current buffer has a headline for node ID that
is writable, or that will become writable at the next save because it
requests a editable view.  The server allows only one of those per ID."
  (save-excursion
    (goto-char (point-min))
    (let (( found nil ))
      (while (and (not found)
                  (re-search-forward org-heading-regexp nil t))
        (beginning-of-line)
        (let (( sexp (ignore-errors ;; a half-edited headline is not an occurrence
                       (skg--metadata-sexp-at-point-or-nil)) ))
          (when (and (skg--occurrence-sexp-p sexp)
                     (equal (skg--node-id sexp) gnid)
                     (or (not (skg--node-write-protected-p sexp))
                         (skg-sexp-subtree-p
                          sexp '(skg (viewRequests editableView)))))
            (setq found t)))
        (forward-line 1))
      found)))

(defun skg-paste-id ()
  "Insert the ID at the top of `skg-linkstack' at point.
Does not modify the stack."
  (interactive)
  (let (( entry (skg--linkstack-top-or-message) ))
    (when entry
      (insert (car entry)) )))

(defun skg-paste-link ()
  "Insert an org link at point, using the top of `skg-linkstack'.
Does not modify the stack.
Prompts for the link label, defaulting to the title from the stack.
A tree that `skg-push-ancestry' pushed pastes as a chain of links,
the root first, joined by what joins them in the graph
\=(`skg--linkstack-tree-text'); only the last link's label is asked for.
In the prompt, S-left and S-right paste another tree of the stack
instead (`skg--linkstack-choose-tree-and-label')."
  (interactive)
  (when (skg--linkstack-top-or-message)
    (skg--insert-link-from-stack nil) ))

(defun skg-paste-node ()
  "Insert a write-protected occurrence headline from the top of `skg-linkstack'.
Does not modify the stack.  The inserted metadata contains the node ID
and `writeProtected`, and maybe a editable view request (see
`skg--insert-node-from-entry'); the headline title comes from the stack entry."
  (interactive)
  (let (( entry (skg--linkstack-top-or-message) ))
    (when entry
      (skg--insert-node-from-entry entry) )))

(defun skg-pop-id ()
  "Pop the top of `skg-linkstack' and insert the ID at point."
  (interactive)
  (let (( entry (skg--linkstack-top-or-message) ))
    (when entry
      (pop skg-linkstack)
      (insert (car entry)) )))

(defun skg-pop-link ()
  "Pop the top of `skg-linkstack' and insert an org link at point.
Prompts for the link label, defaulting to the title from the stack.
Like `skg-paste-link', a chain pastes as one, and S-left and S-right
choose another tree: the tree pasted is the one popped."
  (interactive)
  (when (skg--linkstack-top-or-message)
    (skg--insert-link-from-stack t) ))

(defun skg-pop-node ()
  "Pop the top of `skg-linkstack' and insert a write-protected occurrence headline.
The inserted metadata contains the node ID and `writeProtected`, and maybe a
editable view request (see `skg--insert-node-from-entry'); the headline
title comes from the stack entry."
  (interactive)
  (let (( entry (skg--linkstack-top-or-message) ))
    (when entry
      (pop skg-linkstack)
      (skg--insert-node-from-entry entry) )))

(defun skg-replace-linkstack-from-buffer ()
  "Replace `skg-linkstack' with contents parsed from current buffer.
Uses `skg-validate-linkstack-buffer' to parse and validate.
On success, sets `skg-linkstack' to the parsed result.
On failure, prints error message and leaves `skg-linkstack' unchanged."
  (let (( validation-result (skg-validate-linkstack-buffer) ))
    (if (eq (car validation-result) 'success)
        (setq skg-linkstack (cadr validation-result))
      (message "Invalid ID buffer: %s" (cadr validation-result)) )))

(defun skg--save-linkstack-buffer ()
  "Save handler for the linkstack buffer.
Validates and replaces `skg-linkstack' with buffer contents.
Bound to C-x C-s (which normally calls `save-buffer')
by `skg-linkstack-mode'."
  (interactive)
  (let (( validation-result (skg-validate-linkstack-buffer) ))
    (if (eq (car validation-result) 'success)
        (progn
          (setq skg-linkstack (cadr validation-result))
          (set-buffer-modified-p nil)
          (message "Linkstack updated (%d items)"
                   (length skg-linkstack)) )
      (message "Invalid ID buffer: %s" (cadr validation-result)) )))

(defun skg-validate-linkstack-buffer ()
  "Validate current buffer as an linkstack buffer and return the stack.
Each headline must have a non-empty title and exactly one body line (the ID).
A headline below another is a child of it in its tree (see
`skg-linkstack'): one level deeper at most, and its title begins with
its edge to it, as `skg--format-linkstack-as-org' writes it, \"(aC) \".
Returns (success RESULT) where RESULT is a list of trees,
with the first top-level headline's first in the list.
Returns (error MESSAGE) if validation fails."
  (save-excursion
    (goto-char (point-min))
    (let ( ( headlines nil ) ;; (LEVEL ID LABEL EDGE), the last first
           ( error-msg nil )
           ( headline-regex "^\\(\\*+\\) +\\([^ \n].*\\)$" ))
      (progn ;; Check no content before first headline
        (skip-chars-forward " \t\n")
        (when (and (not (eobp))
                   (not (looking-at "^\\*")) )
          (setq error-msg "content before first headline") ))
      (while (and (not error-msg) (not (eobp)))
        (if (looking-at headline-regex)
            ;; Current line is a headline with non-empty title.
            (let* ( ( level (length (match-string 1)) )
                    ( title (match-string 2) )
                    ( above-level (if headlines (car (car headlines)) 0) )
                    ( body-start (1+ (line-end-position)) )
                    ( body-end nil ))
              (progn ;; Find where the body ends:
                ;; either at the next headline or at end of buffer.
                (forward-line 1)
                (setq body-end
                      (if (re-search-forward "^\\*" nil t)
                          (progn (beginning-of-line) (point))
                        (point-max) )))
              (let* ( ( body-text (buffer-substring-no-properties
                                   body-start body-end) )
                      ( body-lines (seq-filter
                                    (lambda (line)
                                      (not (string-empty-p
                                            (string-trim line) )))
                                    (split-string body-text "\n") ))
                      ( edge-and-label
                        (if (= level 1)
                            (cons nil title)
                          (skg--linkstack-edge-and-label-of-title title)) ))
                (cond
                 ((> level (1+ above-level))
                  (setq error-msg
                        (format "headline '%s' is more than one level below the headline above it"
                                title) ))
                 ((not edge-and-label)
                  (setq error-msg
                        (format "headline '%s' must begin with its edge to the headline above it, such as (aC)"
                                title) ))
                 ((/= (length body-lines) 1)
                  (setq error-msg
                        (format "headline '%s' must have exactly one body line"
                                title) ))
                 (t (push (list level
                                (string-trim (car body-lines)) ;; the id
                                (cdr edge-and-label)
                                (car edge-and-label))
                          headlines) )) ))
          (;; Not a headline with a valid title.
           ;; Could be a blank line, or a headline with empty title.
           if (looking-at "^\\*")
              ;; Looks like headline but has no title - that's an error.
              (setq error-msg "headline with empty title")
            ( ;; Not a headline at all (blank line), so skip it.
             forward-line 1) ) ))
      (if error-msg
          (list 'error error-msg)
        ;; nreverse so first headline in buffer = head of stack
        (list 'success (skg--linkstack-forest-of-headlines
                        (nreverse headlines) )) )) ))

(defun skg--linkstack-edge-and-label-of-title (title)
  "(EDGE . LABEL) of TITLE, the title of a linkstack buffer's headline
below another: \"(HERALD) LABEL\", HERALD an edge as
`skg--linkstack-edge-herald' writes it. Nil if TITLE is not that."
  (when (string-match "\\`(\\([^)]*\\)) +\\([^ ].*\\)\\'" title)
    (let* (( label (match-string 2 title) )
           ( edge (skg--linkstack-edge-from-herald (match-string 1 title)) ))
      (when edge (cons edge label)) )))

(defun skg--linkstack-forest-of-headlines (headlines)
  "The trees of HEADLINES, (LEVEL ID LABEL EDGE) in buffer order, each
headline the child of the nearest one above it at a lesser level."
  (let (( trees nil ))
    (while headlines
      (let* (( headline (pop headlines) )
             ( below nil ))
        (while (and headlines (> (car (car headlines)) (car headline)))
          (push (pop headlines) below))
        (push (skg--linkstack-tree
               (nth 1 headline) (nth 2 headline) (nth 3 headline)
               (skg--linkstack-forest-of-headlines (nreverse below)))
              trees) ))
    (nreverse trees) ))

;; skg-linkstack-mode-map is defined in skg-keymaps-and-aliases.el.

(defun skg-view-linkstack ()
  "Open a buffer to edit `skg-linkstack'.
The buffer displays the stack in org format
(label headlines and ID bodies).
Saving (C-x C-s) validates and updates `skg-linkstack'
without writing to disk."
  (interactive)
  (let (( buf (get-buffer-create "*skg-linkstack*") ))
    (switch-to-buffer buf)
    (erase-buffer)
    (insert (skg--format-linkstack-as-org))
    (goto-char (point-min))
    (skg--org-mode-with-options)
    (skg-linkstack-mode 1)
    (set-buffer-modified-p nil)
    (message "Edit the linkstack. C-x C-s to save changes.") ))

(defun skg--format-linkstack-as-org ()
  "Format `skg-linkstack' as org-mode text.
Each tree's note becomes a headline, the label its title and the id its
body; a child's headline is one level below its parent's, its title
starting with its edge to it (`skg--linkstack-edge-herald'), \"(aC) \".
Head of stack (most recently pushed) appears at top of buffer."
  (mapconcat (lambda (tree) (skg--format-linkstack-tree-as-org tree 1))
             skg-linkstack
             "\n" ))

(defun skg--format-linkstack-tree-as-org (tree level)
  "TREE as `skg--format-linkstack-as-org' writes it, its root a headline
of LEVEL."
  (let (( edge (skg--linkstack-tree-edge tree) ))
    (mapconcat
     #'identity
     (cons (format "%s %s%s\n%s"
                   (make-string level ?*)
                   (if edge
                       (format "(%s) " (skg--linkstack-edge-herald edge))
                     "")
                   (cadr tree)
                   (car tree))
           (mapcar (lambda (child)
                     (skg--format-linkstack-tree-as-org child (1+ level)))
                   (skg--linkstack-tree-children tree)))
     "\n" )))

(define-minor-mode skg-linkstack-mode
  "Minor mode for editing the skg linkstack.
Overrides save to update `skg-linkstack' instead of writing to a file."
  :lighter " Linkstack"
  :keymap skg-linkstack-mode-map)

;; Hide from M-x: this mode is only activated
;; programmatically by 'skg-view-linkstack'.
(put 'skg-linkstack-mode 'completion-predicate #'ignore)

(provide 'skg-linkstack)
