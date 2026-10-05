;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Expand/strip default fields for UnrestrictedVognode metadata editing.
;;; After sexp-to-org, expand inserts missing editable fields with defaults.
;;; Before org-to-sexp, strip removes default-valued fields.
;;;
;;; ENTRY POINTS:
;;;   skg-unrestrictedNode-sexp-p
;;;   skg-unrestrictedNode-expand-defaults-in-org
;;;   skg-unrestrictedNode-strip-defaults-from-org
;;;   skg-headlines-to-org

(require 'skg-sexpr-org-bijection)
(require 'skg-compare-sexpr)

;;
;; Constants
;;

(defconst skg-unrestrictedNode--canonical-field-order
  '("id" "repo"
    "writeProtected" "affectsParent" "birth" "editRequest" "viewRequests")
  "Canonical order for node fields. Fields not in this list go last.")

(defconst skg-unrestrictedNode--editable-defaults
  '(("writeProtected"  . "false (default)")
    ("affectsParent"      . "true (default)")
    ("birth"         . "unremarkable (default)")
    ("editRequest"   . "none (default)")
    ("viewRequests"  . "none (default)"))
  "Alist of editable field names to their default value text.")

;;
;; Predicate
;;

(defun skg-unrestrictedNode-sexp-p (sexp)
  "Return non-nil if SEXP is an UnrestrictedVognode sexp."
  (or (skg-sexp-subtree-p sexp '(skg (node)))
      (skg-sexp-subtree-p sexp '(skg node))))

;;
;; Headlines <-> org text
;;

(defun skg-headlines-to-org (headlines)
  "Convert HEADLINES (list of (LEVEL . TEXT) pairs) to org text."
  (mapconcat
   (lambda (hl)
     (concat (make-string (car hl) ?*) " " (cdr hl)))
   headlines
   "\n"))

;;
;; Expand
;;

(defun skg-unrestrictedNode-expand-defaults-in-org
    (org-text &optional default-skgrepo display-title)
  "Expand default fields in ORG-TEXT for UnrestrictedVognode metadata editing.
Parses org text to headlines, finds the ** node section,
reorders fields to canonical order, inserts missing editable
fields with defaults, and expands bare boolean atoms to have
a value child.
If DEFAULT-REPO is non-nil, insert it as the repo default
and mark existing repo values that match it with '(default)'.
If DISPLAY-TITLE is non-nil and non-empty, prepend a read-only
title display group."
  (let* ((lines (split-string org-text "\n"))
         (headlines (org-to-sexp--extract-headlines lines))
         (expanded (skg-unrestrictedNode--expand-headlines
                    headlines default-skgrepo))
         (with-title
          (skg-unrestrictedNode--maybe-prepend-title
           expanded display-title)))
    (skg-headlines-to-org with-title)))

(defun skg-unrestrictedNode--maybe-prepend-title (headlines display-title)
  "Maybe prepend the read-only DISPLAY-TITLE group to HEADLINES."
  (if (and display-title
           (not (string-empty-p (string-trim display-title))))
      (append (list (cons 1 "title")
                    (cons 2 display-title))
              headlines)
    headlines))

(defun skg-unrestrictedNode--expand-headlines (headlines &optional default-skgrepo)
  "Expand HEADLINES by reordering fields and inserting defaults.
DEFAULT-REPO, if non-nil, is used for repo field defaults.
Returns a new headline list."
  (let* ((node-idx (skg-unrestrictedNode--find-node-headline headlines))
         (node-level (car (nth node-idx headlines)))
         (child-level (1+ node-level))
         (before-node (cl-subseq headlines 0 (1+ node-idx)))
         (after-node (cl-subseq headlines (1+ node-idx)))
         (groups (skg-unrestrictedNode--group-children after-node child-level))
         (children (car groups))
         (remainder (cdr groups))
         (expanded-children
          (skg-unrestrictedNode--expand-and-reorder
           children child-level default-skgrepo)))
    (append before-node expanded-children remainder)))

(defun skg-unrestrictedNode--find-node-headline (headlines)
  "Return the index of the '** node' headline in HEADLINES."
  (let ((idx 0)
        (found nil))
    (while (and (< idx (length headlines)) (not found))
      (let ((hl (nth idx headlines)))
        (when (string= (cdr hl) "node")
          (setq found idx)))
      (unless found
        (setq idx (1+ idx))))
    (unless found
      (error "skg-unrestrictedNode--find-node-headline: no 'node' headline found"))
    found))

(defun skg-unrestrictedNode--group-children (headlines child-level)
  "Split HEADLINES into children of node and remainder.
Children are groups starting at CHILD-LEVEL.
Returns (CHILDREN . REMAINDER) where CHILDREN is a list of
headline groups, each being a list of headlines."
  (let ((children '())
        (current-group nil)
        (rest headlines)
        (done nil))
    (while (and rest (not done))
      (let* ((hl (car rest))
             (level (car hl)))
        (cond
         ((= level child-level)
          (when current-group
            (push (nreverse current-group) children))
          (setq current-group (list hl))
          (setq rest (cdr rest)))
         ((> level child-level)
          (when current-group
            (push hl current-group))
          (setq rest (cdr rest)))
         (t
          (setq done t)))))
    (when current-group
      (push (nreverse current-group) children))
    (cons (nreverse children) rest)))

(defun skg-unrestrictedNode--expand-and-reorder (children child-level
                                        &optional default-skgrepo)
  "Reorder CHILDREN to canonical order and insert missing defaults.
CHILDREN is a list of headline groups. CHILD-LEVEL is the level
for field headlines. DEFAULT-REPO, if non-nil, is used for
repo field defaults. Returns a flat list of headlines."
  (let* ((field-map (skg-unrestrictedNode--children-to-field-map children))
         (canonical skg-unrestrictedNode--canonical-field-order)
         (known-fields (mapcar #'car skg-unrestrictedNode--editable-defaults))
         (ordered '())
         (seen '()))
    ;; Add fields in canonical order
    (dolist (field-name canonical)
      (let ((existing (assoc field-name field-map)))
        (if existing
            (progn
              (push field-name seen)
              (let ((group (cdr existing)))
                (setq ordered
                      (append ordered
                              (skg-unrestrictedNode--maybe-expand-field
                               group child-level field-name
                               default-skgrepo)))))
          ;; Insert default if it's an editable field
          (cond
           ((member field-name known-fields)
            (push field-name seen)
            (let ((default-val
                   (cdr (assoc field-name
                               skg-unrestrictedNode--editable-defaults))))
              (setq ordered
                    (append ordered
                            (list (cons child-level field-name)
                                  (cons (1+ child-level)
                                        default-val))))))
           ;; Insert default skgrepo if missing and default-repo given
           ((and (string= field-name "repo") default-skgrepo)
            (push field-name seen)
            (setq ordered
                  (append ordered
                          (list (cons child-level "repo")
                                (cons (1+ child-level)
                                      (concat default-skgrepo
                                              " (default)"))))))))))
    ;; Add remaining fields not in canonical order (write-protected stats etc.)
    (dolist (entry field-map)
      (unless (member (car entry) seen)
        (setq ordered (append ordered (cdr entry)))))
    ordered))

(defun skg-unrestrictedNode--children-to-field-map (children)
  "Convert CHILDREN (list of headline groups) to an alist of (NAME . GROUP)."
  (mapcar
   (lambda (group)
     (cons (cdr (car group)) group))
   children))

(defun skg-unrestrictedNode--maybe-expand-field (group child-level field-name
                                        default-skgrepo)
  "Expand GROUP for display. Handles booleans and repo defaults.
Returns the group, possibly with a value child added or modified."
  (cond
   ;; Bare boolean atom: expand to have 'true' child
   ((and (= (length group) 1)
         (string= field-name "writeProtected"))
    (list (car group)
          (cons (1+ child-level) "true")))
   ;; Repo field: mark with (default) if it matches
   ((and (string= field-name "repo")
         default-skgrepo
         (= (length group) 2))
    (let ((value (string-trim (cdr (nth 1 group)))))
      (if (string= value default-skgrepo)
          (list (car group)
                (cons (1+ child-level)
                      (concat default-skgrepo " (default)")))
        group)))
   (t group)))

;;
;; Strip
;;

(defun skg-unrestrictedNode-strip-defaults-from-org (org-text)
  "Strip default fields from ORG-TEXT before converting back to sexp.
Removes fields at their default value and collapses boolean
true values back to bare atoms."
  (let* ((lines (split-string org-text "\n"))
         (headlines (org-to-sexp--extract-headlines lines))
         (stripped (skg-unrestrictedNode--strip-headlines headlines)))
    (skg-headlines-to-org stripped)))

(defun skg-unrestrictedNode--strip-headlines (headlines)
  "Strip default-valued fields from HEADLINES. Returns new headline list."
  (let* ((metadata-headlines
          (skg-unrestrictedNode--remove-title-headlines headlines))
         (node-idx (skg-unrestrictedNode--find-node-headline metadata-headlines))
         (node-level (car (nth node-idx metadata-headlines)))
         (child-level (1+ node-level))
         (before-node (cl-subseq metadata-headlines 0 (1+ node-idx)))
         (after-node (cl-subseq metadata-headlines (1+ node-idx)))
         (groups (skg-unrestrictedNode--group-children after-node child-level))
         (children (car groups))
         (remainder (cdr groups))
         (stripped-children
          (skg-unrestrictedNode--strip-children children child-level)))
    (append before-node stripped-children remainder)))

(defun skg-unrestrictedNode--remove-title-headlines (headlines)
  "Remove the display-only title group from HEADLINES, if present."
  (if (and headlines
           (= (caar headlines) 1)
           (string= (string-trim (cdar headlines)) "title"))
      (let ((rest (cdr headlines)))
        (while (and rest (> (caar rest) 1))
          (setq rest (cdr rest)))
        rest)
    headlines))

(defun skg-unrestrictedNode--strip-children (children child-level)
  "Strip default values from CHILDREN. Returns flat headline list."
  (let ((result '()))
    (dolist (group children)
      (let* ((field-name (string-trim (cdr (car group))))
             (stripped (skg-unrestrictedNode--strip-one-field
                        group field-name child-level)))
        (when stripped
          (setq result (append result stripped)))))
    result))

(defun skg-unrestrictedNode--strip-one-field (group field-name child-level)
  "Strip one field GROUP named FIELD-NAME. Returns nil to remove, or headlines.
CHILD-LEVEL is the level of the field headline."
  (let ((value-text
         (when (> (length group) 1)
           (string-trim (cdr (nth 1 group))))))
    (cond
     ((string= field-name "writeProtected") ;; Boolean field
      (cond
       ((or (null value-text)
            (skg-unrestrictedNode--default-false-p value-text))
        nil) ;; remove: at default
       ((string= value-text "true")
        ;; Collapse to bare atom (no children)
        (list (cons child-level field-name)))
       (t group)))
     ((string= field-name "affectsParent")
      (cond
       ((or (null value-text)
            (skg-unrestrictedNode--default-content-p value-text))
        nil) ;; remove: at default (content)
       (t group)))
     ((string= field-name "birth")
      (cond
       ((or (null value-text)
            (skg-unrestrictedNode--default-birth-p value-text))
        nil)
       (t group)))
     ((string= field-name "editRequest")
      (cond
       ((or (null value-text)
            (skg-unrestrictedNode--default-none-p value-text))
        nil) ;; remove: at default
       ((string= value-text "delete")
        ;; (editRequest delete) — keep field + value child
        group)
       ((string= value-text "merge")
        ;; (merge ID) structured as: *** editRequest / **** merge / ***** XYZ
        ;; Extract ID, handle org links in the ID child
        (if (> (length group) 2)
            (let* ((id-text (string-trim (cdr (nth 2 group))))
                   (skgid (skg-unrestrictedNode--extract-id-from-text id-text)))
              (list (cons child-level field-name)
                    (cons (1+ child-level) "merge")
                    (cons (+ child-level 2) skgid)))
          group))
       ((string-prefix-p "merge " value-text)
        ;; User typed "merge <ID>" as a single headline text
        ;; Restructure to nested: **** merge / ***** ID
        (let* ((rest (string-trim
                      (substring value-text (length "merge"))))
               (skgid (skg-unrestrictedNode--extract-id-from-text rest)))
          (list (cons child-level field-name)
                (cons (1+ child-level) "merge")
                (cons (+ child-level 2) skgid))))
       (t group)))
     ((string= field-name "viewRequests")
      (cond
       ((null value-text) nil) ;; childless: drop the field, key and all
       ((and value-text
             (skg-unrestrictedNode--default-none-p value-text)
             (= (length group) 2))
        nil) ;; remove: at default
       (t group)))
     ((string= field-name "repo")
      (when value-text
        (let ((stripped-val ;; strip " (default)" suffix if present
               (skg-unrestrictedNode--strip-default-suffix value-text)))
          (list (cons child-level field-name)
                (cons (1+ child-level) stripped-val)))))
     ;; All other fields: keep as-is
     (t group))))

(defun skg-unrestrictedNode--default-false-p (text)
  "Return non-nil if TEXT represents the default false value."
  (let ((trimmed (string-trim text)))
    (or (string= trimmed "false (default)")
        (string= trimmed "false"))))

(defun skg-unrestrictedNode--default-content-p (text)
  "Return non-nil if TEXT represents the default 'true' value."
  (let ((trimmed (string-trim text)))
    (or (string= trimmed "true (default)")
        (string= trimmed "true"))))

(defun skg-unrestrictedNode--default-birth-p (text)
  "Return non-nil if TEXT represents the default birth value."
  (let ((trimmed (string-trim text)))
    (or (string= trimmed "unremarkable (default)")
        (string= trimmed "unremarkable"))))

(defun skg-unrestrictedNode--default-none-p (text)
  "Return non-nil if TEXT represents the default none value."
  (let ((trimmed (string-trim text)))
    (or (string= trimmed "none (default)")
        (string= trimmed "none"))))

(defun skg-unrestrictedNode--strip-default-suffix (text)
  "Strip ' (default)' suffix from TEXT if present."
  (let ((trimmed (string-trim text)))
    (if (string-suffix-p " (default)" trimmed)
        (substring trimmed 0 (- (length trimmed) (length " (default)")))
      trimmed)))

(defun skg-unrestrictedNode--extract-id-from-text (text)
  "Extract an ID from TEXT, handling org links like [[id:X][label]].
If TEXT is an org link, extracts the ID part. Otherwise returns TEXT trimmed."
  (let ((trimmed (string-trim text)))
    (if (string-match "\\[\\[id:\\([^]]+\\)\\]\\[" trimmed)
        (match-string 1 trimmed)
      trimmed)))

(provide 'skg-unrestrictedNode-defaults)
