;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Fill in and strip the fields a user may set on a headline,
;;; for the metadata editor ('skg-sexpr-edit.el'), and say what each
;;; field's cycle runs through. The fields of each kind of headline,
;;; their defaults and their values come from
;;; 'shared/metadata-fields.json', whose comments describe them.
;;; After sexp-to-org, filling in inserts each missing field at its
;;; default; before org-to-sexp, stripping removes each field at its
;;; default and writes a value that took arguments as a list.
;;; The Neovim analog is nvim/lua/skg/sexpr/metadata_fields.lua.
;;;
;;; ENTRY POINTS:
;;;   skg-metadata-fields-kind-of-sexp
;;;   skg-metadata-fields-fillIn-defaults-in-org
;;;   skg-metadata-fields-strip-defaults-from-org
;;;   skg-metadata-fields-cycle-values
;;;   skg-metadata-fields-value-arguments
;;;   skg-headlines-to-org

(require 'cl-lib)
(require 'skg-sexpr-org-bijection)
(require 'skg-shared)

;;
;; The table
;;

(defun skg-metadata-fields-kind-of-sexp (sexp)
  "The kind of the metadata SEXP, '(skg ...)', from
'shared/metadata-fields.json', or nil: the kind of the first atom of
SEXP, bare or heading a list, that the table names."
  (when (consp sexp)
    (cl-some (lambda (element)
               (let (( atom (if (consp element) (car element) element) ))
                 (and atom (symbolp atom)
                      (skg-shared-metadata-kind (symbol-name atom)))))
             (cdr sexp))))

(defun skg-metadata-fields-field-names (kind)
  "The names of the fields a user may set on KIND, in the table's order."
  (mapcar (lambda (field) (alist-get 'name field))
          (alist-get 'fields kind)))

(defun skg-metadata-fields--field (kind name)
  "The field of KIND named NAME, or nil."
  (cl-find name (alist-get 'fields kind)
           :key (lambda (field) (alist-get 'name field))
           :test #'equal))

(defun skg-metadata-fields--shown-default (field)
  "FIELD's default as the editor shows it: 'DEFAULT (default)'."
  (concat (alist-get 'default field) " (default)"))

(defun skg-metadata-fields--default-p (field text)
  "Whether TEXT is FIELD's default, shown or bare."
  (member (string-trim text)
          (list (alist-get 'default field)
                (skg-metadata-fields--shown-default field))))

(defun skg-metadata-fields--value-entry (field word)
  "The entry of FIELD's values whose value is WORD, or nil."
  (cl-find word (alist-get 'values field)
           :key (lambda (entry) (alist-get 'value entry))
           :test #'equal))

(defun skg-metadata-fields-cycle-values (kind field-name editable removed-member)
  "What the cycle of KIND's field FIELD-NAME runs through: its default as
shown, then each value the headline may take, given whether it is
EDITABLE (not write-protected) and a REMOVED-MEMBER. Nil when KIND has
no such field."
  (let (( field (skg-metadata-fields--field kind field-name) ))
    (when field
      (cons (skg-metadata-fields--shown-default field)
            (delq nil
                  (mapcar
                   (lambda (entry)
                     (let (( condition (alist-get 'when entry) ))
                       (when (cond ((null condition) t)
                                   ((equal condition "editable") editable)
                                   ((equal condition "removedMember")
                                    removed-member))
                         (alist-get 'value entry))))
                   (alist-get 'values field)))))))

(defun skg-metadata-fields-value-arguments (kind field-name value)
  "The arguments the VALUE of KIND's field FIELD-NAME takes, each an
alist with 'of' (\"id\" or \"skgrepo\") or 'one_of' (a list); nil for
a value that takes none."
  (alist-get 'arguments
             (skg-metadata-fields--value-entry
              (skg-metadata-fields--field kind field-name) value)))

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

(defun skg-metadata-fields--group-children (headlines child-level)
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

(defun skg-metadata-fields--root-index (headlines kind)
  "The index in HEADLINES of the root, '(skg ...)', whose children are
KIND's fields, beside the kind's atom."
  (or (cl-position-if (lambda (hl) (= (car hl) 1)) headlines)
      (error "skg-metadata-fields: no headline holds the fields of a %s"
             (alist-get 'kind kind))))

(defun skg-metadata-fields--map-fields (headlines kind transform)
  "HEADLINES with the children of the root replaced by
(TRANSFORM CHILDREN CHILD-LEVEL), a flat headline list;
CHILDREN is a list of headline groups, one per child."
  (let* (( index (skg-metadata-fields--root-index headlines kind) )
         ( child-level (1+ (car (nth index headlines))) )
         ( groups (skg-metadata-fields--group-children
                   (cl-subseq headlines (1+ index)) child-level) ))
    (append (cl-subseq headlines 0 (1+ index))
            (funcall transform (car groups) child-level)
            (cdr groups))))

;;
;; Fill in
;;

(defun skg-metadata-fields-fillIn-defaults-in-org
    (org-text kind &optional default-skgrepo display-title)
  "ORG-TEXT, the metadata of a headline of KIND as org text, with each
field KIND's headlines may set shown: in canonical order, a missing
one at its default, a bare atom with a 'true' child.
If DEFAULT-SKGREPO is non-nil, insert it as the nodeRepo default
\(in '(skgRepos ...)') and mark an existing nodeRepo value that
matches it with '(default)'.
If DISPLAY-TITLE is non-nil and non-empty, prepend a read-only
title display group."
  (let* (( headlines (org-to-sexp--extract-headlines
                      (split-string org-text "\n")) )
         ( filledIn
           (if (alist-get 'fields kind)
               (skg-metadata-fields--map-fields
                headlines kind
                (lambda (children child-level)
                  (skg-metadata-fields--fillIn-and-reorder
                   children child-level kind default-skgrepo)))
             headlines) ))
    (skg-headlines-to-org
     (skg-metadata-fields--maybe-prepend-title filledIn display-title))))

(defun skg-metadata-fields--maybe-prepend-title (headlines display-title)
  "Maybe prepend the read-only DISPLAY-TITLE group to HEADLINES."
  (if (and display-title
           (not (string-empty-p (string-trim display-title))))
      (append (list (cons 1 "title")
                    (cons 2 display-title))
              headlines)
    headlines))

(defun skg-metadata-fields--fillIn-and-reorder
    (children child-level kind default-skgrepo)
  "CHILDREN (headline groups at CHILD-LEVEL) in canonical order, flat,
each of KIND's fields filled in: the children that say what the
headline is (`skg-metadata-fields--leading-child-p': the markers, the
kind's atom, the gnid and the skgRepos) lead, in their order; the
fields follow, in the table's order; the other children close, in
their order. With DEFAULT-SKGREPO, the skgRepos child shows its
nodeRepo (see `skg-metadata-fields--others-with-skgRepos-default')."
  (let* (( canonical (skg-metadata-fields-field-names kind) )
         ( field-map (mapcar (lambda (group)
                               (cons (string-trim (cdr (car group))) group))
                             children) )
         ( filledIn
           (apply #'append
                  (mapcar (lambda (name)
                            (skg-metadata-fields--fillIn-one
                             name (cdr (assoc name field-map))
                             child-level kind))
                          canonical)) )
         ( others-map (cl-remove-if (lambda (entry)
                                      (member (car entry) canonical))
                                    field-map) )
         ( others-shown
           (if default-skgrepo
               (skg-metadata-fields--others-with-skgRepos-default
                others-map child-level default-skgrepo)
             others-map) ))
    (append (apply #'append
                   (mapcar #'cdr
                           (cl-remove-if-not
                            #'skg-metadata-fields--leading-child-p
                            others-shown)))
            filledIn
            (apply #'append
                   (mapcar #'cdr
                           (cl-remove-if
                            #'skg-metadata-fields--leading-child-p
                            others-shown))))))

(defun skg-metadata-fields--leading-child-p (entry)
  "Whether ENTRY, a (NAME . GROUP) child of the root, says what the
headline is, and so leads it: a marker, a kind's atom, the gnid or
the skgRepos."
  (let (( name (car entry) ))
    (or (member name '("vnid" "focused" "folded" "bodyFolded"
                       "gnid" "skgRepos"))
        (skg-shared-metadata-kind name))))

(defun skg-metadata-fields--fillIn-one
    (name group child-level kind)
  "The headlines that show the child NAME of KIND, whose group is GROUP
or nil when absent: a missing field at its default, a bare atom with a
'true' child."
  (let (( field (skg-metadata-fields--field kind name) ))
    (cond
     ((and field (null group))
      (list (cons child-level name)
            (cons (1+ child-level)
                  (skg-metadata-fields--shown-default field))))
     ((and field (= (length group) 1)
           (equal (alist-get 'shape field) "atom"))
      (list (car group)
            (cons (1+ child-level) "true")))
     (t group))))

(defun skg-metadata-fields--others-with-skgRepos-default
    (others-map child-level default-skgrepo)
  "OTHERS-MAP, the root's non-field children as (NAME . GROUP) at
CHILD-LEVEL, with the skgRepos child's nodeRepo shown against
DEFAULT-SKGREPO: a missing skgRepos (appended) or nodeRepo filled in
as 'DEFAULT-SKGREPO (default)', a nodeRepo value matching it marked
'(default)'."
  (let (( with-default
          (lambda (group)
            (cons "skgRepos"
                  (skg-metadata-fields--skgRepos-with-default
                   group child-level default-skgrepo)))) )
    (if (assoc "skgRepos" others-map)
        (mapcar (lambda (entry)
                  (if (equal (car entry) "skgRepos")
                      (funcall with-default (cdr entry))
                    entry))
                others-map)
      (append others-map
              (list (funcall with-default
                             (list (cons child-level "skgRepos"))))))))

(defun skg-metadata-fields--skgRepos-with-default
    (group child-level default-skgrepo)
  "GROUP, the skgRepos child at CHILD-LEVEL, with its nodeRepo shown
against DEFAULT-SKGREPO (see
`skg-metadata-fields--others-with-skgRepos-default')."
  (let* (( shown-default (concat default-skgrepo " (default)") )
         ( position (cl-position-if
                     (lambda (hl) (and (= (car hl) (1+ child-level))
                                       (string= (string-trim (cdr hl))
                                                "nodeRepo")))
                     group) ))
    (if (null position)
        (append (list (car group)
                      (cons (1+ child-level) "nodeRepo")
                      (cons (+ 2 child-level) shown-default))
                (cdr group))
      (let (( value (nth (1+ position) group) ))
        (if (and value
                 (= (car value) (+ 2 child-level))
                 (string= (string-trim (cdr value)) default-skgrepo))
            (append (cl-subseq group 0 (1+ position))
                    (list (cons (car value) shown-default))
                    (nthcdr (+ 2 position) group))
          group)))))

;;
;; Strip
;;

(defun skg-metadata-fields-strip-defaults-from-org (org-text kind)
  "ORG-TEXT, the metadata of a headline of KIND as org text, ready for
org-to-sexp: without the display-only title group, each field at its
default removed, each bare atom set to 'true' collapsed back to the
atom, each value that took arguments written as a list."
  (let* (( headlines (skg-metadata-fields--remove-title-headlines
                      (org-to-sexp--extract-headlines
                       (split-string org-text "\n"))) ))
    (skg-headlines-to-org
     (if (alist-get 'fields kind)
         (skg-metadata-fields--map-fields
          headlines kind
          (lambda (children child-level)
            (apply #'append
                   (mapcar (lambda (group)
                             (skg-metadata-fields--strip-one
                              group child-level kind))
                           children))))
       headlines))))

(defun skg-metadata-fields--remove-title-headlines (headlines)
  "Remove the display-only title group from HEADLINES, if present."
  (if (and headlines
           (= (caar headlines) 1)
           (string= (string-trim (cdar headlines)) "title"))
      (let ((rest (cdr headlines)))
        (while (and rest (> (caar rest) 1))
          (setq rest (cdr rest)))
        rest)
    headlines))

(defun skg-metadata-fields--strip-one (group child-level kind)
  "The headlines that GROUP, a child of KIND at CHILD-LEVEL, saves as:
nil for a field at its default."
  (let* (( name (string-trim (cdr (car group))) )
         ( field (skg-metadata-fields--field kind name) )
         ( value-text (when (> (length group) 1)
                        (string-trim (cdr (nth 1 group)))) ))
    (cond
     ((and field (equal (alist-get 'shape field) "atom"))
      (cond ((or (null value-text)
                 (skg-metadata-fields--default-p field value-text))
             nil)
            ((string= value-text "true")
             (list (cons child-level name)))
            (t group)))
     (field
      (let (( values
              (apply #'append
                     (mapcar (lambda (value-group)
                               (unless (skg-metadata-fields--default-p
                                        field (cdr (car value-group)))
                                 (skg-metadata-fields--value-as-saved
                                  value-group field)))
                             (car (skg-metadata-fields--group-children
                                   (cdr group) (1+ child-level))))) ))
        (when values
          (cons (cons child-level name) values))))
     ((string= name "skgRepos")
      (when (cdr group)
        (cons (car group)
              (mapcar (lambda (hl)
                        (if (= (car hl) (+ 2 child-level))
                            (cons (car hl)
                                  (skg-metadata-fields--strip-default-suffix
                                   (cdr hl)))
                          hl))
                      (cdr group)))))
     (t group))))

(defun skg-metadata-fields--value-as-saved (value-group field)
  "VALUE-GROUP, one value of FIELD, as it saves: a value that takes
arguments, typed on one line as 'VALUE ARG...', becomes the value with
one child per argument; an id argument typed as a link becomes the
link's id."
  (let* (( head (car value-group) )
         ( level (car head) )
         ( text (string-trim (cdr head)) )
         ( word (car (split-string text)) )
         ( arguments (alist-get 'arguments
                                (skg-metadata-fields--value-entry field word)) ))
    (cond
     ((null arguments) value-group)
     ((cdr value-group)
      (let (( argument-index -1 )) ;; of the child, among the arguments
        (cons head
              (mapcar
               (lambda (child)
                 (if (/= (car child) (1+ level))
                     child
                   (cl-incf argument-index)
                   (if (equal (alist-get 'of (nth argument-index arguments))
                              "id")
                       (cons (car child)
                             (skg-metadata-fields--extract-id-from-text
                              (cdr child)))
                     child)))
               (cdr value-group)))))
     ((string= text word) value-group)
     (t
      (let (( rest (string-trim (substring text (length word))) ))
        (cons (cons level word)
              (mapcar (lambda (argument-text)
                        (cons (1+ level) argument-text))
                      (if (cdr arguments)
                          (split-string rest)
                        (list (if (equal (alist-get 'of (car arguments)) "id")
                                  (skg-metadata-fields--extract-id-from-text rest)
                                rest))))))))))

(defun skg-metadata-fields--strip-default-suffix (text)
  "Strip ' (default)' suffix from TEXT if present."
  (let ((trimmed (string-trim text)))
    (if (string-suffix-p " (default)" trimmed)
        (substring trimmed 0 (- (length trimmed) (length " (default)")))
      trimmed)))

(defun skg-metadata-fields--extract-id-from-text (text)
  "Extract an ID from TEXT, handling org links like [[id:X][label]].
If TEXT is an org link, extracts the ID part. Otherwise returns TEXT trimmed."
  (let ((trimmed (string-trim text)))
    (if (string-match "\\[\\[id:\\([^]]+\\)\\]\\[" trimmed)
        (match-string 1 trimmed)
      trimmed)))

(provide 'skg-metadata-fields)
