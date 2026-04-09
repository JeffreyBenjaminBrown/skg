;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: S-left / S-right cycling for sexp-edit buffers.
;;; Dispatch is based on the parent headline text.
;;;
;;; ENTRY POINTS:
;;;   skg-sexp-edit-cycle-left
;;;   skg-sexp-edit-cycle-right

(require 'cl-lib)
(require 'skg-config)
(require 'skg-metadata)
(require 'skg-state)

;;
;; Helpers
;;

(defun skg-sexp-edit--parent-headline-text ()
  "Return the text of the parent headline, trimmed."
  (save-excursion
    (condition-case nil
        (progn
          (outline-up-heading 1)
          (string-trim (org-get-heading t t t t)))
      (error nil))))

(defun skg-sexp-edit--cycle-through (field-value values direction)
  "Cycle FIELD-VALUE through VALUES by DIRECTION (+1 or -1).
Returns the new value. If FIELD-VALUE is not in VALUES, starts at index 0."
  (let* ((idx (or (cl-position field-value values :test #'string=) 0))
         (new-idx (mod (+ idx direction) (length values))))
    (nth new-idx values)))

(defun skg-sexp-edit--cycle-and-replace (field-value values direction)
  "Cycle FIELD-VALUE through VALUES by DIRECTION, then replace the headline."
  (org-edit-headline
   (skg-sexp-edit--cycle-through field-value values direction)))

;;
;; Cycle values per field
;;

(defun skg-sexp-edit--cycle-values-for-field (field-name field-value)
  "Return the list of values to cycle through for FIELD-NAME.
FIELD-VALUE is the current headline text, needed for source defaulting.
Returns nil if the field is not cycleable."
  (cond
   ((string= field-name "indefinitive")
    '("false (default)" "true"))
   ((string= field-name "birth")
    '("contentOf (default)" "independent" "containerOf" "linksTo"))
   ((string= field-name "editRequest")
    '("none (default)" "delete" "merge"))
   ((string= field-name "source") ;; from the config
    (skg-sexp-edit--source-cycle-values field-value))
   ((string= field-name "viewRequests")
    '("none (default)"
      "aliases" "containerwardView" "containerwardStats"
      "sourcewardView" "definitiveView"))))

(defun skg-sexp-edit--source-cycle-values (field-value)
  "Return source names from config as a cycle list.
If FIELD-VALUE has a ' (default)' suffix, prepend it so cycling
starts there rather than jumping to a bare source name."
  (let ((sources (skg--owned-sources)))
    (when sources
      (if (string-suffix-p " (default)" field-value)
          (cons field-value
                (remove (substring field-value 0
                                   (- (length field-value)
                                      (length " (default)")))
                        sources))
        sources))))

;;
;; Dispatch
;;

(defun skg-sexp-edit--cycle (direction)
  "Cycle the headline value at point.
DIRECTION is 1 for right, -1 for left.
Dispatch is based on the parent headline text."
  (unless (org-at-heading-p)
    (user-error "Not on a headline"))
  (let* ((field-value (string-trim (org-get-heading t t t t)))
         (parent (skg-sexp-edit--parent-headline-text)))
    (unless parent
      (user-error "Cannot determine parent field"))
    (let ((values (skg-sexp-edit--cycle-values-for-field
                   parent field-value)))
      (cond
       (values
        (when (string= parent "source")
          (message
           (concat "Warning: if this node already exists in the graph"
                   " with a distinct source, changing it here will"
                   " probably cause problems.")))
        (let ((new-val (skg-sexp-edit--cycle-through
                        field-value values direction)))
          (org-edit-headline new-val)
          (when (string= new-val "merge")
            (let ((id (read-string
                       "Enter merge target ID (or paste a link): ")))
              (org-edit-headline (concat "merge " id))))))
       ;; source with no config: prompt
       ((string= parent "source")
        (message
         (concat "Warning: if this node already exists in the graph"
                 " with a distinct source, changing it here will"
                 " probably cause problems."))
        (org-edit-headline (read-string "Source: " field-value)))
       (t
        (user-error "Field '%s' is not cycleable" parent))))))

;;
;; Interactive commands
;;

(defun skg-sexp-edit-cycle-left ()
  "Cycle the headline value at point to the left (previous)."
  (interactive)
  (skg-sexp-edit--cycle -1))

(defun skg-sexp-edit-cycle-right ()
  "Cycle the headline value at point to the right (next)."
  (interactive)
  (skg-sexp-edit--cycle 1))

(provide 'skg-sexpr-cycling)
