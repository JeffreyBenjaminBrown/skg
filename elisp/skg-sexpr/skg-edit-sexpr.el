;;; -*- lexical-binding: t; -*-
;; Utilities for editing s-expressions with DELETE, REPLACE, ENSURE, and merge operations

(defun skg-edit-nested-sexp (target instructions)
  "Edit TARGET s-expression according to INSTRUCTIONS.
TARGET and INSTRUCTIONS are s-expressions (may start with different symbols).
.
INSTRUCTIONS can contain four types of operations:
1. DELETE: (DELETE elem1 elem2 ...) - removes specified elements
2. REPLACE: (REPLACE old new) - replaces old with new
3. ENSURE: (ENSURE elem) - replaces if found, inserts if not
4. Merge: any other list/atom - recursively merges into target
.
For DELETE/REPLACE patterns:
- Atom: matches exact atom
- List (x): matches any list whose car is x
.
Special cases:
- If target is nil: ignore DELETE/REPLACE, ENSURE works normally, merge rest
- If target is empty list: ignore DELETE/REPLACE, ENSURE works normally, merge rest
- If target and instructions have different cars: return target unchanged
.
Returns the edited s-expression."
  (cond
   ;; Case 1: nil target - transform instructions into pure merge
   ((null target)
    (skg-edit--merge-instructions-into-nil instructions))

   ;; Case 2: empty list target - treat as nil with wrapper
   ((null (car target))
    (skg-edit--merge-instructions-into-empty-list instructions))

   ;; Case 3: different starting symbols - return target unchanged
   ((not (eq (car target) (car instructions)))
    target)

   ;; Case 4: normal processing - both have same car
   (t
    (let ((result-car (car target))
          (target-elements (cdr target))
          (instruction-elements (cdr instructions)))
      ;; Process each instruction in order, updating target-elements
      (dolist (instruction-elem instruction-elements)
        (setq target-elements
              (skg-edit--process-single-instruction
               target-elements
               instruction-elem)))
      ;; Reconstruct the result with the car
      (cons result-car target-elements)))))

(defun skg-edit--merge-instructions-into-nil (instructions)
  "Process INSTRUCTIONS for nil target.
Ignores DELETE and REPLACE operations.
ENSURE works normally (replace if found, insert if not).
Merge operations work normally.
Returns a new s-expression starting with instructions' car."
  (let ((result-car (car instructions))
        (instruction-elements (cdr instructions))
        (result-elements '()))
    ;; Process each instruction, filtering and unwrapping as needed
    (dolist (instruction-elem instruction-elements)
      (cond
       ;; Skip DELETE operations entirely
       ((and (listp instruction-elem)
             (eq (car instruction-elem) 'DELETE))
        nil) ; Do nothing - skip this instruction

       ;; Skip REPLACE operations entirely
       ((and (listp instruction-elem)
             (eq (car instruction-elem) 'REPLACE))
        nil) ; Do nothing - skip this instruction

       ;; ENSURE: works normally (replace if found, insert if not)
       ((and (listp instruction-elem)
             (eq (car instruction-elem) 'ENSURE))
        (setq result-elements
              (skg-edit--apply-ensure-operation
               result-elements
               (nth 1 instruction-elem))))

       ;; Everything else: merge normally
       (t
        (setq result-elements
              (skg-edit--apply-merge-operation
               result-elements
               instruction-elem)))))
    ;; Return the constructed result
    (cons result-car result-elements)))

(defun skg-edit--merge-instructions-into-empty-list (instructions)
  "Process INSTRUCTIONS for empty list target.
Similar to nil target.
Ignores DELETE and REPLACE operations.
ENSURE works normally (replace if found, insert if not).
Merge operations work normally."
  (let ((instruction-elements (cdr instructions))
        (result-elements '()))
    ;; Process each instruction, filtering and unwrapping as needed
    (dolist (instruction-elem instruction-elements)
      (cond
       ;; Skip DELETE operations entirely
       ((and (listp instruction-elem)
             (eq (car instruction-elem) 'DELETE))
        nil) ; Do nothing

       ;; Skip REPLACE operations entirely
       ((and (listp instruction-elem)
             (eq (car instruction-elem) 'REPLACE))
        nil) ; Do nothing

       ;; ENSURE: works normally (replace if found, insert if not)
       ((and (listp instruction-elem)
             (eq (car instruction-elem) 'ENSURE))
        (setq result-elements
              (skg-edit--apply-ensure-operation
               result-elements
               (nth 1 instruction-elem))))

       ;; Everything else: merge normally
       (t
        (setq result-elements
              (skg-edit--apply-merge-operation
               result-elements
               instruction-elem)))))
    ;; Return result with instructions' car (not empty list car)
    (cons (car instructions) result-elements)))

(defun skg-edit--process-single-instruction (target-elements instruction-elem)
  "Process a single INSTRUCTION-ELEM against TARGET-ELEMENTS.
Dispatches to appropriate handler based on instruction type.
Returns the modified target-elements."
  (cond
   ;; DELETE operation: (DELETE spec1 spec2 ...)
   ((and (listp instruction-elem)
         (eq (car instruction-elem) 'DELETE))
    (skg-edit--apply-delete-operations
     target-elements
     (cdr instruction-elem)))

   ;; REPLACE operation: (REPLACE old-spec new-value)
   ((and (listp instruction-elem)
         (eq (car instruction-elem) 'REPLACE))
    (skg-edit--apply-replace-operation
     target-elements
     (nth 1 instruction-elem)  ; old-spec
     (nth 2 instruction-elem))) ; new-value

   ;; ENSURE operation: (ENSURE spec)
   ((and (listp instruction-elem)
         (eq (car instruction-elem) 'ENSURE))
    (skg-edit--apply-ensure-operation
     target-elements
     (nth 1 instruction-elem))) ; spec

   ;; Merge operation: any other element
   (t
    (skg-edit--apply-merge-operation
     target-elements
     instruction-elem))))

(defun skg-edit--apply-delete-operations (target-elements delete-specs)
  "Apply multiple DELETE operations to TARGET-ELEMENTS.
DELETE-SPECS is a list of deletion specifications.
Each spec is either an atom (delete exact match) or a singleton list
(delete any list whose car matches).
Returns the modified target-elements."
  (dolist (spec delete-specs)
    (setq target-elements
          (skg-edit--delete-single-element target-elements spec)))
  target-elements)

(defun skg-edit--delete-single-element (target-elements delete-spec)
  "Delete one element matching DELETE-SPEC from TARGET-ELEMENTS.
If DELETE-SPEC is an atom, deletes that exact atom.
If DELETE-SPEC is a singleton list (x), deletes any list whose car is x.
Returns the modified target-elements."
  (if (listp delete-spec)
      ;; Delete any list whose car matches the spec's car
      (let ((key-to-match (car delete-spec)))
        (seq-filter (lambda (elem)
                      ;; Keep elements that don't match
                      (not (and (listp elem)
                                (eq (car elem) key-to-match))))
                    target-elements))
    ;; Delete exact atom match
    (remove delete-spec target-elements)))

(defun skg-edit--apply-replace-operation
    (target-elements old-spec new-value)
  "Replace OLD-SPEC with NEW-VALUE in TARGET-ELEMENTS.
If OLD-SPEC is an atom, replaces that exact atom.
If OLD-SPEC is a singleton list (x), replaces any list whose car is x.
NEW-VALUE can be either an atom or a list.
Returns the modified target-elements."
  (if (listp old-spec)
      ;; Replace any list whose car matches
      (let ((key-to-match (car old-spec)))
        (mapcar (lambda (elem)
                  (if (and (listp elem)
                           (eq (car elem) key-to-match))
                      new-value
                    elem))
                target-elements))
    ;; Replace exact atom match
    (mapcar (lambda (elem)
              (if (eq elem old-spec)
                  new-value
                elem))
            target-elements)))

(defun skg-edit--apply-ensure-operation (target-elements ensure-spec)
  "Ensure ENSURE-SPEC exists in TARGET-ELEMENTS.
If ENSURE-SPEC is an atom:
  - If present: no change
  - If absent: append it
If ENSURE-SPEC is a list (x ...):
  - If list with car x exists: replace it with ensure-spec
  - If no such list exists: append ensure-spec
Returns the modified target-elements."
  (if (listp ensure-spec)
      ;; List: replace if found, append if not found
      (let* ((key-to-match (car ensure-spec))
             (found-matching-list
              (seq-find (lambda (elem)
                          (and (listp elem)
                               (eq (car elem) key-to-match)))
                        target-elements)))
        (if found-matching-list
            ;; Found - replace it
            (mapcar (lambda (elem)
                      (if (and (listp elem)
                               (eq (car elem) key-to-match))
                          ensure-spec
                        elem))
                    target-elements)
          ;; Not found - append it
          (append target-elements (list ensure-spec))))
    ;; Atom: add if not already present (avoid duplicates)
    (if (member ensure-spec target-elements)
        target-elements
      (append target-elements (list ensure-spec)))))

(defun skg-edit--apply-merge-operation (target-elements merge-elem)
  "Apply merge operation for MERGE-ELEM to TARGET-ELEMENTS.
If MERGE-ELEM is an atom:
  - Add it if not already present (avoid duplicates)
If MERGE-ELEM is a list (key ...):
  - If list with matching car exists: recursively edit it
  - If no such list exists: append merge-elem
Returns the modified target-elements."
  (if (listp merge-elem)
      ;; List: find matching list in target and recurse
      (let* ((key-to-match (car merge-elem))
             (found-matching-list
              (seq-find (lambda (elem)
                          (and (listp elem)
                               (eq (car elem) key-to-match)))
                        target-elements)))
        (if found-matching-list
            ;; Found matching list - recursively edit it
            (mapcar (lambda (elem)
                      (if (and (listp elem)
                               (eq (car elem) key-to-match))
                          ;; Recursively edit this nested element
                          ;; Wrap both in 'skg for recursion, then unwrap result
                          (let ((recursed-result
                                 (skg-edit-nested-sexp
                                  (cons 'skg (cdr elem))
                                  (cons 'skg (cdr merge-elem)))))
                            ;; Result is (skg ...), so cons key back on
                            (cons key-to-match (cdr recursed-result)))
                        elem))
                    target-elements)
          ;; No matching list found - append merge-elem
          (append target-elements (list merge-elem))))
    ;; Atom: add if not already present (avoid duplicates)
    (if (member merge-elem target-elements)
        target-elements
      (append target-elements (list merge-elem)))))

(provide 'skg-edit-sexpr)
