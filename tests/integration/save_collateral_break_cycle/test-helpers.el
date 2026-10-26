;;; Shared helpers for save_collateral integration tests.
;;; Load with (load-file "path/to/test-helpers.el").

(defun headline--role-of-chain-node-from-rels (rels-body)
  "Classify a chain node from its semantic relationship facts, or nil.
RELS-BODY is the cdr of the (view (rels ...)) form -- a list of relation
sub-forms like (contains (in ...) (out ...)). A chain node (a
node of the containerward chain) relates outbound to a tracked
ancestor: it has a relation with an (out ... (ancestors ...)) side.
That relation names the role: contains -> container, linksTo ->
mentioner, subscribesTo -> subscribee, overrides -> overrider,
hidesFromSubs -> hider. Only meaningful for a node already known by
its position to be a chain node; an ordinary content child can carry
the same outbound ancestor (e.g. a cycle) yet is not a chain node."
  (cl-loop for (rel . role) in '((contains    . container)
                                 (linksTo . mentioner)
                                 (subscribesTo . subscribee)
                                 (overrides . overrider)
                                 (hidesFromSubs . hider))
           for form = (assq rel rels-body)
           when (and form
                     (let ((out (assq 'out (cdr form))))
                       (and out (assq 'ancestors (cdr out)))))
           return role))

(defun headline--relation-from-sexp (sexp &optional claims-nothing chain-node)
  "Classify a parsed metadata SEXP by membership and chain position.
CLAIMS-NOTHING says it stands under a write-protected node, where no
child claims membership and none carries the atom noClaim; CHAIN-NODE
says it is a node of the containerward chain (see
`headline--position-of'). A chain node returns its ROLENAME from its
(view (rels ...)) facts (e.g. `container'); otherwise the result is
`noClaim' for a child claiming nothing and `member' for a member."
  (let* ((non-member (or claims-nothing
                         (and sexp
                              (skg-sexp-subtree-p sexp '(skg node noClaim)))))
         (rels-body (when sexp
                      (skg-sexp-cdr-at-path sexp '(skg view rels))))
         (chain-node-role (when chain-node
                       (headline--role-of-chain-node-from-rels rels-body))))
    (cond
     ;; A chain node, with an outbound-ancestor herald.
     (chain-node-role chain-node-role)
     (non-member 'noClaim)
     (t 'member))))

(defun headline--kind-of (sexp metadata-str)
  "The kind of a headline: `node', `folder' or nil."
  (cond ((and (consp sexp) (memq 'node (cdr sexp))) 'node)
        ((and metadata-str (string-match-p "Folder" metadata-str)) 'folder)))

(defun headline--position-of (stack depth sexp metadata-str)
  "Classify the headline at DEPTH by its position. STACK holds, nearest
first, (DEPTH KIND WRITE-PROTECTED CHAIN-NODE) for the headlines above
it, deeper ones already popped. Returns (KIND WRITE-PROTECTED
CLAIMS-NOTHING CHAIN-NODE): a node under a write-protected node claims
nothing, and is a chain node when that node is a folder's member or
itself a chain node."
  (let* ((parent (car stack))
         (grandparent (cadr stack))
         (kind (headline--kind-of sexp metadata-str))
         (write-protected (and (consp sexp) (memq 'writeProtected (cdr sexp)) t))
         (under-write-protected-node
          (and (eq kind 'node) parent
               (eq (nth 1 parent) 'node) (nth 2 parent)))
         (chain-node
          (and under-write-protected-node
               (or (nth 3 parent)
                   (and grandparent (eq (nth 1 grandparent) 'folder))))))
    (list kind write-protected under-write-protected-node chain-node)))

(defun headline-structure (buffer)
  "Extract (depth relation id) triples from every headline in BUFFER.
Depth is the number of asterisks. Relation is a membership symbol or
a chain node's role; see `headline--relation-from-sexp'.
ID comes from the (skg node (gnid X) ...) metadata. Headlines
without metadata are skipped."
  (with-current-buffer buffer
    (let ((result '()))
      (save-excursion
        (goto-char (point-min))
        (let ((stack '()))
         (while (not (eobp))
          (let* ((line (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position)))
                 (parts (skg-split-as-stars-metadata-title line)))
            (when parts
              (let* ((stars (nth 0 parts))
                     (metadata-str (nth 1 parts))
                     (depth (length (string-trim-right stars)))
                     (sexp (condition-case nil
                               (car (read-from-string metadata-str))
                             (error nil)))
                     (id-list (when (and (consp sexp)
                                         (memq 'node (cdr sexp)))
                                (skg-sexp-cdr-at-path sexp '(skg gnid))))
                     (skgid (when id-list
                           (format "%s" (car id-list))))
                     (position (progn
                                 (while (and stack (>= (caar stack) depth))
                                   (pop stack))
                                 (headline--position-of
                                  stack depth sexp metadata-str)))
                     (relation (headline--relation-from-sexp
                                sexp (nth 2 position) (nth 3 position))))
                (push (list depth (nth 0 position) (nth 1 position)
                            (nth 3 position))
                      stack)
                (when skgid
                  (push (list depth relation skgid) result)))))
          (forward-line 1))))
      (nreverse result))))

(defun headline-titles (buffer)
  "Extract (depth relation title) triples from every headline in BUFFER.
Depth is the number of asterisks. Relation is a symbol (see
`headline--relation-from-sexp'). Title is the text after metadata."
  (with-current-buffer buffer
    (let ((result '()))
      (save-excursion
        (goto-char (point-min))
        (let ((stack '()))
         (while (not (eobp))
          (let* ((line (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position)))
                 (parts (skg-split-as-stars-metadata-title line)))
            (when parts
              (let* ((stars (nth 0 parts))
                     (title (nth 2 parts))
                     (depth (length (string-trim-right stars)))
                     (metadata-str (nth 1 parts))
                     (sexp (condition-case nil
                               (car (read-from-string metadata-str))
                             (error nil)))
                     (position (progn
                                 (while (and stack (>= (caar stack) depth))
                                   (pop stack))
                                 (headline--position-of
                                  stack depth sexp metadata-str)))
                     (relation (headline--relation-from-sexp
                                sexp (nth 2 position) (nth 3 position))))
                (push (list depth (nth 0 position) (nth 1 position)
                            (nth 3 position))
                      stack)
                (push (list depth relation title) result))))
          (forward-line 1))))
      (nreverse result))))

(defun format-headline-triples (triples)
  "Format a list of (depth relation string) triples for display."
  (mapconcat
   (lambda (triple)
     (format "(%d %s %S)"
             (nth 0 triple) (nth 1 triple) (nth 2 triple)))
   triples ", "))

(defun assert-headline-structure (buffer expected phase-label)
  "Assert that BUFFER's headline structure matches EXPECTED.
EXPECTED is a list of (depth relation id) triples.
PHASE-LABEL is used in log messages. Kills emacs with exit 1 on failure."
  (let ((actual (headline-structure buffer)))
    (if (equal actual expected)
        (message "✓ PASS [%s]: headline-structure is (%s)"
                 phase-label (format-headline-triples actual))
      (progn
        (message "✗ FAIL [%s]: headline-structure mismatch" phase-label)
        (message "  Expected: (%s)" (format-headline-triples expected))
        (message "  Got:      (%s)" (format-headline-triples actual))
        (message "  Buffer content: %S"
                 (with-current-buffer buffer
                   (buffer-substring-no-properties (point-min) (point-max))))
        (kill-emacs 1)))))

(defun assert-headline-titles (buffer expected phase-label)
  "Assert that BUFFER's headline titles match EXPECTED exactly.
EXPECTED is a list of (depth relation title) triples.
PHASE-LABEL is used in log messages. Kills emacs with exit 1 on failure."
  (let ((actual (headline-titles buffer)))
    (if (equal actual expected)
        (message "✓ PASS [%s]: headline-titles is (%s)"
                 phase-label (format-headline-triples actual))
      (progn
        (message "✗ FAIL [%s]: headline-titles mismatch" phase-label)
        (message "  Expected: (%s)" (format-headline-triples expected))
        (message "  Got:      (%s)" (format-headline-triples actual))
        (message "  Buffer content: %S"
                 (with-current-buffer buffer
                   (buffer-substring-no-properties (point-min) (point-max))))
        (kill-emacs 1)))))

(defun headline-types-and-titles (buffer)
  "Extract (depth type title) triples from every headline in BUFFER.
Type is a symbol derived from the metadata:
  node, ghost, tombstone, subscribeeFolder, etc.
For (skg node ...) the type is node; for (skg ghost ...) it is
ghost; for (skg tombstone) it is tombstone; and so on."
  (with-current-buffer buffer
    (let ((result '()))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position)))
                 (parts (skg-split-as-stars-metadata-title line)))
            (when parts
              (let* ((stars (nth 0 parts))
                     (title (nth 2 parts))
                     (depth (length (string-trim-right stars)))
                     (metadata-str (nth 1 parts))
                     (sexp (condition-case nil
                               (car (read-from-string metadata-str))
                             (error nil)))
                     ;; The kind atom: the first top-level element,
                     ;; bare or heading a list, that names a kind.
                     (type-sym
                      (when (consp sexp)
                        (seq-some
                         (lambda (element)
                           (let ((atom (if (consp element)
                                           (car element)
                                         element)))
                             (and (symbolp atom)
                                  (skg-shared-metadata-kind
                                   (symbol-name atom))
                                  atom)))
                         (cdr sexp)))))
                (when type-sym
                  (push (list depth type-sym title) result)))))
          (forward-line 1)))
      (nreverse result))))

(defun format-types-and-titles (triples)
  "Format a list of (depth type title) triples for display."
  (mapconcat
   (lambda (triple)
     (format "(%d %s %S)" (nth 0 triple) (nth 1 triple) (nth 2 triple)))
   triples "\n    "))

(defun assert-headline-types-and-titles (buffer expected phase-label)
  "Assert that BUFFER's (depth type title) triples match EXPECTED.
EXPECTED is a list of (depth type-symbol title-string) triples.
PHASE-LABEL is used in log messages. Kills emacs with exit 1 on failure."
  (let ((actual (headline-types-and-titles buffer)))
    (if (equal actual expected)
        (message "✓ PASS [%s]: headline-types-and-titles match"
                 phase-label)
      (progn
        (message "✗ FAIL [%s]: headline-types-and-titles mismatch"
                 phase-label)
        (message "  Expected:\n    %s" (format-types-and-titles expected))
        (message "  Got:\n    %s" (format-types-and-titles actual))
        (message "  Buffer content: %S"
                 (with-current-buffer buffer
                   (buffer-substring-no-properties (point-min) (point-max))))
        (kill-emacs 1)))))

(provide 'test-helpers)
