;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE
;;; See the comment for 'heralds-minor-mode' below.
;;;
;;; PITFALL: ORPHANED OVERLAYS
;;; Switching major modes (e.g. org -> text -> org) kills all
;;; buffer-local variables, including `heralds-overlays'. But the
;;; overlay objects themselves are attached to the buffer and survive.
;;; With nothing referencing them, they become orphans: still setting
;;; `display' properties on the text, but invisible to our clearing
;;; code. To guard against this, every overlay we create is tagged
;;; with (overlay-put ov 'heralds t), and the clearing functions scan
;;; for that property rather than relying solely on the tracking list.

(require 'cl-lib)
(require 'skg-sexpr-search)

(defconst heralds--transform-rules
  '(skg
    (focused)
    (folded)
    (GREEN aliasCol "aliases")
    (GREEN alias "alias")
    (GREEN hiddenInSubscribeeCol "hiddenIn")
    (GREEN hiddenOutsideOfSubscribeeCol "hiddenOut")
    (GREEN subscribeeCol "subscribees")
    (GREEN idCol "IDs")
    (GREEN id "ID")
    (GREEN textChanged "text changed : "
      (RED staged   "staged")
      (RED unstaged "unstaged"))
    (RED deletedScaffold (ANY "DELETED" IT))
    (RED deleted "DELETED"
      (id)
      (source))
    ;; Per-stage membership axes at the scaffold level (e.g. on
    ;; alias, id). Each INTERC emits at most one token per matching
    ;; stage child, e.g. "staged:-M" or "unstaged:M". Empty
    ;; separator ("") glues the sub-rule outputs directly. The
    ;; outer GREEN colors the `staged:' / `unstaged:' literal
    ;; prefix; each sub-rule still supplies its own color for the
    ;; axis suffix.
    (GREEN INTERC "" staged "staged:"
      (GREEN newM     "M")
      (RED   removedM "-M"))
    (GREEN INTERC "" unstaged "unstaged:"
      (GREEN newM     "M")
      (RED   removedM "-M"))
    (node
      (source) ;; ignored
      ;; Each birth variant is a first-class herald. Content-born
      ;; nodes get a blue `{' (mirror of `}' for ContainerOf); the
      ;; other three births are orange. There is no longer a
      ;; standalone "this is a node" marker.
      (birth
        (BLUE   contentOf   "{")
        (ORANGE independent "⊥")
        (ORANGE containerOf "}")
        (ORANGE linksTo     "←"))
      ;; Indefinitive marker abuts the birth character (no space).
      ;; Server emits the abbreviated atom `indef' (see
      ;; org_to_text.rs); we match that here.
      (GREEN indef ABUT "☮")
      (graphStats
        ;; Server emits (containers N) and (contents M) as raw
        ;; sibling atoms here; the client INTERCs them into a
        ;; compound herald with `{` between. Containers count is
        ;; YELLOW per spec; contents and the separator are BLUE.
        ;; Unlabelled INTERC: sub-rules match the current object's
        ;; (graphStats') children directly, no wrapper needed.
        (BLUE INTERC "{"
          (YELLOW containers (ANY IT))
          (BLUE   contents   (ANY IT)))
        ;; Similarly for the links herald: raw atoms
        ;; (linksInFromContainers N) and (linksInFromLeaves M),
        ;; INTERCed with `→`, whole thing yellow.
        (YELLOW INTERC "→"
          (linksInFromContainers (ANY IT))
          (linksInFromLeaves     (ANY IT)))
        (BLUE aliasing "A")
        (BLUE extraIDs "I")
        (BLUE overriding "O")
        (BLUE subscribing "S")
        (BLUE containerwardPath (ANY IT)))
      (BLUE viewStats
        (BLUE cycle "⟳")
        (containsParent "}")
        (GREEN sourceHerald (ANY IT)))
      (editRequest
        (RED delete "delete")
        (RED merge (ANY "merge:" IT)))
      (GREEN viewRequests
        (aliases "req:aliases")
        (containerwardView "req:containers")
        (containerwardStats "req:cw-stats")
        (sourcewardView "req:sources")
        (definitiveView "req:definitive"))
      ;; Per-stage axes on TrueNodes. Existence axes (X) appear
      ;; only on TrueNodes, never on scaffolds. Outer GREEN colors
      ;; the `staged:' / `unstaged:' prefix; sub-rules keep their
      ;; own axis colors.
      (GREEN INTERC "" staged "staged:"
        (GREEN newX     "X")
        (RED   removedX "-X")
        (GREEN newM     "M")
        (RED   removedM "-M"))
      (GREEN INTERC "" unstaged "unstaged:"
        (GREEN newX     "X")
        (RED   removedX "-X")
        (GREEN newM     "M")
        (RED   removedM "-M"))
      (RED notInGit "diff:not-in-git")))
  "Rules for lensing `(skg ...)` metadata into a line of herald tokens.

This table is the single source of truth for every visual decision
in the herald display. It is interpreted by the generic engine in
skg-lens.el (`skg-transform-sexp-flat'), whose full semantics for
colour directives, ANY/IT, ABUT, and INTERC are documented there.

COLOUR VOCABULARY

  GREEN   -- informational / structural atoms that the user scans for
             orientation: scaffold labels (alias, id, idCol, ...), the
             `staged:'/`unstaged:' prefixes on per-stage axes, source
             herald values, indef marker.
  BLUE    -- node-intrinsic, neutral counts: contents count, aliasing,
             extraIDs, overriding, subscribing, containerwardPath,
             cycle/containsParent markers. Also the ContentOf birth
             glyph `{', chosen to rhyme with containerOf's `}'.
  YELLOW  -- link-flavoured counts, in all directions: the
             `containers' count on a node (how many contain it), plus
             the compound `linksInFromContainers → linksInFromLeaves'.
             Chosen because links are what make the graph a graph.
  RED     -- user-attention markers: deletion (deletedScaffold,
             deleted, editRequest/delete), merge requests, and the
             `-X' / `-M' axis markers for removals in git diffs.
  ORANGE  -- the three non-content birth variants (independent,
             containerOf, linksTo). Distinct from BLUE so ContentOf
             can own that slot without visual collision.

SERVER-EMITTED ATOMS VS. RULE FORM

The server emits metadata atoms as small labeled tuples; this rule
table converts them 1:1 into short herald strings. A few patterns:

  * Simple rules (COLOR? LABEL CHILDREN...) -- e.g.
    `(GREEN alias \"alias\")' matches `(alias \"foo\")' and emits the
    literal `alias' in green. Children are consumed positionally;
    use `(ANY ...)' in a child position to match any leaf/atom.

  * INTERC rules -- used when the server emits a parent whose
    children should be glued together with a separator, preserving
    each child's own colour. Two forms:

    - Labelled: `(GREEN INTERC \"\" staged \"staged:\" ...)' matches
      `(staged ...)' and emits `staged:-M' etc. by running each sub-
      rule against `staged' s children and joining the results.
    - Unlabelled: sub-rules run against the current object's
      own children (no wrapper atom required). Used at
      `(graphStats ...)' level to turn raw sibling atoms like
      `(containers N)' and `(contents M)' into a single `N{M'
      token, without needing a fake wrapper on the server side.

  * ABUT marker -- `(GREEN indef ABUT \"☮\")' tells the renderer to
    glue the `☮' onto the preceding token with no space. Used so
    the indefinitive marker sits directly on its birth glyph.

WHY SOME RULES LOOK EMPTY OR REDUNDANT

  * `(focused)', `(folded)', `(node (source) ...)', `(deleted (id)
    (source))' -- these match and emit nothing, so the atom is
    consumed without contributing to the display. Without these the
    default engine behaviour would reproduce the atom verbatim.

  * `(RED deletedScaffold (ANY \"DELETED\" IT))' vs `(RED deleted
    \"DELETED\" (id) (source))' -- two shapes come in from the server
    depending on whether the deletion is on a scaffold row (like a
    deleted aliasCol) or on a file-level node. Each gets its own
    matcher; both render as `DELETED ...'.

  * Two `(GREEN INTERC \"\" staged ...)' rules and two `(GREEN
    INTERC \"\" unstaged ...)' -- the scaffold-level pair omits the
    `X' / `-X' axes because existence-change markers only apply to
    TrueNodes, not to scaffolds.

NORMALISATION

The server leaves ContentOf birth implicit in `(node ...)'. Before
this table runs, `heralds--read-metadata' calls
`heralds--inject-default-birth' to add `(birth contentOf)' when
missing, so the `(birth (BLUE contentOf \"{\") ...)' sub-rule can
fire uniformly for all four variants.")

(defun heralds--tokens->text (tokens)
  "Convert list of TOKENS (propertized strings) to a display string.
Tokens carry `skg-color' on character ranges (single-color tokens
propertize the whole string; INTERC-built tokens carry per-segment
colors). Tokens separated by a space, except tokens whose position
0 has an `skg-abut' property are joined to the preceding token
with no separator (used to glue e.g. ☮ onto its birth character).
Structural colons added by the transform (like `3:{' -> `3{') are
stripped when either side is non-alphanumeric."
  (when tokens
    (let ((out ""))
      (dolist (tok tokens)
        (let* ((abut    (get-text-property 0 'skg-abut tok))
               (cleaned (heralds--strip-structural-colons tok))
               (faced   (heralds--apply-faces-per-region cleaned)))
          (setq out
                (concat out
                        (if (or (string-empty-p out) abut) "" " ")
                        faced))))
      out)))

(defun heralds--strip-structural-colons (s)
  "Return a copy of S with structural colons removed.
A colon is structural when either the character before or after
it is non-alphanumeric (and not `-', `+', or space, which we keep
since they often appear as label values or separators).
`replace-regexp-in-string' preserves text properties of the kept
characters, which is what we need so ranges' colors survive."
  (replace-regexp-in-string
   "\\([^[:alnum:]+ -]\\):\\|:\\([^[:alnum:]+ -]\\)"
   "\\1\\2"
   s))

(defun heralds--apply-faces-per-region (s)
  "For each region in S where `skg-color' is non-nil, set `face'
to the corresponding herald face. Works for both single-color
tokens and per-segment-colored INTERC tokens."
  (let ((len (length s))
        (pos 0))
    (while (< pos len)
      (let* ((color (get-text-property pos 'skg-color s))
             (next  (or (next-single-property-change pos 'skg-color s)
                        len)))
        (when color
          (put-text-property pos next 'face
                             (heralds--color-to-face color) s))
        (setq pos next)))
    s))

(defun heralds--color-to-face
  (color-keyword)
  "Map COLOR-KEYWORD (RED, GREEN, BLUE, YELLOW, ORANGE) to a face."
  (cond
    ((eq color-keyword 'RED)    'heralds-red-face)
    ((eq color-keyword 'GREEN)  'heralds-green-face)
    ((eq color-keyword 'BLUE)   'heralds-blue-face)
    ((eq color-keyword 'YELLOW) 'heralds-yellow-face)
    ((eq color-keyword 'ORANGE) 'heralds-orange-face)
    (t nil)))

;;;###autoload
(define-minor-mode heralds-minor-mode
  "Display skg metadata as a short list of \"herald\" markers.
Each org headline the server sends starts with `(skg ...)` metadata.
This mode lenses that tree via `skg-transform-sexp-flat`, producing
coloured tokens that summarise view and code information. Every
piece of display logic lives in `heralds--transform-rules'."
  :lighter " ⟪Y⟫"
  (if heralds-minor-mode
      (progn
        (heralds-apply-to-buffer)
        (add-hook ;; When a user edits some lines, redisplay heralds only for those lines.
         'after-change-functions
         #'heralds-after-change nil t)
        (add-hook ;; In this case, re-render the entire file. (This might never happen, since a skg view corresponds to no file on disk.)
         'after-revert-hook
         #'heralds-apply-to-buffer nil t))
    (progn
      (remove-hook 'after-change-functions #'heralds-after-change t)
      (remove-hook 'after-revert-hook #'heralds-apply-to-buffer t)
      (heralds-clear-overlays))))

(defvar-local heralds-overlays nil
  "List of overlays created by `heralds-minor-mode'.")

(defun heralds-apply-to-buffer ()
  "Do `heralds-apply-to-line` to each line."
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (heralds-apply-to-line)
      (forward-line 1))))

(defun heralds-after-change (beg end _len)
  "Refresh overlays only on lines touched by the edit from BEG to END."
  (when heralds-minor-mode
    (save-excursion
      (let* ((lbeg (progn (goto-char beg) (line-beginning-position)) )
             (lend (progn (goto-char end) (line-end-position)) )
             (start-line (line-number-at-pos lbeg))
             (end-line   (line-number-at-pos lend)) )
        (heralds-clear-overlays-in-region lbeg lend)
        (goto-char lbeg)
        (dotimes (_ (1+ (- end-line start-line)) )
          (heralds-apply-to-line)
          (forward-line 1)) )) ))

(defun heralds-apply-to-line ()
  "On the current line, lens only the first (skg ...) occurrence.
Creates one overlay (at most) and pushes it onto `heralds-overlays`."
  (save-excursion
    (let ((bol (line-beginning-position))
          (eol (line-end-position)))
      (goto-char bol)
      (when (search-forward "(skg" eol t)
        (let* ((start (- (point) 4))
               (remaining-text (buffer-substring-no-properties start eol))
               (sexp-end-pos (skg-find-sexp-end remaining-text)))
          (when sexp-end-pos
            (let* ((end (+ start sexp-end-pos -1))
                   (skg-sexp (buffer-substring-no-properties start (1+ end)))
                   (heralds (heralds-from-metadata skg-sexp)))
              (when heralds
                (let ((ov (make-overlay start (1+ end))))
                  (overlay-put ov 'display heralds)
                  (overlay-put ov 'heralds t)
                  (overlay-put ov 'evaporate t)
                  (push ov heralds-overlays))))))))))

(defun heralds-clear-overlays ()
  "Remove all heralds overlays from buffer.
Scans every overlay for the `heralds' property so that orphaned
overlays (e.g. from a major-mode switch that killed the
buffer-local `heralds-overlays' list) are also deleted."
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'heralds)
      (delete-overlay ov)))
  (setq heralds-overlays nil))

(defun heralds-clear-overlays-in-region (start end)
  "Delete heralds overlays that overlap [START, END)."
  (let (keep)
    (dolist (ov heralds-overlays)
      (let ((valid (heralds-overlay-valid-and-useable-p ov)))
        (if (and valid
                 (< (overlay-start ov) end)
                 (> (overlay-end ov) start))
            (delete-overlay ov)
          (when valid
            (push ov keep)))))
    (dolist (ov (overlays-in start end))
      (when (overlay-get ov 'heralds)
        (delete-overlay ov)))
    (setq heralds-overlays (nreverse keep))))

(defun heralds-from-metadata
    (metadata-sexp) ;; Begins with '(skg ' and ends with ')'.
  "Return a display-ready herald string for METADATA-SEXP.
All composition rules live in `heralds--transform-rules'; there
is no post-processing step. Returns nil if METADATA-SEXP doesn't
parse as an `(skg ...)' form."
  (let* ((sexp (heralds--read-metadata metadata-sexp))
         (tokens (when (and (listp sexp)
                            (eq (car sexp) 'skg))
                   (skg-transform-sexp-flat
                    sexp heralds--transform-rules))))
    (heralds--tokens->text tokens)))

(defun heralds--read-metadata (metadata-sexp)
  "Read METADATA-SEXP string into a Lisp object and normalise it.
Returns nil if parsing fails. Normalisation currently means: if
the sexp is an (skg (node ...)) form whose node has no explicit
(birth ...) sub-form, insert (birth contentOf) -- the server
leaves ContentOf implicit (see the note in org_to_text.rs), but
the herald rules want to dispatch on all four birth variants
explicitly."
  (let ((parsed (condition-case nil
                    (car (read-from-string metadata-sexp))
                  (error nil))))
    (heralds--inject-default-birth parsed)))

(defun heralds--inject-default-birth (sexp)
  "If SEXP is `(skg (node ...) ...)` and the node has no (birth ...)
child, return a copy with `(birth contentOf)' inserted into the
node. Otherwise return SEXP unchanged."
  (if (and (listp sexp)
           (eq (car-safe sexp) 'skg))
      (cons 'skg
            (mapcar
             (lambda (child)
               (if (and (listp child) (eq (car-safe child) 'node)
                        (not (cl-some
                              (lambda (sub)
                                (and (listp sub)
                                     (eq (car-safe sub) 'birth)))
                              (cdr child))))
                   ;; insert (birth contentOf) immediately after the
                   ;; `node' symbol; its position in the list doesn't
                   ;; affect matching but keeps the normalised form
                   ;; readable if ever inspected.
                   (cons 'node
                         (cons '(birth contentOf) (cdr child)))
                 child))
             (cdr sexp)))
    sexp))

(defun heralds-overlay-valid-and-useable-p (ov)
  "Check if overlay OV is valid and usable."
  (and (overlayp ov)
       (overlay-buffer ov)
       (overlay-start ov)
       (overlay-end ov)))

(defface heralds-blue-face
  '((t :foreground "white" :background "blue"))
  "White-on-blue for blue values.")

(defface heralds-green-face
  '((t :foreground "white" :background "#006400"))
  "White-on-green for green values.")

(defface heralds-red-face
  '((t :foreground "white" :background "red"))
  "White-on-red for problem markers like delete.")

(defface heralds-yellow-face
  '((t :foreground "black" :background "yellow"))
  "Black-on-yellow for link-count heralds and cycle markers.")

(defface heralds-orange-face
  '((t :foreground "white" :background "#d2691e"))
  "White-on-orange for non-content birth heralds (⊥, }, ←).")

(provide 'heralds-minor-mode)
