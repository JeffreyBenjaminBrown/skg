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

;; Defined in skg-request-herald-rules.el, which requires THIS file, so
;; we cannot require it back (circular). `heralds--ensure-rules' reaches
;; it through `fboundp' instead; this declaration is just for the
;; byte-compiler.
(declare-function skg-herald-rules-ensure "skg-request-herald-rules" ())

(defvar heralds--transform-rules nil
  "Rules for lensing `(skg ...)` metadata into a line of herald tokens.

The table itself LIVES IN RUST (`server/heralds.rs`), the single
source of truth for every visual decision in the herald display:
match atoms, labels, colors, presentation order. Emacs fetches it
over the \"herald rules\" endpoint at connect time
(`skg-request-herald-rules', called by `skg-client-init') and caches
it here; re-running `skg-client-init' re-fetches it. (A lazy
reconnect by a request function does NOT re-fetch -- the cached
table survives, which is correct unless the server binary changed
under the session.) Tests inject a table directly via
`heralds-install-rules' (see `skg-test-install-herald-rules'), so
batch-mode tests need no server.

While this is nil (fetch failed, or not yet connected), enabling
`heralds-minor-mode' first tries to self-heal -- re-fetching the
table via `skg-herald-rules-ensure' (bounded retries, then give up)
-- and only disables itself, with an informative message, if that
fails. There is deliberately no vendored fallback table, which
would re-create the two-homes problem the Rust move ended.

The table is interpreted by the generic engine in skg-lens.el
\(`skg-transform-sexp-flat'), whose full semantics for colour
directives, ANY/IT, ABUT, and INTERC are documented there. The rule
patterns the table uses are documented on `herald_rule_table` in
server/heralds.rs.

No client-side normalisation is needed: the ordinary-content herald is
the orange birth herald the server assembles, `(node ... (birthHerald
\"aC\") ..)', so omitted parentIs=affected content carries no parentIs
herald of its own.")

(defun heralds-install-rules (rules)
  "Install RULES (a list whose car is `skg') as the herald rule table.
Called by the connect-time fetch (`skg-request-herald-rules') and by
tests. Signals an error if RULES does not look like a rule table."
  (unless (and (listp rules)
               (eq (car-safe rules) 'skg))
    (error "heralds-install-rules: not a rule table: %S" rules))
  (setq heralds--transform-rules rules))

(defun heralds--tokens->text (tokens)
  "Convert list of TOKENS (propertized strings) to a display string.
Tokens carry `skg-color' on character ranges (single-color tokens
propertize the whole string; INTERC-built tokens carry per-segment
colors). Tokens separated by a space, except tokens whose position
0 has an `skg-abut' property are joined to the preceding token
with no separator (used to glue e.g. ☮ onto its parentIs character).
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

(defun heralds--ensure-rules ()
  "Return non-nil when the herald rule table is available for display.
The table is session state fetched from the skg server (it lives only
in Rust; see `heralds--transform-rules'). When it is missing -- the
connect-time fetch was dropped, or a botched reload wiped it -- try to
self-heal by re-fetching via `skg-herald-rules-ensure', which retries a
bounded number of times and then gives up rather than spin.

On failure show an informative message and return nil, so the caller
disables heralds cleanly. Never signals."
  (cond
   (heralds--transform-rules t)
   ((not (fboundp 'skg-herald-rules-ensure))
    ;; The fetcher lives in skg-request-herald-rules, loaded with the
    ;; rest of the client; without it we have no way to recover.
    (message "Heralds disabled: not connected to the skg server, \
so no herald rule table is available.")
    nil)
   (t
    (condition-case err
        (or (skg-herald-rules-ensure)
            (progn
              (message "Heralds disabled: the skg server sent no herald \
rule table after repeated attempts.")
              nil))
      (error
       (message "Heralds disabled: could not fetch the herald rule table: %s"
                (error-message-string err))
       nil)))))

;;;###autoload
(define-minor-mode heralds-minor-mode
  "Display skg metadata as a short list of \"herald\" markers.
Each org headline the server sends starts with `(skg ...)` metadata.
This mode lenses that tree via `skg-transform-sexp-flat`, producing
coloured tokens that summarise view and code information. Every
piece of display logic lives in `heralds--transform-rules'."
  :lighter " ⟪Y⟫"
  (if heralds-minor-mode
      (if (not (heralds--ensure-rules))
          ;; No rule table, and self-heal could not get one: heralds
          ;; cannot display, so stay off. `heralds--ensure-rules' has
          ;; already shown an informative message explaining why.
          (setq heralds-minor-mode nil)
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

(defconst heralds--rels-sentinel "__RELS_SPANS__"
  "Placeholder token the server's `rels' rule emits (RELS_SPANS_SENTINEL
in server/heralds.rs). The relationship heralds are per-CHARACTER styled
spans -- more than the rule table's atom-level coloring can express -- so
the rule only POSITIONS them by emitting this sentinel, and
`heralds-from-metadata' swaps it for the spans it renders itself from the
`(rels (COLOR \"text\") ...)' payload.")

(defun heralds-from-metadata
    (metadata-sexp) ;; Begins with '(skg ' and ends with ')'.
  "Return a display-ready herald string for METADATA-SEXP.
Most composition lives in `heralds--transform-rules' (the served rule
table); the one exception is the relationship heralds, whose SEMANTIC
`(rels ...)' facts the rule table only positions with a sentinel token
that this function replaces with `heralds--render-rel-facts' output.
Returns nil if METADATA-SEXP doesn't parse as an `(skg ...)' form."
  (let* ((sexp (heralds--read-metadata metadata-sexp))
         (is-skg (and (listp sexp) (eq (car sexp) 'skg)))
         (tokens (when is-skg
                   (skg-transform-sexp-flat
                    sexp heralds--transform-rules)))
         (rel-str (when is-skg (heralds--render-rel-facts sexp))))
    (heralds--tokens->text
     (heralds--splice-rel-spans tokens rel-str))))

(defun heralds--splice-rel-spans (tokens rel-str)
  "Return TOKENS with the sentinel token replaced by REL-STR.
When REL-STR is nil (no `(rels ...)' payload, so the sentinel should be
absent anyway) any stray sentinel token is dropped."
  (when tokens
    (delq nil
          (mapcar
           (lambda (tok)
             (if (equal (substring-no-properties tok)
                        heralds--rels-sentinel)
                 rel-str ;; may be nil -> removed by delq
               tok))
           tokens))))

(defun heralds--find-rels (sexp)
  "Return the first `(rels ...)' sub-list anywhere within SEXP, else nil."
  (when (consp sexp)
    (if (eq (car-safe sexp) 'rels)
        sexp
      (cl-loop for child in (cdr sexp)
               for found = (and (consp child) (heralds--find-rels child))
               when found return found))))

;; ── relationship heralds: render the server's SEMANTIC facts ─────────
;; ALL presentation lives here (letters, colors, order, count-omission,
;; the a(b,c) link form); the server sends only facts. See
;; TODO/heralds-semantic-wire.org. The nvim client mirrors this exactly
;; (nvim/lua/skg/heralds.lua).

(defconst heralds--rel-order '(contains textlinksTo subscribes overrides hides)
  "Relationship display order: C L S O H.")

(defun heralds--rel-letter (rel)
  "The display letter for relation symbol REL."
  (pcase rel ('contains "C") ('textlinksTo "L") ('subscribes "S")
             ('overrides "O") ('hides "H") (_ "?")))

(defun heralds--rel-base-face (rel)
  "Group base face for REL when it is not the reason-for-being:
C/L blue, S/O/H purple."
  (pcase rel ((or 'contains 'textlinksTo) 'heralds-blue-face)
             (_ 'heralds-purple-face)))

(defun heralds--gen-letters (gens)
  "GENS (generation integers) as sorted distinct letters: 1->a, 2->b, ..."
  (mapconcat (lambda (g) (if (and (integerp g) (>= g 1) (<= g 26))
                             (char-to-string (+ ?a (1- g)))
                           (format "{%s}" g)))
             (sort (delete-dups (copy-sequence gens)) #'<)
             ""))

(defun heralds--rel-side (form side)
  "FORM is a relation form like (contains (in 2 (ancestors 1)) (out 1));
return (COUNT . GENS) for SIDE (`in' or `out'), or nil if absent."
  (let ((s (assq side (cdr form))))
    (when s
      (cons (or (cl-find-if #'integerp (cdr s)) 0)
            (cdr (assq 'ancestors (cdr s)))))))

(defun heralds--rel-side-string (count gens base-face multi)
  "COUNT then ancestor letters, as a propertized string. Omit the count
when it equals the number of ancestors (>=1). BASE-FACE colors the
count, unless MULTI (the contains inbound side) and count > 1, which is
orange. Ancestor letters are always yellow."
  (let* ((letters (heralds--gen-letters (or gens '())))
         (n (length letters))
         (out ""))
    (when (or (> count 0) (> n 0))
      (unless (and (> n 0) (= count n))
        (setq out (propertize (number-to-string count) 'face
                              (if (and multi (> count 1))
                                  'heralds-orange-face base-face))))
      (when (> n 0)
        (setq out (concat out (propertize letters 'face 'heralds-yellow-face)))))
    out))

(defun heralds--ordinary-rel-token (rel form base-face)
  "Render an ordinary (non-link) relation token, or nil if empty."
  (let* ((in  (heralds--rel-side form 'in))
         (out (heralds--rel-side form 'out))
         (in-s  (heralds--rel-side-string
                 (if in (car in) 0) (and in (cdr in)) base-face
                 (eq rel 'contains)))
         (out-s (heralds--rel-side-string
                 (if out (car out) 0) (and out (cdr out)) base-face nil)))
    (unless (and (string-empty-p in-s) (string-empty-p out-s))
      (concat in-s (propertize (heralds--rel-letter rel) 'face base-face)
              out-s))))

(defun heralds--link-rel-token (form base-face)
  "Render the textlinksTo token: the a(b,c) inbound digit form and an
outbound ancestor-letters-only side. FORM = (textlinksTo (in TOTAL
(surprising B) (withContent C)) (out (ancestors ...)))."
  (let* ((in  (assq 'in  (cdr form)))
         (out (assq 'out (cdr form)))
         (in-s (when in
                 (let* ((total (or (cl-find-if #'integerp (cdr in)) 0))
                        (b (cadr (assq 'surprising  (cdr in))))
                        (c (cadr (assq 'withContent (cdr in))))
                        (inner (cond ((and (null b) (null c)) "")
                                     ((null c) (format "(%d)" b))
                                     ((null b) (format "(,%d)" c))
                                     (t (format "(%d,%d)" b c)))))
                   (propertize (format "%d%s" total inner) 'face base-face))))
         (out-s (when out
                  (let ((gens (cdr (assq 'ancestors (cdr out)))))
                    (when gens
                      (propertize (heralds--gen-letters gens)
                                  'face 'heralds-yellow-face))))))
    (unless (and (null in-s) (null out-s))
      (concat (or in-s "") (propertize "L" 'face base-face) (or out-s "")))))

(defun heralds--render-rel-facts (sexp)
  "Render the semantic `(rels ...)' payload in SEXP to one propertized
string, or nil if there is none / it produces nothing. Coloring: group
base (C/L blue, S/O/H purple), the reason-for-being token black-on-white,
ancestor letters black-on-yellow, the contains inbound count>1 orange,
A/I cyan. Tokens are ordered C L S O H A I and space-separated."
  (let ((rels (heralds--find-rels sexp)))
    (when rels
      (let ((birth (cdr (assq 'birth (cdr rels))))
            (tokens '()))
        (dolist (rel heralds--rel-order)
          (let ((form (assq rel (cdr rels))))
            (when form
              (let* ((base (if (memq rel birth) 'heralds-birth-face
                             (heralds--rel-base-face rel)))
                     (tok (if (eq rel 'textlinksTo)
                              (heralds--link-rel-token form base)
                            (heralds--ordinary-rel-token rel form base))))
                (when tok (push tok tokens))))))
        (let ((a (cadr (assq 'aliases (cdr rels)))))
          (when a (push (propertize (format "A%d" a) 'face 'heralds-cyan-face)
                        tokens)))
        (let ((i (cadr (assq 'extraIds (cdr rels)))))
          (when i (push (propertize (format "I%d" i) 'face 'heralds-cyan-face)
                        tokens)))
        (setq tokens (nreverse tokens))
        (when tokens (mapconcat #'identity tokens " "))))))

(defun heralds--read-metadata (metadata-sexp)
  "Read METADATA-SEXP string into a Lisp object.
Returns nil if parsing fails. The ordinary-content herald is now the
orange birth herald the server assembles (an (skg (node ... (birthHerald
\"aC\") ..)) form), so no client-side normalisation is needed: omitted
'affected' membership simply has no parentIs herald of its own."
  (condition-case nil
      (car (read-from-string metadata-sexp))
    (error nil)))

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
  "Black-on-yellow for the ancestor-flag letters (a/b/c...) inside the
relationship-herald spans -- the letters that mark a tracked ancestor as
a member on that side.")

(defface heralds-orange-face
  '((t :foreground "white" :background "#d2691e"))
  "White-on-orange: the multi-contained containers count (the number
before C), the parentIs-independent marker (⊥), and the \"unknown
node\" message.")

(defface heralds-purple-face
  '((t :foreground "white" :background "#8b00ff"))
  "White-on-purple for the S / O / H relationship-herald tokens.")

(defface heralds-cyan-face
  '((t :foreground "black" :background "#00ffff"))
  "Black-on-cyan for the A / I count tokens (cyan is too light for
white text).")

(defface heralds-birth-face
  '((t :foreground "black" :background "white"))
  "Black-on-white for the reason-for-being (birth) relationship token
-- the token that explains why the node was drawn.")

(provide 'heralds-minor-mode)
