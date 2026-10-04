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
(require 'skg-shared)

;; Defined in skg-request-herald-rules.el, which requires THIS file, so
;; we cannot require it back (circular). `heralds--ensure-rules' reaches
;; it through `fboundp' instead; this declaration is just for the
;; byte-compiler.
(declare-function skg-herald-rules-ensure "skg-request-herald-rules" ())

(defvar heralds--transform-rules nil
  "Rules for lensing `(skg ...)` metadata into a line of herald tokens.

The table itself LIVES IN RUST (`server/heralds.rs`) and supplies
non-relationship match atoms, labels, styles, and placement. This
client renders semantic relationship facts and their styles. Emacs fetches it
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
\(`skg-transform-sexp-flat'), whose full semantics for styles
directives, ANY/IT, ABUT, and INTERC are documented there. The rule
patterns the table uses are documented on `herald_rule_table` in
server/heralds.rs.

No client-side normalisation is needed: omitted affectsParent=true
content carries no affectsParent herald of its own. Its relationship
and birth facts arrive in `(rels ...)'.")

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
Tokens carry `skg-style' on character ranges (single-style tokens
propertize the whole string; INTERC-built tokens carry per-segment
styles). Tokens separated by a space, except tokens whose position
0 has an `skg-abut' property are joined to the preceding token
with no separator (used to glue e.g. ☮ onto its affectsParent character).
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
characters, which is what we need so ranges' styles survive."
  (replace-regexp-in-string
   "\\([^[:alnum:]+ -]\\):\\|:\\([^[:alnum:]+ -]\\)"
   "\\1\\2"
   s))

(defun heralds--apply-faces-per-region (s)
  "For each region in S where `skg-style' is non-nil, set `face'
to the corresponding herald face. Works for both single-style
tokens and per-segment-styled INTERC tokens."
  (let ((len (length s))
        (pos 0))
    (while (< pos len)
      (let* ((style (get-text-property pos 'skg-style s))
             (next  (or (next-single-property-change pos 'skg-style s)
                        len)))
        (when style
          (put-text-property pos next 'face
                             (heralds--style-to-face style) s))
        (setq pos next)))
    s))

(defun heralds--style-to-face
  (style-keyword)
  "Map STYLE-KEYWORD, a style name in capitals (e.g. GO), to its face
heralds-STYLE-face, or nil if it names no style."
  (and (memq style-keyword (skg-shared-style-keywords))
       (intern (format "heralds-%s-face"
                       (downcase (symbol-name style-keyword))))))

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
coloured tokens that summarise view and code information. The served
rules place non-relationship tokens; this client renders `(rels ...)'."
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
`(rels (STYLE \"text\") ...)' payload.")

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
na anyway) any stray sentinel token is dropped."
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
;; ALL presentation lives here (letters, styles, order, count-omission,
;; fractions); the server sends only facts. The letters and order come
;; from shared/relations.json, and the tiers and floors from
;; shared/herald-styles.json, which the nvim client
;; (nvim/lua/skg/heralds.lua) reads too. docs/heralds.org states the
;; rules: a side's tier is shown by its carrier, which is its numeral if
;; it shows one, else its ancestor flags; every other glyph shows its
;; floor, except the slash, which shows the side's tier.

(defconst heralds--rel-order
  (mapcar (lambda (relation) (intern (alist-get 'name relation)))
          (skg-shared-relations-in-display-order))
  "Relation symbols in display order, from 'shared/relations.json'.")

(defun heralds--rel-letter (rel)
  "The display letter for relation symbol REL, from
'shared/relations.json'."
  (let ((relation (skg-shared-relation (symbol-name rel))))
    (if relation (alist-get 'letter relation) "?")))

(defun heralds--relationship-style (key)
  "The tier or style named at KEY in the relationship_heralds section of
'shared/herald-styles.json', as a symbol."
  (intern (alist-get key (alist-get 'relationship_heralds
                                    skg-shared-herald-styles))))

(defun heralds--side-tier (rel key)
  "The tier of relation REL's side KEY (`in', `out', or a variant such as
`in_substantive') in the side-tier table."
  (intern (alist-get key (alist-get rel (alist-get 'side_tiers
                                                   (alist-get 'relationship_heralds
                                                              skg-shared-herald-styles))))))

(defun heralds--floor (key)
  "The floor named KEY in 'shared/herald-styles.json'."
  (intern (alist-get key (alist-get 'floors
                                    (alist-get 'relationship_heralds
                                               skg-shared-herald-styles)))))

(defun heralds--max-tier (a b)
  "The greater of tiers A and B, per tier_order."
  (let ((order (mapcar #'intern (alist-get 'tier_order skg-shared-herald-styles))))
    (if (> (cl-position a order) (cl-position b order)) a b)))

(defun heralds--styled (text style)
  "TEXT in the face of STYLE (a symbol such as `high')."
  (propertize text 'face (intern (format "heralds-%s-face" style))))

(defun heralds--gen-list (gens)
  "Return sorted distinct generation integers from GENS."
  (sort (delete-dups (copy-sequence gens)) #'<))

(defun heralds--ancestor-glyph (generation)
  "The ancestor flag for GENERATION: a for the parent, b for the
grandparent, ..., then {N}."
  (if (and (>= generation 1) (<= generation 26))
      (char-to-string (+ ?a (1- generation)))
    (format "{%s}" generation)))

(defun heralds--ancestor-text (gens carrier-tier)
  "Render the ancestor flags GENS. Each shows its floor, or the greater
of its floor and CARRIER-TIER when the flags carry a tier."
  (mapconcat
   (lambda (generation)
     (let ((floor (heralds--floor (if (= generation 1) 'ancestor_a
                                    'ancestor_b_and_higher))))
       (heralds--styled (heralds--ancestor-glyph generation)
                        (if carrier-tier (heralds--max-tier floor carrier-tier)
                          floor))))
   (heralds--gen-list gens) ""))

(defun heralds--part-text (count gens tier &optional members-gens)
  "COUNT then its ancestor flags GENS, carrying TIER. The numeral is
omitted when it equals the number of ancestor members, MEMBERS-GENS
\(default GENS); then the flags carry the tier."
  (let* ((gens (heralds--gen-list (or gens '())))
         (show-numeral
          (not (= count (length (heralds--gen-list (or members-gens gens)))))))
    (concat (if show-numeral
                (heralds--styled (number-to-string count) tier) "")
            (heralds--ancestor-text gens (unless show-numeral tier)))))

(defun heralds--side-text (count gens tier)
  "Render a side with COUNT members and ancestor flags GENS, at TIER."
  (if (and (= count 0) (null gens)) ""
    (heralds--part-text count gens tier)))

(defun heralds--fraction-text
    (total total-gens numerator numerator-gens side-tier subset-tier)
  "Render a side with a subset of its own tier: NUMERATOR (with flags
NUMERATOR-GENS, at SUBSET-TIER) of TOTAL (with flags TOTAL-GENS, at
SIDE-TIER). The slash shows SIDE-TIER."
  (unless (<= numerator total)
    (error "Herald subset %s exceeds total %s" numerator total))
  (let* ((total-gens (heralds--gen-list (or total-gens '())))
         (numerator-gens (heralds--gen-list (or numerator-gens '())))
         (remaining-gens (cl-set-difference total-gens numerator-gens)))
    (cond ((= total 0) "")
          ((= numerator 0)
           (heralds--side-text total total-gens side-tier))
          (t (concat (heralds--part-text numerator numerator-gens subset-tier)
                     (heralds--styled "/" side-tier)
                     (if (= numerator total) ""
                       (heralds--part-text total remaining-gens side-tier
                                           total-gens)))))))

(defun heralds--side-facts (form side)
  "FORM is a relation form like (contains (in 2 (ancestors 1)) (out 1));
return (COUNT GENS SUBFORMS) for SIDE (`in' or `out'), or nil if absent."
  (let ((s (assq side (cdr form))))
    (when s
      (list (or (cl-find-if #'integerp (cdr s)) 0)
            (cdr (assq 'ancestors (cdr s)))
            (cdr s)))))

(defun heralds--subset-facts (side-facts key)
  "The (COUNT . GENS) of subset KEY (e.g. `substantive') in SIDE-FACTS."
  (let ((subset (assq key (nth 2 side-facts))))
    (when subset
      (cons (or (cl-find-if #'integerp (cdr subset)) 0)
            (cdr (assq 'ancestors (cdr subset)))))))

(defun heralds--birth-explained-p (rel side count gens birth)
  "Non-nil if the side's only members are ancestors that BIRTH, a list
of birth facts (RELATION SIDE [GEN]), already accounts for."
  (and gens
       (= count (length (heralds--gen-list gens)))
       (cl-every (lambda (generation)
                   (cl-some (lambda (fact)
                              (and (eq (nth 0 fact) rel)
                                   (eq (nth 1 fact) side)
                                   (eql (nth 2 fact) generation)))
                            birth))
                 gens)))

(defun heralds--rel-side-text (rel side form birth write-protected)
  "Render relation REL's SIDE from FORM."
  (let ((facts (heralds--side-facts form side)))
    (if (not facts) ""
      (let* ((count (nth 0 facts))
             (gens (nth 1 facts))
             (tier (cond ((heralds--birth-explained-p rel side count gens birth)
                          (heralds--relationship-style 'birth_explained_side))
                         ((and (eq rel 'contains) (eq side 'out) write-protected)
                          (heralds--side-tier rel 'out_write_protected))
                         (t (heralds--side-tier rel side))))
             (subset-key (cond ((and (eq rel 'contains) (eq side 'out)) 'unintegrated)
                               ((and (eq rel 'links_to) (eq side 'in)) 'substantive)))
             (subset (and subset-key (heralds--subset-facts facts subset-key))))
        (if subset
            (heralds--fraction-text
             count gens (car subset) (cdr subset) tier
             (heralds--side-tier rel (if (eq subset-key 'unintegrated)
                                         'out_unintegrated 'in_substantive)))
          (heralds--side-text count gens tier))))))

(defun heralds--rel-token (rel form birth write-protected overrides-here)
  "Render relation REL's token from FORM, or nil if it has nothing to show."
  (let ((in-s (heralds--rel-side-text rel 'in form birth write-protected))
        (out-s (heralds--rel-side-text rel 'out form birth write-protected))
        (here (and (eq rel 'overrides_view_of) overrides-here)))
    (unless (and (string-empty-p in-s) (string-empty-p out-s) (not here))
      (concat in-s
              (heralds--styled (heralds--rel-letter rel)
                               (if (assq rel birth)
                                   (heralds--relationship-style 'birth_letter)
                                 (heralds--relationship-style 'letter)))
              (if here (heralds--styled "ĥ" (heralds--relationship-style
                                             'overrides_here))
                "")
              out-s))))

(defun heralds--render-rel-facts (sexp)
  "Render the semantic `(rels ...)' payload in SEXP to one propertized
string, or nil if there is none / it produces nothing. Tokens are the
relations in display order (C L S O H), then the property counts A I F,
space-separated."
  (let ((rels (heralds--find-rels sexp)))
    (when rels
      (let* ((node (cdr (assq 'node (cdr sexp))))
             (birth (cdr (assq 'birth (cdr rels)))) ;; facts (RELATION SIDE [GEN])
             (write-protected (memq 'writeProtected node))
             (overrides-here
              (assq 'overridesHere (cdr (assq 'viewStats node))))
             (property-style (heralds--relationship-style 'property_counts))
             (tokens '()))
        (dolist (rel heralds--rel-order)
          (let ((form (assq rel (cdr rels))))
            (when (or form (and (eq rel 'overrides_view_of) overrides-here))
              (let ((tok (heralds--rel-token rel form birth write-protected
                                             overrides-here)))
                (when tok (push tok tokens))))))
        (dolist (count '((aliases . "A") (extraIds . "I") (flags . "F")))
          (let ((k (cadr (assq (car count) (cdr rels)))))
            (when k (push (heralds--styled (format "%s%d" (cdr count) k)
                                           property-style)
                          tokens))))
        (setq tokens (nreverse tokens))
        (when tokens (mapconcat #'identity tokens " "))))))

(defun heralds--read-metadata (metadata-sexp)
  "Read METADATA-SEXP string into a Lisp object.
Returns nil if parsing fails. Omitted affectsParent=true membership
has no affectsParent herald of its own; relationship and birth facts
are rendered from `(rels ...)'."
  (condition-case nil
      (car (read-from-string metadata-sexp))
    (error nil)))

(defun heralds-overlay-valid-and-useable-p (ov)
  "Check if overlay OV is valid and usable."
  (and (overlayp ov)
       (overlay-buffer ov)
       (overlay-start ov)
       (overlay-end ov)))

;; One face per herald style, heralds-STYLE-face, defined from
;; shared/herald-styles.json, which the Neovim client also reads.
(defun heralds--define-style-faces ()
  "Define the face heralds-STYLE-face for each style in
'shared/herald-styles.json'."
  (dolist (style (skg-shared-styles))
    (let* ((name (symbol-name (car style)))
           (look (cdr style))
           (foreground (alist-get 'foreground look))
           (background (alist-get 'background look))
           (underline (alist-get 'underline look)))
      (custom-declare-face
       (intern (format "heralds-%s-face" name))
       `((t ,@(and foreground (list :foreground foreground))
            ,@(and background (list :background background))
            ,@(and underline (list :underline t))))
       (format "The herald style %s; see shared/herald-styles.json." name)
       :group 'faces))))

(heralds--define-style-faces)

(provide 'heralds-minor-mode)
