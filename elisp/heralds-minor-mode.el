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
  "Black-on-yellow for link-count heralds and cycle markers.")

(defface heralds-orange-face
  '((t :foreground "white" :background "#d2691e"))
  "White-on-orange for non-content parentIs heralds (⊥, }, ←).")

(provide 'heralds-minor-mode)
