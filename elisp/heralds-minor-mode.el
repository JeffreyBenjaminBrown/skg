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
    (BLUE staged
      (GREEN newM     "staged:M")
      (RED   removedM "staged:-M"))
    (BLUE unstaged
      (GREEN newM     "unstaged:M")
      (RED   removedM "unstaged:-M"))
    (BLUE node
      (ANY "◌") ;; For nodes with no ID. heralds--post-process-text removes it if there's an ID.
      (id (ANY "●")) ;; For nodes with an ID. heralds--post-process-text removes it if there are non-default stats.
      (source) ;; ignored
      (RED birth
        (independent "!{")
        (containerOf "}")
        (linksTo "←"))
      (GREEN indefinitive "☮")
      (BLUE graphStats
        (containers)            ;; instead we show the containsHerald
        (contents)              ;; instead we show the containsHerald
        (containsHerald (ANY IT))
        (linksInFromContainers) ;; instead we show the linksHerald
        (linksInFromLeaves)     ;; instead we show the linksHerald
        (linksHerald (ANY IT))
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
      (BLUE staged
        (GREEN newX     "staged:X")
        (RED   removedX "staged:-X")
        (GREEN newM     "staged:M")
        (RED   removedM "staged:-M"))
      (BLUE unstaged
        (GREEN newX     "unstaged:X")
        (RED   removedX "unstaged:-X")
        (GREEN newM     "unstaged:M")
        (RED   removedM "unstaged:-M"))
      (RED notInGit "diff:not-in-git")))
  "Rules to convert metadata sexps into herald tokens.")

(defun heralds--tokens->text (tokens)
  "Convert list of TOKENS (propertized strings) to display string.
Tokens are propertized strings created by skg-transform-sexp-flat.
Colons between letters are preserved (like 'req:containers').
Colons followed by '-' or '+' are also preserved (e.g. 'staged:-M').
Colons with a space on either side are preserved too, so literal
prefixes from rule strings (e.g. \"text changed : \") survive.
Structural colons added by the transform are removed (like '3:{' -> '3{').
We detect this by checking if both sides of a colon are alphanumeric --
treating '-', '+', and space as alphanumeric for this purpose since they
often appear as label values (sign markers) or as literal separators.
Multiple tokens are separated by spaces.
Hide ID if there are other tokens present."
  (when tokens
    (let* ((token-strings
            (mapcar
             (lambda (token)
               (let* ((s token)
                      (cleaned
                        (replace-regexp-in-string
                          "\\([^[:alnum:]+ -]\\):\\|:\\([^[:alnum:]+ -]\\)"
                          "\\1\\2"
                          s))
                      (color (get-text-property 0 'skg-color token)))
                 (when color
                   (put-text-property
                     0 (length cleaned)
                     'face (heralds--color-to-face color)
                     cleaned))
                 cleaned))
             tokens))
           (token-strings (heralds--merge-stage-tokens token-strings))
           (non-id-tokens
            (cl-remove-if
             (lambda (s)
               (string= (substring-no-properties s) "ID"))
             token-strings)))
      (mapconcat #'identity
                 (if non-id-tokens non-id-tokens token-strings)
                 " "))))

(defun heralds--stage-prefix (s)
  "If S starts with 'staged:' or 'unstaged:', return that prefix
\(without the trailing colon, e.g. 'staged'). Otherwise nil."
  (cond
   ((string-prefix-p "staged:"   s) "staged")
   ((string-prefix-p "unstaged:" s) "unstaged")
   (t nil)))

(defun heralds--merge-stage-tokens (tokens)
  "Merge adjacent TOKENS that share a 'staged:' or 'unstaged:' prefix.
Suffixes are concatenated (so 'staged:X' + 'staged:M' becomes
'staged:XM' and 'staged:-X' + 'staged:-M' becomes 'staged:-X-M').
Adjacent stage tokens with /different/ stage prefixes (e.g. 'staged:M'
followed by 'unstaged:M') get glued together by ',' rather than the
default ' ', producing 'staged:M,unstaged:M'.
Color: if all merged suffixes within a stage have the same face, keep
it; if mixed, the merged token uses heralds-yellow-face."
  (let ((result nil)
        (current nil)
        (current-prefix nil)
        (current-faces nil))
    (cl-flet ((flush ()
                (when current
                  (let ((merged (mapconcat #'identity (nreverse current) "")))
                    (when current-faces
                      (let ((face (if (= 1 (length (delete-dups (copy-sequence current-faces))))
                                       (car current-faces)
                                     'heralds-yellow-face)))
                        (put-text-property 0 (length merged) 'face face merged)))
                    ;; If the previous result element is also a stage
                    ;; token, glue this one onto it with ','.
                    (let ((prev (car result)))
                      (if (and prev (heralds--stage-prefix prev))
                          (setcar result (concat prev "," merged))
                        (push merged result))))
                  (setq current nil current-prefix nil current-faces nil))))
      (dolist (tok tokens)
        (let ((p (heralds--stage-prefix tok)))
          (cond
           ;; Continuation of the current group: strip the prefix,
           ;; keep just the suffix (the part after "staged:" / "unstaged:").
           ((and p current-prefix (string= p current-prefix))
            (push (substring tok (1+ (length p))) current)
            (push (get-text-property 0 'face tok) current-faces))
           ;; Start a new stage group.
           (p
            (flush)
            (setq current (list tok)
                  current-prefix p
                  current-faces (list (get-text-property 0 'face tok))))
           ;; Non-stage token: flush the group, then pass the token through.
           (t
            (flush)
            (push tok result)))))
      (flush))
    (nreverse result)))

(defun heralds--color-to-face
  (color-keyword)
  "Map COLOR-KEYWORD (RED, GREEN, BLUE, YELLOW) to a face."
  (cond
    ((eq color-keyword 'RED)    'heralds-red-face)
    ((eq color-keyword 'GREEN)  'heralds-green-face)
    ((eq color-keyword 'BLUE)   'heralds-blue-face)
    ((eq color-keyword 'YELLOW) 'heralds-yellow-face)
    (t nil)))

;;;###autoload
(define-minor-mode heralds-minor-mode
  "Display skg metadata as a short list of \"herald\" markers.
Each org headline the server sends starts with `(skg ...)` metadata.
This mode lenses that tree via `skg-transform-sexp-flat`,
producing colored tokens that summarise view and code information.
View-related heralds render blue, code heralds green,
while problem markers such as `!{` and `delete` render red.
The primary id is hidden whenever any other herald is present.
Whitespace separates the heralds; the raw metadata is otherwise hidden."
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
  "Returns a space-separated string of herald markers.
METADATA-SEXP should be the complete (skg ...) s-expression."
  (let* ((sexp (heralds--read-metadata metadata-sexp))
         (tokens (when (and (listp sexp)
                            (eq (car sexp) 'skg))
                   (skg-transform-sexp-flat
                    sexp heralds--transform-rules)))
         (heralds (heralds--tokens->text tokens)))
    (heralds--post-process-text heralds sexp)))

(defun heralds--read-metadata (metadata-sexp)
  "Read METADATA-SEXP string into a Lisp object.
Returns nil if parsing fails."
  (condition-case nil
      (car (read-from-string metadata-sexp))
    (error nil)))

(defun heralds--post-process-text
    (herald-string sexp)
  "Some post-processing rules for heralds:
- Remove ◌ if id present in SEXP
- Remove ⦿ if graphStats or viewStats present in SEXP"
  (when herald-string
    (let* ((heralds (split-string herald-string " " t))
           (heralds (heralds--remove-token-if-sexp-matches-structure
                   "◌" heralds sexp '(skg (node (id)) )) )
           (heralds (heralds--remove-token-if-sexp-matches-structure
                   "⦿" heralds sexp '(skg (node (graphStats)) )) )
           (heralds (heralds--remove-token-if-sexp-matches-structure
                   "⦿" heralds sexp '(skg (node (viewStats)) )) ))
      (mapconcat #'identity heralds " "))))

(defun heralds--remove-token-if-sexp-matches-structure
    (token tokens sexp structure)
  "Remove TOKEN from TOKENS if STRUCTURE is present in SEXP."
  (if (skg-sexp-subtree-p sexp structure)
      (cl-remove-if (lambda (part)
                      (string= (substring-no-properties part) token))
                    tokens)
    tokens))

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
  "White-on-red for problem markers like !{ and delete.")

(defface heralds-yellow-face
  '((t :foreground "black" :background "yellow"))
  "Black-on-yellow for CYCLE.")

(provide 'heralds-minor-mode)
