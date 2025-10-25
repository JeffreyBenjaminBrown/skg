;;; -*- lexical-binding: t; -*-

;; PURPOSE
;; See the comment for 'heralds-minor-mode' below.
;;
;; TESTING
;; Try it on the sample text at the end of the file.

(require 'cl-lib)
(require 'skg-sexpr)

(defconst heralds--transform-rules
  '(skg (id (ANY "ID"))
        (view (cycle (ANY "⟳"))
              (folded) ;; ignored
              (focused) ;; ignored
              (rels
                (notInParent "!{")
                (containsParent "}")
                (containers (ANY IT "{"))
                (contents (ANY "{" IT))
                (linksIn (ANY IT "→"))))
        (code (relToParent
                (content) ;; ignored
                (aliasCol "aliases")
                (alias "alias")
                (parentIgnores "!{"))
              (indefinitive "indef")
              (toDelete "delete")
              (nodeRequests
                (containerwardView "req:containers")
                (sourcewardView "req:sources"))))
  "Rules to convert metadata sexps into herald tokens.")

(defun heralds--read-metadata (metadata-sexp)
  "Read METADATA-SEXP string into a Lisp object.
Returns nil if parsing fails."
  (condition-case nil
      (car (read-from-string metadata-sexp))
    (error nil)))

(defun heralds--tokens->text (tokens)
  "Convert list of TOKENS (symbols) to plain text display string.
Tokens are symbols created by skg-transform-sexp-flat.
Colons between letters are preserved (like 'req:containers'),
but structural colons added by the transform are removed (like '3:{' -> '3{').
We detect this by checking if both sides of a colon are alphanumeric.
Multiple tokens are separated by spaces.
Hide ID if there are other tokens present."
  (when tokens
    (let* ((token-strings
            (mapcar
             (lambda (token)
               (let ((s (symbol-name token)))
                 ;; Remove colons unless they're between alphanumeric chars
                 (replace-regexp-in-string
                  "\\([^[:alnum:]]\\):\\|:\\([^[:alnum:]]\\)"
                  "\\1\\2"
                  s)))
             tokens))
           (non-id-tokens
            (cl-remove-if
             (lambda (s) (string= s "ID"))
             token-strings)))
      (mapconcat #'identity
                 (if non-id-tokens non-id-tokens token-strings)
                 " "))))

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
                  (overlay-put ov 'evaporate t)
                  (push ov heralds-overlays))))))))))

(defun heralds-clear-overlays ()
  "Remove all overlays from buffer."
  (dolist (ov heralds-overlays)
    (when ;; Exclude invalid overlays.
        (and (overlayp ov) (overlay-buffer ov))
      (delete-overlay ov)))
  (setq heralds-overlays nil))

(defun heralds-clear-overlays-in-region (start end)
  "Delete overlays we manage that overlap [START, END)."
  (let (keep)
    (dolist (ov heralds-overlays)
      (let ((valid (heralds-overlay-valid-and-useable-p ov)))
        (if (and valid
                 (< (overlay-start ov) end)
                 (> (overlay-end ov) start))
            (delete-overlay ov)
          (when valid
            (push ov keep)))))
    (setq heralds-overlays (nreverse keep))))

(defun heralds-from-metadata
    (metadata-sexp) ;; Begins with '(skg ' and ends with ')'.
  "Returns a space-separated string of herald markers.
METADATA-SEXP should be the complete (skg ...) s-expression."
  (let* ((sexp (heralds--read-metadata metadata-sexp))
         (tokens (when (and (listp sexp)
                            (eq (car sexp) 'skg))
                   (skg-transform-sexp-flat sexp heralds--transform-rules))))
    (heralds--tokens->text tokens)))

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
  "White-on-red for REP (repeated).")

(defface heralds-yellow-face
  '((t :foreground "black" :background "yellow"))
  "Black-on-yellow for CYCLE.")

;; TESTING, interactive:
;; `M-x heralds-minor-mode` should change how the text below looks.
;;
;; ID only: (skg (id 123)) should show "ID".
;; View with cycle: (skg (id 456) (view (cycle) (relationships (numContents 3)))) should show "⟳ {3".
;; Code markers: (skg (id 789) (code (relToParent aliasCol))) should show "aliases".
;; Delete marker: (skg (id abc) (code (toDelete))) should show red "delete".
;; Not in parent: (skg (id def) (view (relationships (notInParent)))) should show red "!{".
;; Multiple heralds: (skg (id xyz) (view (cycle) (relationships (numContainers 2) (numLinksIn 5)))) should show "⟳ 2{ 5→".

(provide 'heralds-minor-mode)
