;; PURPOSE
;; See the comment for 'heralds-mode' below.
;;
;; TESTING
;; Try it on the sample text at the end of the file.

;;;###autoload
(define-minor-mode heralds-mode
"When Rust sends a view of the graph to Emacs,
each line (after the org-bullet) begins with some metadata.
The API is in flux -- see api.md -- but might look something like
<<id:long-string, repeated, key:value, another-value>>
This minor mode changes how such things are displayed:
.
The id becomes a single '⅄' character. (It looks graphy to me.)
Keyed values with the keys 'blue' or 'green' are displayed
on a background of that color,
and the unkeyed value 'repeated' displayes as a white 'REP'
on a red background. Those displayed symbols are called 'heralds'.
Whitespace separates each herald from the next.
The metadata is otherwise not displayed."
  :lighter " ⟪Y⟫"
  (if heralds-mode
      (progn
        (heralds-apply-to-buffer)
        (add-hook 'after-change-functions #'heralds-after-change nil t))
    (remove-hook 'after-change-functions #'heralds-after-change t)
    (heralds-clear-overlays)))

(defvar-local heralds-overlays nil
  "List of overlays created by `heralds-mode'.")

(defun heralds-apply-to-buffer ()
  "Do `heralds-apply-on-line` to each line."
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (heralds-apply-on-line)
      (forward-line 1))))

(defun heralds-apply-to-line ()
  "On the current line, lens only the first <<...>> occurrence.
Creates one overlay (at most) and pushes it onto `heralds-overlays`."
  (save-excursion
    (let ((bol (line-beginning-position))
          (eol (line-end-position)))
      (goto-char bol)
      (when (re-search-forward "<<\\([^<>]*\\)>>" eol t)
        (let* ((beg (match-beginning 0))
               (end (match-end 0))
               (inner (match-string-no-properties 1))
               (glyphs (heralds-from-metadata inner)))
          (when glyphs
            (let ((ov (make-overlay beg end)))
              (overlay-put ov 'display glyphs)
              (overlay-put ov 'evaporate t)
              (push ov heralds-overlays))))))))

(defun heralds-from-metadata
    (metadata) ;; line's first text inside (not including) << and >>
  "Returns a propertized space-separated string of heralds.
Whitespace in METADATA is ignored."
  (let* ( (s ;; discard whitespace
           (replace-regexp-in-string "[ \t\n]+" "" metadata))
          (parts (split-string s "," t))
          (out '())) ;; accumulates the output
    (dolist (part parts)
      (if (string-match ":" part)
          (let* ((kv (split-string part ":" t))
                 (k (car kv))
                 (v (cadr kv)))
            (cond
             ((string-equal k "id")
              (push (propertize "⅄" 'face
                                'heralds-green-face)
                    out))
             ((string-equal k "blue")
              (push (propertize (or v "") 'face
                                'heralds-blue-face)
                    out))
             ((string-equal k "green")
              (push (propertize (or v "") 'face
                                'heralds-green-face)
                    out)) ))
        (when (string-equal part "repeated")
          (push (propertize "REP" 'face 'heralds-red-face)
                out))))
    (when out
      (mapconcat #'identity (nreverse out) " ")) ))

(defun heralds-clear-overlays ()
  "Remove all overlays from buffer."
  (mapc #'delete-overlay heralds-overlays)
  (setq heralds-overlays nil))

(defun heralds-after-change (&rest _)
  "Refresh overlays after any buffer change."
  (when heralds-mode
    (heralds-clear-overlays)
    (heralds-apply-to-buffer)))

(defface heralds-blue-face
  '((t :foreground "white" :background "blue"))
  "White-on-blue for blue values.")

(defface heralds-green-face
  '((t :foreground "white" :background "#006400"))
  "White-on-green for green values.")

(defface heralds-red-face
  '((t :foreground "white" :background "red"))
  "White-on-red for REP (repeated).")

;; TESTING, interactive:
;; `M-x heralds-mode` should change how the text below looks.
;;
;; Here is some example text <<id:123,blue:Hello,green:World,repeated,other:ignored>> and more text.
;; Another example: <<green:Success,id:456,repeated,blue:Test>> end of line.
;; <<id:789>> A second batch of similarly-formatted data should render normally: <<id:yeah,repeated,aw yeah>>
;; Colors only: <<blue:Azure,green:Forest>>
;; <<foo:Azure,bar:Forest,bazoo>> Metadata with unrecognized keys and values is not rendered at all.

(provide 'lensed-glyph)
