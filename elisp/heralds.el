;; PURPOSE
;; When Rust sends a view of the graph to Emacs,
;; each line (after the org-bullet) begins with some metadata.
;; The API is in flux -- see api.md -- but might look something like
;; <<id:long-string, repeated, key:value, another-value>>
;; This file defines a minor mode, herald-mode,
;; which changes how such things are displayed:
;;
;; The id becomes a single "⅄" character,
;; keyed values with the keys 'blue' or 'green' are displayed
;; on a background of that color,
;; and the unkeyed value 'repeated' displayes as a white 'REP'
;; on a red background. Those displayed symbols are called 'heralds'.
;; Whitespace separates each herald from the next.
;; The metadata is otherwise not displayed.

(defface herald-blue-face
  '((t :foreground "white" :background "blue"))
  "White-on-blue for blue values.")

(defface herald-green-face
  '((t :foreground "white" :background "#006400"))
  "White-on-green for green values.")

(defface herald-red-face
  '((t :foreground "white" :background "red"))
  "White-on-red for REP (repeated).")

(defvar-local heralds-overlays nil
  "List of overlays created by `herald-mode'.")

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
                                'herald-green-face)
                    out))
             ((string-equal k "blue")
              (push (propertize (or v "") 'face
                                'herald-blue-face)
                    out))
             ((string-equal k "green")
              (push (propertize (or v "") 'face
                                'herald-green-face)
                    out)) ))
        (when (string-equal part "repeated")
          (push (propertize "REP" 'face 'herald-red-face)
                out))))
    (when out
      (mapconcat #'identity (nreverse out) " ")) ))

(defun heralds-clear-overlays ()
  "Remove all overlays from buffer."
  (mapc #'delete-overlay heralds-overlays)
  (setq heralds-overlays nil))

(defun heralds-apply ()
  "Change how the first <<...>> on each line appears."
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (let ((bol (line-beginning-position))
            (eol (line-end-position)))
        ;; Search within the current line. Stop at the first match.
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
                (push ov heralds-overlays)))))
        ;; Move to next line unconditionally.
        (forward-line 1)))))

;;;###autoload
(define-minor-mode herald-mode
  "Toggle glyph rendering of <<...>> passages across the buffer.
Per line, only the FIRST <<...>> is rendered.
- id: show “⅄”
- blue: show its value white-on-blue
- green: show its value white-on-green
- value \"repeated\": show “REP” white-on-red
Other keys/values ignored. Only display is changed."
  :lighter " ⟪Y⟫"
  (if herald-mode
      (progn
        (heralds-apply)
        (add-hook 'after-change-functions #'heralds-after-change nil t))
    (remove-hook 'after-change-functions #'heralds-after-change t)
    (heralds-clear-overlays)))

(defun heralds-after-change (&rest _)
  "Refresh overlays after any buffer change."
  (when herald-mode
    (heralds-clear-overlays)
    (heralds-apply)))

;; Some text for interactive testing.
;; Toggle `M-x herald-mode` and it should change how it looks.
;;
;; Here is some example text <<id:123,blue:Hello,green:World,repeated,other:ignored>> and more text.
;; Another example: <<green:Success,id:456,repeated,blue:Test>> end of line.
;; Simple case: <<id:789>> just an ID. A second batch of metadata should look uncheed: <<id:yeah,repeated,aw yeah>>
;; Colors only: <<blue:Azure,green:Forest>> no ID or repeated.

(provide 'lensed-glyph)
