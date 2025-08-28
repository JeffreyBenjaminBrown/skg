;; PURPOSE
;; See the comment for 'heralds-minor-mode' below.
;;
;; TESTING
;; Try it on the sample text at the end of the file.

;;;###autoload
(define-minor-mode heralds-minor-mode
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
  (if heralds-minor-mode
      (progn
        (heralds-apply-to-buffer)
        (add-hook ;; When a user edits some lines, redisplay heralds only for those lines.
         'after-change-functions
         #'heralds-after-change nil t)
        (add-hook ;; In this case, re-render the entire file. (This might never happen, since a skg view corresponds to no file on disk.)
         'after-revert-hook
         #'heralds-apply-to-buffer nil t))
    (remove-hook 'after-change-functions #'heralds-after-change t)
    (remove-hook 'after-revert-hook #'heralds-apply-to-buffer t)
    (heralds-clear-overlays)))

(defvar-local heralds-overlays nil
  "List of overlays created by `heralds-minor-mode'.")

(defun heralds-apply-to-buffer ()
  "Do `heralds-apply-to-line` to each line."
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (heralds-apply-to-line)
      (forward-line 1))))

(defun heralds-apply-to-line ()
  "On the current line, lens only the first <<...>> occurrence.
Creates one overlay (at most) and pushes it onto `heralds-overlays`."
  (save-excursion
    (let ((bol (line-beginning-position))
          (eol (line-end-position)) )
      (goto-char bol)
      (when (re-search-forward "<<\\([^<>]*\\)>>" eol t)
        (let* ((beg (match-beginning 0))
               (end (match-end 0))
               (inner (match-string-no-properties 1))
               (heralds (heralds-from-metadata inner)) )
          (when heralds
            (let ((ov (make-overlay beg end)) )
              (overlay-put ov 'display heralds)
              (overlay-put ov 'evaporate t)
              (push ov heralds-overlays)) )) )) ))

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

(defun heralds-clear-overlays-in-region (start end)
  "Delete overlays we manage that overlap [START, END)."
  (let (keep)
    (dolist (ov heralds-overlays)
      (if (and (< (overlay-start ov) end)
               (> (overlay-end   ov) start))
          (delete-overlay ov)
        (push ov keep)))
    (setq heralds-overlays (nreverse keep)) ))

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
;; `M-x heralds-minor-mode` should change how the text below looks.
;;
;; Here is some example text <<id:123,blue:Hello,green:World,repeated,other:ignored>> and more text.
;; Another example: <<green:Success,id:456,repeated,blue:Test>> end of line.
;; <<id:789>> A second batch of similarly-formatted data should render normally: <<id:yeah,repeated,aw yeah>>
;; Colors only: <<blue:Azure,green:Forest>>
;; <<foo:Azure,bar:Forest,bazoo>> Metadata with unrecognized keys and values is not rendered at all.

(provide 'heralds-minor-mode)
