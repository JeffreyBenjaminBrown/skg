;; PURPOSE
;; See the comment for 'heralds-minor-mode' below.
;;
;; TESTING
;; Try it on the sample text at the end of the file.

(require 'skg-metadata)
(require 'skg-sexpr)

;;;###autoload
(define-minor-mode heralds-minor-mode
  "When Rust sends a view of the graph to Emacs,
each line (after the org-bullet) begins with some metadata.
The API is in flux -- see api.md -- but might look something like
(skg (id long-string) repeated (key value) another-value)
This minor mode changes how such things are displayed:
.
The id becomes a single '⅄' character. (It looks graphy to me.)
The 'type' key with values 'aliases' or 'searchResult' are displayed
on a blue background, the 'id' key is displayed as '⅄' on a green background,
and the unkeyed value 'repeated' displays as a white 'REP'
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
                   (inner (string-trim (substring skg-sexp 4 (1- (length skg-sexp)))))
                   (heralds (heralds-from-metadata inner)))
              (when heralds
                (let ((ov (make-overlay start (1+ end))))
                  (overlay-put ov 'display heralds)
                  (overlay-put ov 'evaporate t)
                  (push ov heralds-overlays))))))))))

(defun heralds-from-metadata
    (metadata) ;; line's first text inside (not including) (skg and )
  "Returns a propertized space-separated string of heralds.
Whitespace in METADATA is ignored."
  (let* ((parsed (skg-parse-metadata-inner metadata))
         (alist (car parsed))
         (bare-values (cadr parsed))
         (out '())
         (id-val (cdr (assoc "id" alist)))
         (type-val (cdr (assoc "type" alist)))
         (num-containers-val (cdr (assoc "numContainers" alist)))
         (num-contents-val (cdr (assoc "numContents" alist)))
         (num-links-in-val (cdr (assoc "numLinksIn" alist))))

    ;; Build heralds in a particular order.
    (when id-val ;; ID
      (push (propertize "⅄" 'face 'heralds-green-face) out))

    ;; ! (if NOT contained by parent)
    (unless (member "parentIsContainer" bare-values)
      (push (propertize "!" 'face 'heralds-blue-face) out))

    ;; parentIsContent
    (when (member "parentIsContent" bare-values)
      (push (propertize "⮌" 'face 'heralds-green-face) out))

    ;; mightContainMore
    (when (member "might_contain_more" bare-values)
      (push (propertize "+?" 'face 'heralds-blue-face) out))

    ;; cycle
    (when (member "cycle" bare-values)
      (push (propertize "CYCLE" 'face 'heralds-yellow-face) out))

    ;; REP (repeated)
    (when (member "repeated" bare-values)
      (push (propertize "REP" 'face 'heralds-red-face) out))

    ;; aliases
    (when (and type-val (string-equal type-val "aliases"))
      (push (propertize "aliases" 'face 'heralds-blue-face) out))

    ;; searchResult
    (when (and type-val (string-equal type-val "searchResult"))
      (push (propertize "title matches" 'face 'heralds-blue-face) out))

    ;; container count (N})
    (when num-containers-val
      (let ((n (string-to-number num-containers-val)))
        (when (/= n 1)
          (push (propertize (format "%d}" n) 'face 'heralds-green-face) out))))

    ;; links in (N→)
    (when num-links-in-val
      (let ((n (string-to-number num-links-in-val)))
        (when (> n 0)
          (push (propertize (format "%d→" n) 'face 'heralds-green-face) out))))

    ;; content count ({N)
    (when num-contents-val
      (let ((n (string-to-number num-contents-val)))
        (when (> n 0)
          (push (propertize (format "{%d" n) 'face 'heralds-green-face) out))))

    (when out
      (mapconcat #'identity (nreverse out) " "))))

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

(defun heralds-overlay-valid-and-useable-p (ov)
  "Check if overlay OV is valid and usable."
  (and (overlayp ov)
       (overlay-buffer ov)
       (overlay-start ov)
       (overlay-end ov)))

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

(defface heralds-yellow-face
  '((t :foreground "black" :background "yellow"))
  "Black-on-yellow for CYCLE.")

;; TESTING, interactive:
;; `M-x heralds-minor-mode` should change how the text below looks.
;;
;; Here is some example text (skg (id 123) repeated (other ignored)) and more text.
;; Another example: (skg (id 456) repeated (type searchResult)) end of line.
;; (skg (id 789)) A second batch of similarly-formatted data should render normally: (skg (id yeah) repeated)
;; Type aliases test: (skg (id test) (type aliases) (other ignored)) should show blue "aliases".
;; (skg (foo Azure) (bar Forest) bazoo) Metadata with unrecognized keys and values is not rendered at all.

(provide 'heralds-minor-mode)
