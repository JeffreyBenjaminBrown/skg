(defun first-id-property-on-line ()
  "RETURNS the value of the `id` property on this line
(which can be of any type) or nil if there is none."
  (save-excursion
    (let ( (found-pos nil) )
      (setq found-pos (text-property-not-all
                       (line-beginning-position)
                       (line-end-position)
                       'id nil) )
      (when found-pos
        (get-text-property found-pos 'id)))))

(defun org-to-sexp-parse-heading-at-point ()
  "RETURNS an alist with heading and id (if present).
Example: ((heading . STRING) (id . STRING))
ASSUMES point is on a heading."
  (interactive)
  (let* ( (id-value (first-id-property-on-line))
          (heading-value (substring-no-properties
                          (string-trim
                           (org-get-heading t t t t))))
          (is-folded (save-excursion
                       (beginning-of-line)
                       (org-fold-folded-p)))
          (result `((heading . ,heading-value))))
    (when id-value
      (setq result (append result `((id . ,id-value)))))
    (when is-folded
      (setq result (append result '((folded . t)))))
    result))

(defun org-to-sexp-parse-body-at-point ()
  "RETURNS either nil or a string without properties,
of the form (body . STRING).
ASSUMES point is on the first line of a heading body.
MOVES POINT to the first line after the body."
  (beginning-of-line)
  (let ((body-start (point))
        (body-end
         ;; Find the next heading or end of buffer
         (if (re-search-forward "^\\*+ " nil t)
             (match-beginning 0)
           (point-max))))
    (when (< body-start body-end)
      (let ((body-text (string-trim-right
                        (buffer-substring-no-properties
                         body-start body-end))))
        (if ;; Without this, if this body
            ;; were the last thing in the file,
            ;; point does not end up after it.
            (= body-end (point-max))
            (goto-char (point-max)))
        `(body . ,body-text)))))

(defun org-to-sexp-parse-heading-at-point-and-maybe-body
    (&optional focused-line-number)
  "Parses the heading at point and its body text if any.

RETURNS an alist with these keys (some optional):
  heading  : string,
  ?id      : string,
  ?body    : string,
  ?focused : t or absent.
ASSUMES point is on a heading.
SIDE EFFECTS: Moves point to the line just after the parsed content.

If `focused-line-number` is given and lies within the parsed region,
the returned alist will include the pair (focused . t)."
  (let* ((start-line (line-number-at-pos))
         (heading-data (org-to-sexp-parse-heading-at-point))
         (body-sexp nil)
         (result heading-data))
    (forward-line)
    (unless (org-at-heading-p) ;; whether at a heading
      (setq body-sexp (org-to-sexp-parse-body-at-point)))
    (when body-sexp ;; there is a body
      (setq result (append result (list body-sexp))))
    (let ((end-line (line-number-at-pos)))
      (when (and focused-line-number
                 (<= start-line focused-line-number)
                 (<             focused-line-number end-line))
        (setq result (append result '((focused . t)))))
      result)))

(defun org-to-sexp-parse-branch
    (&optional focused-line-number)
  "Parse the current heading, its body,
and (recursively) any child branches.

RETURNS an alist with these keys (some optional):
  heading  : string,
  ?id      : string,
  ?body    : string,
  ?focused : t or absent,
  ?content : list of alists.

ASSUMES point is on a heading."
  (let* ((initial-level (org-outline-level))
         (node-data
          (org-to-sexp-parse-heading-at-point-and-maybe-body
           focused-line-number))
         (child-data nil))
    (when (and (org-at-heading-p) ;; Look for children
               (> (org-outline-level) initial-level))
      ;; PITFALL: One might think it sufficient to test
      ;; for whether org-outline-level is equal to (1 + initial-level).
      ;; But the first "child" in an org file might be indented
      ;; even further than the other children --
      ;; that is, it might skip a generation, or any number of them.
      ;; Nonetheless, this is only processing the immediate children
      ;; of the initial node. Their children in turn are processed
      ;; by the recursive call to `org-to-sexp-parse-branch` below.
      (setq child-data '())
      (while (and (org-at-heading-p)
                  (> (org-outline-level) initial-level))
        (push (org-to-sexp-parse-branch focused-line-number)
              child-data)))
    (when child-data
      (setq node-data
            (append node-data
                    `((content . ,(nreverse child-data))))))
    node-data))

(defun org-to-sexp-parse-all-branches ()
  "Returns an s-exp of the form `(content . results)`, where `results` is a list containing the result of calling `org-to-sexp-parse-branch`.

RETURNS: a single cons cell:
  (content . LIST-OF-ALISTS),
  where the cdr is a list of the kind of thing
  returned by `org-to-sexp-parse-branch`.

PITFALL: The s-expression might look malformed, because there's no (.) shown between `content` and `results`. But that's just a quirk of how Emacs displays a cons cell when the cdr is a list."
  (save-excursion
    (let ((focused-line-number (line-number-at-pos)))
      (progn ;; go to the first heading
        (goto-char (point-min))
        (unless
            (org-at-heading-p)
          (condition-case nil
              (outline-next-heading)
            (error nil))))
      (let ((results nil)) ;; parse each branch
        (while (org-at-heading-p)
          (push (org-to-sexp-parse-branch focused-line-number)
                results))
        `(content . ,(nreverse results))))))

(provide 'org-to-sexp)
