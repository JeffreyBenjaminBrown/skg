;;; -*- lexical-binding: t; -*-
;;; Display-only status and optional home-repo suffixes for Skg links.

(require 'cl-lib)
(require 'subr-x)
(require 'heralds-minor-mode)
(require 'skg-state)

(defconst skg-link-annotations--regexp
  "\\[\\[id:\\([^]\n]+\\)\\]\\[\\([^]\n]*\\)\\]\\]"
  "Capture a literal Skg link's target ID and label in a view buffer.")

(defvar skg-link-annotations--cache (make-hash-table :test 'equal)
  "Status by literal ID for the current graph and repo-set lifetime.")
(defvar skg-link-annotations--requests (make-hash-table :test 'equal)
  "Outstanding request ID to (buffer buffer-generation tick epoch ids).")
(defvar skg-link-annotations--next-request 0)
(defvar skg-link-annotations--epoch 0)

(defvar-local skg-link-annotations--repo-suffix-enabled nil
  "Whether this buffer displays repo suffixes on Skg links.")
(put 'skg-link-annotations--repo-suffix-enabled 'permanent-local t)
(defvar-local skg-link-annotations--buffer-generation 0)
(defvar-local skg-link-annotations--timer nil)

(defun skg-toggle-repo-overlay-on-links ()
  "Toggle display-only repo suffixes for Skg links in this view."
  (interactive)
  (unless (derived-mode-p 'skg-content-view-mode)
    (user-error "This command needs an Skg view buffer"))
  (setq skg-link-annotations--repo-suffix-enabled
        (not skg-link-annotations--repo-suffix-enabled))
  (skg-link-annotations-mode 1)
  (skg-link-annotations-refresh))

(defun skg-link-annotations--clear ()
  "Remove only this feature's overlays from the current buffer."
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (when (overlay-get overlay 'skg-link-annotation)
      (delete-overlay overlay))))

(defun skg-link-annotations--scan ()
  "Return (label-start label-end link-end literal-ID) for this buffer.
Link syntax in text Org shows literally is an example, not a link."
  (let ((positions nil)
        (literal (skg-link-annotations--literal-ranges)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward skg-link-annotations--regexp nil t)
        (let ((start (match-beginning 0))
              (end (match-end 0)))
          ;; Only a literal region containing the whole link makes it an
          ;; example; =verbatim= in a link's label is just its formatting.
          (unless (cl-some (lambda (range)
                             (and (<= (car range) start) (<= end (cdr range))))
                           literal)
            (push (list (match-beginning 2) (match-end 2)
                        end (match-string-no-properties 1))
                  positions)))))
    (nreverse positions)))

(defconst skg-link-annotations--verbatim-pre "-('\"{"
  "Characters Org accepts just before an opening = or ~.")
(defconst skg-link-annotations--verbatim-post "-.,:!?;'\")}\\["
  "Characters Org accepts just after a closing = or ~.")

(defun skg-link-annotations--literal-ranges ()
  "Return (BEG . END) ranges of this buffer that Org shows literally.
Mirrors the server's 'org_literal_ranges' in
server/types/links/org_literal_ranges.rs: #+begin_X ... #+end_X
blocks, ``` fences, fixed-width lines, and inline =verbatim= and
~code~. A headline ends any open block, as the end of a node's body
does on the server. tests/shared/literal-link-cases.txt holds the cases
this, the server and the Neovim client must agree on."
  (let ((ranges nil)
        (open-start nil)
        (closing nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((bol (line-beginning-position))
               (eol (line-end-position))
               (line (buffer-substring-no-properties bol eol))
               (trimmed (downcase (string-trim-left line))))
          (when (and open-start (string-match-p "\\`\\*+ " line))
            (push (cons open-start bol) ranges)
            (setq open-start nil))
          (cond
           (open-start
            (when (string-prefix-p closing trimmed)
              (push (cons open-start eol) ranges)
              (setq open-start nil)))
           ((string-prefix-p "#+begin_" trimmed)
            (setq open-start bol
                  closing (concat "#+end_"
                                  (car (split-string
                                        (substring trimmed 8))))))
           ((string-prefix-p "```" trimmed)
            (setq open-start bol
                  closing "```"))
           ((or (string= trimmed ":") (string-prefix-p ": " trimmed))
            (push (cons bol eol) ranges))
           (t
            (dolist (span (skg-link-annotations--inline-verbatim-spans line))
              (push (cons (+ bol (car span)) (+ bol (cdr span)))
                    ranges)))))
        (forward-line 1))
      (when open-start
        (push (cons open-start (point-max)) ranges)))
    ranges))

(defun skg-link-annotations--inline-verbatim-spans (line)
  "Return (START . END) offsets of =verbatim= and ~code~ spans in LINE.
The opening marker follows the line start, whitespace or a
`skg-link-annotations--verbatim-pre' character, and precedes a
non-space; the closing marker follows a non-space and precedes the
line end, whitespace or a `skg-link-annotations--verbatim-post'
character."
  (let ((spans nil)
        (index 0)
        (length (length line)))
    (cl-flet ((space-p (char) (memq char '(?\s ?\t ?\r ?\f))))
      (while (< index length)
        (let* ((marker (aref line index))
               (opens
                (and (memq marker '(?= ?~))
                     (or (= index 0)
                         (let ((before (aref line (1- index))))
                           (or (space-p before)
                               (cl-find before skg-link-annotations--verbatim-pre))))
                     (< (1+ index) length)
                     (not (space-p (aref line (1+ index))))))
               (closing
                (and opens
                     (cl-loop for candidate from (+ index 2) below length
                              when (and (eq (aref line candidate) marker)
                                        (not (space-p (aref line (1- candidate))))
                                        (or (= (1+ candidate) length)
                                            (let ((after (aref line (1+ candidate))))
                                              (or (space-p after)
                                                  (cl-find after skg-link-annotations--verbatim-post)))))
                              return candidate))))
          (if closing
              (progn (push (cons index (1+ closing)) spans)
                     (setq index (1+ closing)))
            (setq index (1+ index))))))
    (nreverse spans)))

(defun skg-link-annotations--paint (positions)
  "Paint POSITIONS using the current status cache and suffix setting."
  (skg-link-annotations--clear)
  (dolist (position positions)
    (pcase-let* ((`(,label-start ,label-end ,link-end ,id) position)
                 (status (gethash id skg-link-annotations--cache))
                 (kind (car-safe status)))
      (when (eq kind 'missing)
        (let ((overlay (make-overlay label-start label-end nil nil t)))
          (overlay-put overlay 'skg-link-annotation t)
          (overlay-put overlay 'face
                       '(:inherit heralds-yucky-face :underline t))))
      ;; A dangling link's label is styled above and gets no suffix.
      (when (and skg-link-annotations--repo-suffix-enabled
                 (not (eq kind 'missing)))
        (let* ((label (pcase kind
                        ('resolved (format "⌂:%s" (nth 2 status)))
                        ('inactive "⌂:inactive")
                        ('lookup-failed "⌂:lookup failed")
                        (_ "⌂:…")))
               (face (if (eq kind 'resolved)
                         'heralds-normal-face 'heralds-yucky-face))
               (overlay (make-overlay link-end link-end nil nil t)))
          (overlay-put overlay 'skg-link-annotation t)
          (overlay-put overlay 'after-string
                       (concat " " (propertize (format "[%s]" label)
                                               'face face))))))))

(defun skg-link-annotations-refresh ()
  "Rescan this view and refresh annotations without changing its text."
  (when skg-link-annotations-mode
    (when (timerp skg-link-annotations--timer)
      (cancel-timer skg-link-annotations--timer))
    (setq skg-link-annotations--timer nil)
    (cl-incf skg-link-annotations--buffer-generation)
    (let* ((buffer-generation skg-link-annotations--buffer-generation)
           (tick (buffer-chars-modified-tick))
           (positions (skg-link-annotations--scan))
           (unknown (delete-dups
                     (cl-loop for position in positions
                              for skgid = (nth 3 position)
                              unless (gethash skgid skg-link-annotations--cache)
                              collect skgid))))
      (skg-link-annotations--paint positions)
      (when unknown
        (skg-link-annotations--request unknown buffer-generation tick)))))

(defun skg-link-annotations--request (skgids buffer-generation tick)
  "Request the status of IDS for this buffer's BUFFER-GENERATION and TICK."
  (let ((process (and (boundp 'skg-rust-tcp-proc) skg-rust-tcp-proc)))
    (if (not (and process (process-live-p process)))
        (progn
          (dolist (skgid skgids)
            (puthash skgid '(lookup-failed) skg-link-annotations--cache))
          (skg-link-annotations--paint (skg-link-annotations--scan)))
      (let* ((request-id (format "links-%s" (cl-incf skg-link-annotations--next-request)))
             (entry (list (current-buffer) buffer-generation tick
                          skg-link-annotations--epoch skgids))
             (request (concat (prin1-to-string
                               `((request . "link statuses")
                                 (request-id . ,request-id)
                                 (ids ,@skgids))) "\n")))
        (skg-register-response-handler
         'link-statuses #'skg-link-annotations--handle-response nil)
        (puthash request-id entry skg-link-annotations--requests)
        (cl-incf skg-lp--pending-count)
        (condition-case nil
            (process-send-string process request)
          (error
           (remhash request-id skg-link-annotations--requests)
           (setq skg-lp--pending-count (max 0 (1- skg-lp--pending-count)))
           (dolist (skgid skgids)
             (puthash skgid '(lookup-failed) skg-link-annotations--cache))
           (skg-link-annotations--paint (skg-link-annotations--scan))))))))

(defun skg-link-annotations--handle-response (_process payload)
  "Accept a response only for its live buffer, text, and graph epoch."
  (let* ((response (read payload))
         (request-id (cadr (assq 'request-id response)))
         (entry (gethash request-id skg-link-annotations--requests)))
    (when entry
      (remhash request-id skg-link-annotations--requests)
      (setq skg-lp--pending-count (max 0 (1- skg-lp--pending-count)))
      (pcase-let ((`(,buffer ,buffer-generation ,tick ,epoch ,ids) entry))
        (when (and (= epoch skg-link-annotations--epoch)
                   (buffer-live-p buffer))
          (with-current-buffer buffer
            (when (and skg-link-annotations-mode
                       (= buffer-generation skg-link-annotations--buffer-generation)
                       (= tick (buffer-chars-modified-tick)))
              (dolist (row (cadr (assq 'results response)))
                (let ((skgid (car row)))
                  (when (member skgid ids)
                    (puthash skgid
                             (pcase (cadr row)
                               ('resolved (list 'resolved (nth 2 row)
                                                (nth 3 row)))
                               ('inactive '(inactive))
                               ('missing '(missing))
                               (_ '(lookup-failed)))
                             skg-link-annotations--cache))))
              (skg-link-annotations--paint
               (skg-link-annotations--scan)))))))))

(defun skg-link-annotations-invalidate-all ()
  "Expire lookup results after a graph or repo-set change."
  (cl-incf skg-link-annotations--epoch)
  (clrhash skg-link-annotations--cache)
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when skg-link-annotations-mode
          (skg-link-annotations-refresh))))))

(defun skg-link-annotations-connection-reset ()
  "Drop requests and results tied to a closed connection."
  (clrhash skg-link-annotations--requests)
  (skg-link-annotations-invalidate-all))

(defun skg-link-annotations--after-change (&rest _ignored)
  "Debounce link scans after edits, including server view replacement."
  (when skg-link-annotations-mode
    (skg-link-annotations--clear)
    (when (timerp skg-link-annotations--timer)
      (cancel-timer skg-link-annotations--timer))
    (let ((buffer (current-buffer)))
      (setq skg-link-annotations--timer
            (run-with-idle-timer
             0.15 nil
             (lambda ()
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (skg-link-annotations-refresh)))))))))

;;;###autoload
(define-minor-mode skg-link-annotations-mode
  "Style confirmed broken Skg links and optionally show repo suffixes."
  :lighter ""
  (if skg-link-annotations-mode
      (progn
        (add-hook 'after-change-functions
                  #'skg-link-annotations--after-change nil t)
        (skg-link-annotations-refresh))
    (remove-hook 'after-change-functions
                 #'skg-link-annotations--after-change t)
    (when (timerp skg-link-annotations--timer)
      (cancel-timer skg-link-annotations--timer))
    (setq skg-link-annotations--timer nil)
    (skg-link-annotations--clear)))

(provide 'skg-link-annotations)
