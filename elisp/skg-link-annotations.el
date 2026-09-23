;;; -*- lexical-binding: t; -*-
;;; Display-only status and optional home-source suffixes for Skg links.

(require 'cl-lib)
(require 'subr-x)
(require 'heralds-minor-mode)
(require 'skg-state)

(defconst skg-link-annotations--regexp
  "\\[\\[id:\\([^]\n]+\\)\\]\\[\\([^]\n]*\\)\\]\\]"
  "The literal Skg link syntax used by the server graph parser.")

(defvar skg-link-annotations--cache (make-hash-table :test 'equal)
  "Status by literal ID for the current graph and source-set lifetime.")
(defvar skg-link-annotations--requests (make-hash-table :test 'equal)
  "Outstanding request ID to (buffer generation tick epoch ids).")
(defvar skg-link-annotations--next-request 0)
(defvar skg-link-annotations--epoch 0)

(defvar-local skg-link-annotations--source-suffix-enabled nil
  "Whether this buffer displays source suffixes on Skg links.")
(put 'skg-link-annotations--source-suffix-enabled 'permanent-local t)
(defvar-local skg-link-annotations--generation 0)
(defvar-local skg-link-annotations--timer nil)

(defun skg-toggle-source-overlay-on-links ()
  "Toggle display-only source suffixes for Skg links in this view."
  (interactive)
  (unless (derived-mode-p 'skg-content-view-mode)
    (user-error "This command needs an Skg view buffer"))
  (setq skg-link-annotations--source-suffix-enabled
        (not skg-link-annotations--source-suffix-enabled))
  (skg-link-annotations-mode 1)
  (skg-link-annotations-refresh))

(defun skg-link-annotations--clear ()
  "Remove only this feature's overlays from the current buffer."
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (when (overlay-get overlay 'skg-link-annotation)
      (delete-overlay overlay))))

(defun skg-link-annotations--scan ()
  "Return (label-start label-end link-end literal-ID) for this buffer."
  (let ((positions nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward skg-link-annotations--regexp nil t)
        (push (list (match-beginning 2) (match-end 2)
                    (match-end 0) (match-string-no-properties 1))
              positions)))
    (nreverse positions)))

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
                       '(:inherit heralds-confusable-face :underline t))))
      (when skg-link-annotations--source-suffix-enabled
        (let* ((label (pcase kind
                        ('resolved (format "⌂:%s" (nth 2 status)))
                        ('missing "⌂:missing")
                        ('inactive "⌂:inactive")
                        ('error "⌂:unavailable")
                        (_ "⌂:…")))
               (face (if (eq kind 'resolved)
                         'heralds-green-face 'shadow))
               (overlay (make-overlay link-end link-end nil nil t)))
          (overlay-put overlay 'skg-link-annotation t)
          (overlay-put overlay 'after-string
                       (propertize (format " [%s]" label) 'face face)))))))

(defun skg-link-annotations-refresh ()
  "Rescan this view and refresh annotations without changing its text."
  (when skg-link-annotations-mode
    (when (timerp skg-link-annotations--timer)
      (cancel-timer skg-link-annotations--timer))
    (setq skg-link-annotations--timer nil)
    (cl-incf skg-link-annotations--generation)
    (let* ((generation skg-link-annotations--generation)
           (tick (buffer-chars-modified-tick))
           (positions (skg-link-annotations--scan))
           (unknown (delete-dups
                     (cl-loop for position in positions
                              for id = (nth 3 position)
                              unless (gethash id skg-link-annotations--cache)
                              collect id))))
      (skg-link-annotations--paint positions)
      (when unknown
        (skg-link-annotations--request unknown generation tick)))))

(defun skg-link-annotations--request (ids generation tick)
  "Request the status of IDS for this buffer's GENERATION and TICK."
  (let ((process (and (boundp 'skg-rust-tcp-proc) skg-rust-tcp-proc)))
    (if (not (and process (process-live-p process)))
        (progn
          (dolist (id ids)
            (puthash id '(error) skg-link-annotations--cache))
          (skg-link-annotations--paint (skg-link-annotations--scan)))
      (let* ((request-id (format "links-%s" (cl-incf skg-link-annotations--next-request)))
             (entry (list (current-buffer) generation tick
                          skg-link-annotations--epoch ids))
             (request (concat (prin1-to-string
                               `((request . "link statuses")
                                 (request-id . ,request-id)
                                 (ids ,@ids))) "\n")))
        (skg-register-response-handler
         'link-statuses #'skg-link-annotations--handle-response nil)
        (puthash request-id entry skg-link-annotations--requests)
        (cl-incf skg-lp--pending-count)
        (condition-case nil
            (process-send-string process request)
          (error
           (remhash request-id skg-link-annotations--requests)
           (setq skg-lp--pending-count (max 0 (1- skg-lp--pending-count)))
           (dolist (id ids)
             (puthash id '(error) skg-link-annotations--cache))
           (skg-link-annotations--paint (skg-link-annotations--scan))))))))

(defun skg-link-annotations--handle-response (_process payload)
  "Accept a response only for its live buffer, text, and graph epoch."
  (let* ((response (read payload))
         (request-id (cadr (assq 'request-id response)))
         (entry (gethash request-id skg-link-annotations--requests)))
    (when entry
      (remhash request-id skg-link-annotations--requests)
      (setq skg-lp--pending-count (max 0 (1- skg-lp--pending-count)))
      (pcase-let ((`(,buffer ,generation ,tick ,epoch ,ids) entry))
        (when (and (= epoch skg-link-annotations--epoch)
                   (buffer-live-p buffer))
          (with-current-buffer buffer
            (when (and skg-link-annotations-mode
                       (= generation skg-link-annotations--generation)
                       (= tick (buffer-chars-modified-tick)))
              (dolist (row (cadr (assq 'results response)))
                (let ((id (car row)))
                  (when (member id ids)
                    (puthash id
                             (pcase (cadr row)
                               ('resolved (list 'resolved (nth 2 row)
                                                (nth 3 row)))
                               ('inactive '(inactive))
                               ('missing '(missing))
                               (_ '(error)))
                             skg-link-annotations--cache))))
              (skg-link-annotations--paint
               (skg-link-annotations--scan)))))))))

(defun skg-link-annotations-invalidate-all ()
  "Expire lookup results after a graph or source-set change."
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
  "Style confirmed broken Skg links and optionally show source suffixes."
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
