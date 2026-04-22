;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Minor mode that annotates skg UUIDs in magit buffers
;;; with their titles, using after-string overlays.
;;; It affects both bare IDs and IDs that are part of .skg filenames.
;;;
;;; PITFALL: The after-string overlays break home/end navigation
;;; when lines are truncated. See PITFALLs.org for details and mitigations.

(require 'skg-id-search)
(require 'skg-state)
(require 'skg-length-prefix)

(defface skg-magit-title-face
  '((t :foreground "cornflower blue"))
  "Face for skg title annotations in magit buffers.")

(defvar-local skg-magit-titles--positions nil
  "List of (start end id-string) triples from the last scan.")

(defvar-local skg-magit-titles--generation 0
  "Counter incremented on each annotation request.
The response handler drops stale responses.")

(defun skg-magit-titles--clear-overlays ()
  "Remove all skg-magit-title overlays from the buffer."
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'skg-magit-title)
      (delete-overlay ov))))

(defun skg-magit-titles--collect-ids ()
  "Scan the buffer for UUID v4 patterns.
Returns a list of (start end id-string) triples."
  (let ((positions nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward skg--uuid-v4-regex nil t)
        (push (list (match-beginning 0)
                    (match-end 0)
                    (match-string-no-properties 0))
              positions)))
    (nreverse positions)))

(defun skg-magit-titles--annotate-buffer ()
  "Clear overlays, scan for IDs, and request titles from the server."
  (skg-magit-titles--clear-overlays)
  (let ((positions (skg-magit-titles--collect-ids)))
    (when positions
      (setq skg-magit-titles--positions positions)
      (let* ((generation
              (setq skg-magit-titles--generation
                    (1+ skg-magit-titles--generation)))
             (unique-ids
              (delete-dups
               (mapcar (lambda (p) (nth 2 p)) positions)))
             (buf (current-buffer)))
        (skg-magit-titles--request-titles
         unique-ids generation buf)))))

(defun skg-magit-titles--request-titles (ids generation buf)
  "Send a titles-by-ids request for IDS.
GENERATION and BUF are captured for the response handler."
  (condition-case err
      (progn
        (skg-register-response-handler
         'titles-by-ids
         (lambda (_tcp-proc payload)
           (skg-magit-titles--handle-response
            payload generation buf))
         t)
        (skg-lp-reset)
        (let* ((tcp-proc (skg-tcp-connect-to-rust))
               (id-list
                (mapconcat
                 (lambda (id) (format "%S" id))
                 ids " "))
               (request-sexp
                (concat
                 (format "((request . \"titles by ids\") (ids %s))"
                         id-list)
                 "\n")))
          (process-send-string tcp-proc request-sexp)))
    (error
     (message "skg-magit-titles: server not connected: %s"
              (error-message-string err)))))

(defun skg-magit-titles--handle-response (payload generation buf)
  "Handle the titles-by-ids response.
PAYLOAD is the tagged LP response. GENERATION is checked
against the buffer's current generation to drop stale responses.
BUF is the magit buffer to annotate."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (= generation skg-magit-titles--generation)
        (let* ((response (read payload))
               (content  (cadr (assoc 'content response)))
               (title-map (make-hash-table :test 'equal)))
          (dolist (pair content)
            (when (consp pair)
              (puthash (format "%s" (car pair))
                       (format "%s" (cdr pair))
                       title-map)))
          (dolist (pos skg-magit-titles--positions)
            (let* ((start  (nth 0 pos))
                   (end    (nth 1 pos))
                   (id-str (nth 2 pos))
                   (title  (gethash id-str title-map)))
              (when title
                (let* ((ov (make-overlay start end nil t nil)))
                  (overlay-put ov 'skg-magit-title t)
                  (overlay-put ov 'evaporate t)
                  (overlay-put ov 'after-string
                               (propertize
                                (concat " " title)
                                'face 'skg-magit-title-face)))))))))))

;;;###autoload
(define-minor-mode skg-magit-titles-mode
  "Annotate skg IDs in magit buffers with their titles."
  :lighter " skg-titles"
  (if skg-magit-titles-mode
      (progn
        (setq-local truncate-lines nil)
        (skg-magit-titles--annotate-buffer)
        (add-hook 'magit-post-refresh-hook
                  #'skg-magit-titles--annotate-buffer nil t))
    (remove-hook 'magit-post-refresh-hook
                 #'skg-magit-titles--annotate-buffer t)
    (skg-magit-titles--clear-overlays)))

(provide 'skg-magit-titles)
