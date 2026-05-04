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

(defvar-local skg-readable-ids--positions nil
  "List of (start end id-string) triples from the last scan.")

(defvar-local skg-readable-ids--generation 0
  "Counter incremented on each annotation request.
The response handler drops stale responses.")

(defun skg-readable-ids--clear-overlays ()
  "Remove all skg-magit-title overlays from the buffer."
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'skg-magit-title)
      (delete-overlay ov))))

(defun skg-readable-ids--collect-ids ()
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

(defun skg-readable-ids--annotate-buffer ()
  "Clear overlays, scan for IDs, shorten their display, and request titles from the server."
  (skg-readable-ids--clear-overlays)
  (let ((positions (skg-readable-ids--collect-ids)))
    (when positions
      (setq skg-readable-ids--positions positions)
      (skg-readable-ids--shorten-id-overlays positions)
      (let* ((generation
              (setq skg-readable-ids--generation
                    (1+ skg-readable-ids--generation)))
             (unique-ids
              (delete-dups
               (mapcar (lambda (p) (nth 2 p)) positions)))
             (buf (current-buffer)))
        (skg-readable-ids--request-titles
         unique-ids generation buf)))))

(defun skg-readable-ids--shorten-id-overlays (positions)
  "Put a display overlay over each id's tail (chars 9 onward) so
only the first 8 chars are shown, followed by an ellipsis. The
underlying buffer text is unchanged, so copy-paste still yields
the full id. POSITIONS is a list of (start end id-string) triples."
  (dolist (pos positions)
    (let* ((start (nth 0 pos))
           (end   (nth 1 pos))
           (tail-start (+ start 8)))
      (when (< tail-start end)
        (let ((ov (make-overlay tail-start end nil t nil)))
          (overlay-put ov 'skg-magit-title t)
          (overlay-put ov 'evaporate t)
          (overlay-put ov 'display
                       (propertize "…"
                                   'face 'skg-magit-title-face)))))))

(defun skg-readable-ids--request-titles (ids generation buf)
  "Send a titles-by-ids request for IDS.
GENERATION and BUF are captured for the response handler."
  (condition-case err
      (progn
        (skg-register-response-handler
         'titles-by-ids
         (lambda (_tcp-proc payload)
           (skg-readable-ids--handle-response
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
     (message "skg-readable-ids: server not connected: %s"
              (error-message-string err)))))

(defun skg-readable-ids--handle-response (payload generation buf)
  "Handle the titles-by-ids response.
PAYLOAD is the tagged LP response. GENERATION is checked
against the buffer's current generation to drop stale responses.
BUF is the magit buffer to annotate."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (= generation skg-readable-ids--generation)
        (let* ((response (read payload))
               (content  (cadr (assoc 'content response)))
               (title-map (make-hash-table :test 'equal)))
          (dolist (pair content)
            (when (consp pair)
              (puthash (format "%s" (car pair))
                       (format "%s" (cdr pair))
                       title-map)))
          (dolist (pos skg-readable-ids--positions)
            (let* ((start  (nth 0 pos))
                   (end    (nth 1 pos))
                   (id-str (nth 2 pos))
                   (title  (gethash id-str title-map)))
              (when title
                (let* ((display-title
                        (skg-readable-ids--format-title title))
                       (ov (make-overlay start end nil t nil)))
                  (overlay-put ov 'skg-magit-title t)
                  (overlay-put ov 'evaporate t)
                  (overlay-put ov 'after-string
                               (propertize
                                (concat " " display-title)
                                'face 'skg-magit-title-face)))))))))))

(defun skg-readable-ids--format-title (title)
  "Return the display form of TITLE for annotation.
Every '[[id:X][LABEL]]' sub-expression in TITLE is replaced
by '[[LABEL]]', so link-bearing titles render as links without
exposing the id. Plain titles pass through unchanged."
  (replace-regexp-in-string
   "\\[\\[id:[^]]+\\]\\[\\([^]]+\\)\\]\\]"
   "[[\\1]]"
   title))

;;;###autoload
(define-minor-mode skg-readable-ids-mode
  "Annotate skg IDs in magit buffers with their titles."
  :lighter " skg-titles"
  (if skg-readable-ids-mode
      (progn
        (setq-local truncate-lines nil)
        (skg-readable-ids--annotate-buffer)
        (add-hook 'magit-post-refresh-hook
                  #'skg-readable-ids--annotate-buffer nil t))
    (remove-hook 'magit-post-refresh-hook
                 #'skg-readable-ids--annotate-buffer t)
    (skg-readable-ids--clear-overlays)))

(provide 'skg-readable-ids)
