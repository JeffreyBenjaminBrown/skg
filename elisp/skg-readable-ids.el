;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Minor mode that annotates skg UUIDs in magit and .skg
;;; file buffers with their titles, using after-string overlays.
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

(defvar skg-readable-ids--pending-title-requests nil
  "FIFO queue of pending titles-by-ids requests.
Each entry is (GENERATION BUFFER REQUESTED-IDS). The server
responds on the same TCP connection in request order, so a
response is matched to the oldest pending request.")

(defvar skg-readable-ids--title-cache (make-hash-table :test 'equal)
  "Session-global map from node ID to its title, or to `:missing'
for an ID the server did not resolve (unknown to it, or inactive
under the current source-set). Retained across magit refreshes so
that a staging event re-requests nothing it has already seen; a
refresh whose IDs are all cached sends no request at all. Can go
stale -- a node retitled, the source-set widened -- in which case
`skg-readable-ids-refresh-titles' rebuilds it.")

(defun skg-readable-ids-refresh-titles ()
  "Empty the ID-to-title cache and re-annotate the current buffer.
Use this after anything that changes what titles the server would
answer with: a node retitled, or the active source-set widened
(IDs inactive under the old set were cached as missing)."
  (interactive)
  (clrhash skg-readable-ids--title-cache)
  (skg-readable-ids--annotate-buffer))

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
  "Clear overlays, scan for IDs, shorten their display, annotate
from the session title cache, and request only the UNCACHED titles
from the server. When every ID is cached (the common case after
the first scan -- e.g. on each magit refresh a staging event
triggers), no request is sent."
  (skg-readable-ids--clear-overlays)
  (let ((positions (skg-readable-ids--collect-ids)))
    (when positions
      (setq skg-readable-ids--positions positions)
      (skg-readable-ids--shorten-id-overlays positions)
      (skg-readable-ids--place-title-annotations)
      (let ((uncached-ids
             (seq-remove
              (lambda (id)
                (gethash id skg-readable-ids--title-cache))
              (delete-dups
               (mapcar (lambda (p) (nth 2 p)) positions)))))
        (when uncached-ids
          (let ((generation
                 (setq skg-readable-ids--generation
                       (1+ skg-readable-ids--generation))))
            (skg-readable-ids--request-titles
             uncached-ids generation (current-buffer))))))))

(defun skg-readable-ids--place-title-annotations ()
  "(Re)place after-string title annotations over the scanned ID
positions, from `skg-readable-ids--title-cache'. Existing
annotation overlays are removed first, so calling this again after
a cache update never doubles an annotation."
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'skg-magit-title-annotation)
      (delete-overlay ov)))
  (dolist (pos skg-readable-ids--positions)
    (let* ((start  (nth 0 pos))
           (end    (nth 1 pos))
           (id-str (nth 2 pos))
           (title  (gethash id-str skg-readable-ids--title-cache)))
      (when (and title (not (eq title :missing)))
        (let* ((display-title
                (skg-readable-ids--format-title title))
               (ov (make-overlay start end nil t nil)))
          (overlay-put ov 'skg-magit-title t)
          (overlay-put ov 'skg-magit-title-annotation t)
          (overlay-put ov 'evaporate t)
          (overlay-put ov 'after-string
                       (propertize
                        (concat " " display-title)
                        'face 'skg-magit-title-face)))))))

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
      (let ((entry (list generation buf ids)))
        (skg-readable-ids--ensure-title-response-handler)
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
          (setq skg-readable-ids--pending-title-requests
                (append skg-readable-ids--pending-title-requests
                        (list entry)))
          (setq skg-lp--pending-count
                (1+ skg-lp--pending-count))
          (process-send-string tcp-proc request-sexp)))
    (error
     (message "skg-readable-ids: server not connected: %s"
              (error-message-string err)))))

(defun skg-readable-ids--ensure-title-response-handler ()
  "Install the shared titles-by-ids response handler."
  (skg-register-response-handler
   'titles-by-ids
   (lambda (_tcp-proc payload)
     (skg-readable-ids--handle-next-response payload))
   nil))

(defun skg-readable-ids--handle-next-response (payload)
  "Handle PAYLOAD for the oldest pending titles-by-ids request."
  (let ((entry (pop skg-readable-ids--pending-title-requests)))
    (if (not entry)
        (message "skg-readable-ids: titles-by-ids response without pending request")
      (setq skg-lp--pending-count
            (max 0 (1- skg-lp--pending-count)))
      (skg-readable-ids--handle-response
       payload
       (nth 0 entry)
       (nth 1 entry)
       (nth 2 entry)))))

(defun skg-readable-ids--handle-response (payload generation buf requested-ids)
  "Handle the titles-by-ids response.
PAYLOAD is the tagged LP response; its titles merge into
`skg-readable-ids--title-cache' unconditionally (they are true
regardless of buffer state), and each of REQUESTED-IDS the server
did not answer for is cached as `:missing' so it is not
re-requested. GENERATION is checked against the buffer's current
generation before annotating, to drop stale responses. BUF is the
magit buffer to annotate."
  (let* ((response (read payload))
         (content  (cadr (assoc 'content response))))
    (dolist (pair content)
      (when (consp pair)
        (puthash (format "%s" (car pair))
                 (format "%s" (cdr pair))
                 skg-readable-ids--title-cache)))
    (dolist (id requested-ids)
      (unless (gethash id skg-readable-ids--title-cache)
        (puthash id :missing skg-readable-ids--title-cache))))
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (= generation skg-readable-ids--generation)
        (skg-readable-ids--place-title-annotations)))))

(defun skg-readable-ids--format-title (title)
  "Return the display form of TITLE for annotation.
Every '[[id:X][LABEL]]' sub-expression in TITLE is replaced
by '[[LABEL]]', so link-bearing titles render as links without
exposing the id. Plain titles pass through unchanged."
  (replace-regexp-in-string
   "\\[\\[id:[^]]+\\]\\[\\([^]]+\\)\\]\\]"
   "[[\\1]]"
   title))

(defun skg-readable-ids--skg-file-buffer-p (buf)
  "Return non-nil if BUF visits a .skg file."
  (and (buffer-live-p buf)
       (buffer-local-value 'buffer-file-name buf)
       (string-match-p "\\.skg\\'"
                       (buffer-local-value 'buffer-file-name buf))))

(defun skg-readable-ids--maybe-enable ()
  "Enable `skg-readable-ids-mode' if the current buffer visits a .skg file."
  (when (skg-readable-ids--skg-file-buffer-p (current-buffer))
    (skg-readable-ids-mode 1)))

(add-hook 'find-file-hook #'skg-readable-ids--maybe-enable)

;;;###autoload
(define-minor-mode skg-readable-ids-mode
  "Annotate skg IDs in buffers with their titles."
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
