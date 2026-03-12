;;; -*- lexical-binding: t; -*-
;;;
;;; USER-FACING FUNCTIONS
;;;   skg-search-titles
;;;   skg-search-titles-everywhere
;;;
;;; DATA USED/ASSUMED: See /api.md.

(require 'skg-client)
(require 'skg-buffer)
(require 'skg-length-prefix)
(require 'heralds-minor-mode)

(defun skg-search-titles (search-terms)
  "Search titles, showing only origin nodes (roots, targets, etc.)."
  (interactive "sSearch terms: ")
  (skg--request-title-matches search-terms "rooty"))

(defun skg-search-titles-everywhere (search-terms)
  "Search titles, showing all matches including non-origin nodes."
  (interactive "sSearch terms: ")
  (skg--request-title-matches search-terms "everywhere"))

(defun skg--request-title-matches (search-terms scope)
  "Request title matches from the Rust server.
SCOPE is \"rooty\" (filtered) or \"everywhere\" (unfiltered)."
  (let* ((tcp-proc (skg-tcp-connect-to-rust))
         (clean-terms (if (stringp search-terms)
                          (substring-no-properties search-terms)
                        search-terms))
         (request-s-exp
          (concat (prin1-to-string
                   `((request . "title matches")
                     (terms . ,clean-terms)
                     (scope . ,scope)))
                  "\n")))
    (skg-register-response-handler
     ;; Register phase 1 handler (one-shot)
     'search-results
     (lambda (_tcp-proc payload)
       (skg--display-search-phase1 payload clean-terms))
     t)
    (skg-register-response-handler
     ;; Register phase 2 handler for search results 'enriched' with containerward paths and graphnodestats. Persists until fired or replaced.
     'search-enrichment
     (lambda (_tcp-proc payload)
       (skg--display-search-enrichment payload))
     t)
    (skg-register-response-handler
     ;; Rust asks for a snapshot of the search buffer so it can
     ;; integrate ancestry without losing user edits.
     'request-snapshot
     (lambda (tcp-proc payload)
       (skg--handle-snapshot-request tcp-proc payload))
     nil) ;; persistent, not one-shot
    (skg-lp-reset)
    (process-send-string tcp-proc request-s-exp)))

(defun skg--display-search-phase1 (payload search-terms)
  "Display phase 1 search results (without paths).
Sets skg-view-uri to \"search:TERMS\" and registers a
kill-buffer-hook to send close-view to the server."
  (let* ((response (read payload))
         (content (skg--as-string (cadr (assoc 'content response))))
         (view-uri (concat "search:" search-terms)))
    (when content
      (with-current-buffer
          (get-buffer-create (skg-search-buffer-name search-terms))
        (let ((inhibit-read-only t))
          (skg--replace-search-content content)
          (org-mode)
          (heralds-minor-mode)
          (goto-char (point-min)))
        (setq skg-view-uri view-uri)
        (skg-content-view-mode 1)
        (add-hook 'kill-buffer-hook #'skg-send-close-view nil t)
        (switch-to-buffer (current-buffer)) ))))

(defun skg--as-string (value)
  "Convert VALUE to a string. Symbols become their name."
  (cond ((stringp value) value)
        ((symbolp value) (symbol-name value))
        ((null value) nil)
        (t (format "%s" value))))

(defun skg--display-search-enrichment (payload)
  "Replace search buffer with results
'enriched' with containerward paths and graphnodestats.
PAYLOAD contains response-type, terms, and content.
Exits readonly after replacing content."
  (let* ((response (read payload))
         (terms   (skg--as-string (cadr (assoc 'terms   response))))
         (content (skg--as-string (cadr (assoc 'content response)))))
    (when (and terms content)
      (let ((buf (get-buffer (skg-search-buffer-name terms))))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (let ((old-point (point)))
              (skg--replace-search-content content)
              (goto-char (min old-point (point-max))))
            (setq buffer-read-only nil)
            (message "Search results enriched.") )) ))))

(defun skg--handle-snapshot-request (tcp-proc payload)
  "Handle a request from the server for a snapshot of a search buffer.
- Server sends the search terms.
- Client looks up the corresponding search buffer by name.
- Client makes that buffer readonly.
- Client sends its text back to the server for 'enrichment'."
  (let* ((response (read payload))
         (terms (skg--as-string (cadr (assoc 'content response))))
         (buf (when terms
                (get-buffer (skg-search-buffer-name terms)))))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (setq buffer-read-only t)
        (message "Enriching search results...")
        (let* ((buffer-contents (buffer-string))
               (request-s-exp
                (concat (prin1-to-string
                         `((request . "snapshot response")
                           (terms . ,terms)))
                        "\n"))
               (content-bytes
                (encode-coding-string buffer-contents 'utf-8))
               (content-length (length content-bytes))
               (header (format "Content-Length: %d\r\n\r\n"
                               content-length)))
          (process-send-string tcp-proc request-s-exp)
          (process-send-string tcp-proc header)
          (process-send-string tcp-proc buffer-contents))))))

(defun skg--replace-search-content (content)
  "Replace current buffer text with CONTENT, trimmed, with trailing newline.
Clears modified flag.
Callers that do additional buffer work (e.g. org-mode setup)
should bind inhibit-read-only themselves, since the buffer
may already be read-only from a previous search."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (string-trim content))
    (when (> (length content) 0)
      (insert "\n")))
  (set-buffer-modified-p nil))

(provide 'skg-request-title-matches)
