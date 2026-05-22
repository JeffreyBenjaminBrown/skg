;;; -*- lexical-binding: t; -*-
;;;
;;; DATA USED/ASSUMED: See /api.md.

(require 'skg-client)
(require 'skg-buffer)
(require 'skg-length-prefix)
(require 'heralds-minor-mode)

(defun skg-search (search-terms)
  "Text search with the conservative defaults: no regex, titles
only, no Tantivy operator syntax. Searches all nodes; rooty
ones (roots, cycle members, link targets, hadID) are bumped in
the ranking via their context-origin multiplier."
  (interactive "sSearch terms: ")
  (skg--request-text-search search-terms nil nil nil))

(defun skg-search-interactive (search-terms)
  "Text search with per-axis prompts for regex, body, operators."
  (interactive "sSearch terms: ")
  (let* ((regex     (y-or-n-p "Use per-token regex (not phrase search)? "))
         (body      (y-or-n-p "Include body text (titles are always searched)? "))
         (operators (y-or-n-p "Use Tantivy phrase and operator syntax (AND/OR/NOT/+/-)? ")))
    (skg--request-text-search search-terms
                              regex body operators)))

(defun skg--bool-to-string (b)
  "Serialize B as the wire-format \"true\" or \"false\"."
  (if b "true" "false"))

(defun skg--request-text-search (search-terms regex body operators)
  "Request a text search from the Rust server.
REGEX, BODY, OPERATORS are booleans; sent as \"true\"/\"false\"."
  (let* ((tcp-proc (skg-tcp-connect-to-rust))
         (clean-terms (if (stringp search-terms)
                          (substring-no-properties search-terms)
                        search-terms))
         (request-s-exp
          (concat (prin1-to-string
                   `((request   . "text search")
                     (terms     . ,clean-terms)
                     (regex     . ,(skg--bool-to-string regex))
                     (body      . ,(skg--bool-to-string body))
                     (operators . ,(skg--bool-to-string operators))))
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

(defvar skg--search-buffer-setup-hook nil
  "Hook run inside a freshly populated search buffer.
Each function is called with no arguments, with the search buffer
as `current-buffer'. Used by `skg-search-make-link' to upgrade the
buffer to link-creation mode.")

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
          (skg-content-view-mode)
          (heralds-minor-mode)
          (goto-char (point-min)))
        (setq skg-view-uri view-uri)
        (add-hook 'kill-buffer-hook #'skg-send-close-view nil t)
        (run-hooks 'skg--search-buffer-setup-hook)
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

(provide 'skg-request-text-search)
