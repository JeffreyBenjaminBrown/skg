;;; -*- lexical-binding: t; -*-
;;;
;;; USER-FACING FUNCTIONS
;;;   skg-request-title-matches
;;;
;;; DATA USED/ASSUMED: See /api.md.

(require 'skg-client)
(require 'skg-buffer)
(require 'skg-length-prefix)
(require 'heralds-minor-mode)

(defun skg-request-title-matches (search-terms)
  "Request title matches from the Rust server."
  (interactive "sSearch terms: ")
  (let* ((tcp-proc (skg-tcp-connect-to-rust))
         (clean-terms (if (stringp search-terms)
                          (substring-no-properties search-terms)
                        search-terms))
         (request-s-exp
          (concat (prin1-to-string
                   `((request . "title matches")
                     (terms . ,clean-terms)))
                  "\n")))
    ;; Register phase 1 handler (one-shot)
    (skg-register-response-handler
     'search-results
     (lambda (_tcp-proc payload)
       (skg--display-search-phase1 payload clean-terms))
     t)
    ;; Register phase 2 handler for search results enriched with paths. Persists until fired or replaced.
    (skg-register-response-handler
     'search-enrichment
     (lambda (_tcp-proc payload)
       (skg--display-search-enrichment payload))
     t)
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
        (add-hook 'kill-buffer-hook #'skg-send-close-view nil t)
        (setq buffer-read-only t)
        (switch-to-buffer (current-buffer)) ))))

(defun skg--as-string (value)
  "Convert VALUE to a string. Symbols become their name."
  (cond ((stringp value) value)
        ((symbolp value) (symbol-name value))
        ((null value) nil)
        (t (format "%s" value))))

(defun skg--display-search-enrichment (payload)
  "Replace search buffer with results
'enriched' with containerward paths.
PAYLOAD contains response-type, terms, and content."
  (let* ((response (read payload))
         (terms   (skg--as-string (cadr (assoc 'terms   response))))
         (content (skg--as-string (cadr (assoc 'content response)))))
    (when (and terms content)
      (let ((buf (get-buffer (skg-search-buffer-name terms))))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (let ((old-point (point)))
              (skg--replace-search-content content)
              (goto-char (min old-point (point-max)))) )) ))))

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
