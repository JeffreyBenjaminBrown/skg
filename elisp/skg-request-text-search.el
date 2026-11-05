;;; -*- lexical-binding: t; -*-
;;;
;;; DATA USED/ASSUMED: See docs/api-and-formats.org.

(require 'skg-client)
(require 'skg-buffer)
(require 'skg-length-prefix)
(require 'skg-request-save) ; Shared warning presentation.
(require 'heralds-minor-mode)

(defconst skg--project-root
  (when (or load-file-name buffer-file-name)
    (file-name-directory
     (directory-file-name
      (file-name-directory
       (or load-file-name buffer-file-name)))))
  "Repo root, derived from this file's location at load time.
Used to locate docs/COMMANDS.org for `skg-search-interactive' help.")

(defun skg-search (search-terms)
  "Text search with the conservative defaults: no regex, titles
only, no Tantivy operator syntax. Searches all nodes; prominent
ones (roots, cycle members, link targets, hadID) are bumped in
the ranking via their prominence multiplier."
  (interactive "sSearch terms: ")
  (skg--request-text-search search-terms nil nil nil))

(defun skg-search-interactive (search-terms options)
  "Text search, choosing axes by typing option characters.
After the terms, type one character per desired option:
\"r\" for per-token regex, \"b\" to include body text, \"t\" for
Tantivy phrase and operator syntax. Type \"h\" to open the docs
for this command (which abandons the search); unrecognized
characters are ignored. An empty answer uses the same
conservative defaults as `skg-search'."
  (interactive
   (list (read-string "Search terms: ")
         (read-string
          "Type a character for each search option: (r)egex, (b)ody, (t)antivy syntax, (h)elp: ")))
  (if (string-search "h" options)
      (skg--search-interactive-help)
    (skg--request-text-search
     search-terms
     (and (string-search "r" options) t)
     (and (string-search "b" options) t)
     (and (string-search "t" options) t))))

(defun skg--search-interactive-help ()
  "Open docs/COMMANDS.org, unfold it, and put point on the
headline documenting `skg-search-interactive'."
  (unless skg--project-root
    (user-error "Cannot locate the repo root to find docs/COMMANDS.org"))
  (let ((doc (expand-file-name "docs/COMMANDS.org" skg--project-root)))
    (unless (file-exists-p doc)
      (user-error "Help file not found: %s" doc))
    (find-file doc)
    (org-fold-show-all)
    (goto-char (point-min))
    (if (re-search-forward "^\\*+.*skg-search-interactive" nil t)
        (goto-char (match-beginning 0))
      (message
       "Couldn't find the skg-search-interactive headline; showing the whole file."))))

(defun skg--bool-to-string (b)
  "Serialize B as the wire-format \"true\" or \"false\"."
  (if b "true" "false"))

;; A search is answered by one 'search-results' message, whose content
;; is the search buffer: the hits, each with what the enrichment draws
;; under it (docs/api-and-formats.org, "Text search"). The search
;; buffer is an ordinary view from that message on.

(defun skg--request-text-search (search-terms regex body operators
                                              &optional overPrivateText-choice)
  "Request a text search from the Rust server.
REGEX, BODY, OPERATORS are booleans; sent as \"true\"/\"false\"."
  (condition-case err
      (skg--send-text-search-request
       search-terms regex body operators overPrivateText-choice)
    (error
     (skg--cancel-locally-failed-search)
     (signal (car err) (cdr err)))))

(defun skg--send-text-search-request (search-terms regex body operators
                                                   overPrivateText-choice)
  "Register handlers and send one text-search request."
  (let* ((tcp-proc (skg-tcp-connect-to-rust))
         (clean-terms (if (stringp search-terms)
                          (substring-no-properties search-terms)
                        search-terms))
         (request-s-exp
          (concat (prin1-to-string
                   (append
                    `((request   . "text search")
                      (terms     . ,clean-terms)
                      (regex     . ,(skg--bool-to-string regex))
                      (body      . ,(skg--bool-to-string body))
                      (operators . ,(skg--bool-to-string operators)))
                    (when overPrivateText-choice
                      `((overPrivateText-telescopes . ,overPrivateText-choice)))))
                  "\n")))
    (skg-register-response-handler
     'search-results
     (lambda (_tcp-proc payload)
       (setq skg-response-handler-map
             (assoc-delete-all 'overPrivateText-telescope-confirmation
                               skg-response-handler-map))
       (skg--display-search-results payload clean-terms))
     t)
    (skg-register-response-handler
     'overPrivateText-telescope-confirmation
     (lambda (_tcp-proc payload)
       (setq skg-response-handler-map
             (assoc-delete-all 'overPrivateText-telescope-confirmation
                               skg-response-handler-map))
       (skg--forget-search-results-handler)
       (let* ((response (read payload))
              (prompt (format "%s" (cadr (assoc 'prompt response))))
              (choice (if (y-or-n-p
                           (concat prompt " (No means exclude.) "))
                          "include"
                        "exclude")))
         (skg--request-text-search
          clean-terms regex body operators choice)))
     nil)
    (skg-lp-reset)
    (process-send-string tcp-proc request-s-exp)))

(defun skg--forget-search-results-handler ()
  "Drop the one-shot `search-results' handler, if it is still waiting,
and the response it was counted as."
  (when (assoc 'search-results skg-response-handler-map)
    (setq skg-response-handler-map
          (assoc-delete-all 'search-results skg-response-handler-map))
    (setq skg-lp--pending-count
          (max 0 (1- skg-lp--pending-count)))))

(defun skg--cancel-locally-failed-search ()
  "Unwind the search handlers after a local send failure."
  (skg--forget-search-results-handler)
  (setq skg-response-handler-map
        (assoc-delete-all 'overPrivateText-telescope-confirmation
                          skg-response-handler-map)))

(defvar skg--search-buffer-setup-hook nil
  "Hook run inside a freshly populated search buffer.
Each function is called with no arguments, with the search buffer
as `current-buffer'. Used by `skg-search-make-link' to upgrade the
buffer to link-creation mode.")

(defun skg--display-search-results (payload search-terms)
  "Display the search buffer.
Sets skg-view-id to \"search:TERMS\" and registers a
kill-buffer-hook to send close-view to the server."
  (let* ((response (read payload))
         (content (skg--as-string (cadr (assoc 'content response))))
         (warnings (cadr (assoc 'warnings response)))
         (view-id (concat "search:" search-terms)))
    (when content
      (with-current-buffer
          (get-buffer-create (skg-search-buffer-name search-terms))
        (let ((inhibit-read-only t))
          (skg--replace-search-content content)
          (skg-content-view-mode)
          (heralds-minor-mode)
          (goto-char (point-min)))
        (setq skg-view-id view-id)
        (skg--capture-clean-baseline)
        (add-hook 'kill-buffer-hook #'skg-send-close-view nil t)
        (run-hooks 'skg--search-buffer-setup-hook)
        (switch-to-buffer (current-buffer)) ))
    (when warnings
      (skg-big-nonfatal-message
       "*SKG Search Warnings*"
       "Search completed with warnings"
       (skg-errors-and-warnings-to-org-string nil warnings)))))

(defun skg--as-string (value)
  "Convert VALUE to a string. Symbols become their name."
  (cond ((stringp value) value)
        ((symbolp value) (symbol-name value))
        ((null value) nil)
        (t (format "%s" value))))

(defun skg--replace-search-content (content)
  "Replace current buffer text with CONTENT, trimmed, with trailing
newline, and leave the buffer unmodified."
  (let ((inhibit-read-only t)
        (skg--inhibit-dirty-view-confirmation t))
    (erase-buffer)
    (insert (string-trim content))
    (when (> (length content) 0)
      (insert "\n")))
  (set-buffer-modified-p nil))

(provide 'skg-request-text-search)
