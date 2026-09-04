;;; -*- lexical-binding: t; -*-
;;;
;;; DATA USED/ASSUMED: See /api.md.

(require 'skg-client)
(require 'skg-buffer)
(require 'skg-buffer-registry)
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
only, no Tantivy operator syntax. Searches all nodes; rooty
ones (roots, cyclic roots, link targets, hadID) are bumped in
the ranking via their context-origin multiplier."
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

(defun skg--request-text-search (search-terms regex body operators
                                              &optional ugly-choice)
  "Request a text search from the Rust server.
REGEX, BODY, OPERATORS are booleans; sent as \"true\"/\"false\"."
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
                    (when ugly-choice
                      `((ugly-telescopes . ,ugly-choice)))))
                  "\n")))
    (skg-register-response-handler
     ;; Register phase 1 handler (one-shot)
     'search-results
     (lambda (_tcp-proc payload)
       (skg-remove-response-handler 'ugly-telescope-confirmation)
       (skg--display-search-phase1
        payload clean-terms regex body operators ugly-choice))
     t)
    (skg-register-response-handler
     ;; Register phase 2 handler for search results 'enriched' with containerward paths and graphnodestats. Persists until fired or replaced.
    'search-enrichment
     (lambda (tcp-proc payload)
       (skg--display-search-enrichment tcp-proc payload))
     t)
    (skg-register-response-handler
     ;; Rust asks for a snapshot of the search buffer so it can
     ;; integrate ancestry without losing user edits.
     'request-snapshot
     (lambda (tcp-proc payload)
       (skg--handle-snapshot-request tcp-proc payload))
     nil) ;; persistent, not one-shot
    (skg-register-response-handler
     'ugly-telescope-confirmation
     (lambda (_tcp-proc payload)
       (skg-remove-response-handler 'ugly-telescope-confirmation)
       (dolist (response-type '(search-results search-enrichment))
         (skg-remove-response-handler response-type))
       (skg-remove-response-handler 'request-snapshot)
       (let* ((response (read payload))
              (prompt (format "%s" (cadr (assoc 'prompt response))))
              (choice (if (y-or-n-p
                           (concat prompt " (No means exclude.) "))
                          "include"
                        "exclude")))
         (skg--request-text-search
          clean-terms regex body operators choice)))
     nil)
    (skg-submit-request tcp-proc request-s-exp)))

(defvar skg--search-buffer-setup-hook nil
  "Hook run inside a freshly populated search buffer.
Each function is called with no arguments, with the search buffer
as `current-buffer'. Used by `skg-search-make-link' to upgrade the
buffer to link-creation mode.")

(defvar-local skg--search-request-spec nil
  "Arguments which reproduce this live search after rank-only repair.")

(defun skg--display-search-phase1
    (payload search-terms regex body operators ugly-choice)
  "Display phase 1 search results (without paths).
Sets skg-view-uri to \"search:TERMS\" and registers a
kill-buffer-hook to send close-view to the server."
  (let* ((response (read payload))
         (content (skg--as-string (cadr (assoc 'content response))))
         (warnings (cadr (assoc 'warnings response)))
         (authority (skg--view-authority-from-response response))
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
        (setq skg--search-request-spec
              (list search-terms regex body operators ugly-choice))
        (skg-register-buffer
         (current-buffer) 'search-view
         :view-uri view-uri
         :recipe `((kind . "search")
                   (terms . ,search-terms)
                   (regex . ,regex)
                   (body . ,body)
                   (operators . ,operators)
                   (ugly-choice . ,ugly-choice))
         :last-fetched content
         :graph-generation (plist-get authority :graph-generation)
         :presentation-generation
         (plist-get authority :presentation-generation)
         :server-revision (plist-get authority :server-revision)
         :application-token (plist-get authority :application-token))
        (add-hook 'kill-buffer-hook #'skg-send-close-view nil t)
        (run-hooks 'skg--search-buffer-setup-hook)
        (switch-to-buffer (current-buffer)) ))
    (when warnings
      (skg-big-nonfatal-message
       "*SKG Search Warnings*"
       "Search completed with warnings"
       (skg-errors-and-warnings-to-org-string nil warnings)))))

(defun skg-refresh-live-searches-after-rank-repair ()
  "Rerun every clean live search after authoritative rank repair.
Modified search buffers are preserved and reported rather than overwritten."
  (let ((specs nil)
        (skipped nil))
    (dolist (buffer (buffer-list))
      (when (buffer-local-value 'skg--search-request-spec buffer)
        (if (buffer-modified-p buffer)
            (push (buffer-name buffer) skipped)
          (push (buffer-local-value 'skg--search-request-spec buffer)
                specs))))
    (dolist (spec (nreverse specs))
      (apply #'skg--request-text-search spec))
    (when skipped
      (display-warning
       'skg
       (format "Cyclic-root ranks changed, but modified search buffer(s) were left untouched: %s"
               (mapconcat #'identity (nreverse skipped) ", "))
       :warning))
    (length specs)))

(defun skg--as-string (value)
  "Convert VALUE to a string. Symbols become their name."
  (cond ((stringp value) value)
        ((symbolp value) (symbol-name value))
        ((null value) nil)
        (t (format "%s" value))))

(defun skg--display-search-enrichment (tcp-proc payload)
  "Replace search buffer with results
'enriched' with containerward paths and graphnodestats.
PAYLOAD contains response-type, terms, and content.
Exits readonly after replacing content."
  (let* ((response (read payload))
         (terms   (skg--as-string (cadr (assoc 'terms   response))))
         (content (skg--as-string (cadr (assoc 'content response))))
         (warnings (cadr (assoc 'warnings response)))
         (operation-id (cadr (assoc 'operation-id response)))
         (uri (cadr (assoc 'view-uri response)))
         (client-buffer-id (cadr (assoc 'client-buffer-id response)))
         (graph-generation
          (skg--nat-from-response response 'graph-generation))
         (presentation-generation
          (skg--nat-from-response response 'presentation-generation))
         (base-revision
          (skg--nat-from-response response 'viewforest-base-revision))
         (result-revision
          (skg--nat-from-response response 'resulting-server-revision))
         (base-graph-generation
          (skg--nat-from-response response 'view-base-graph-generation))
         (base-presentation-generation
          (skg--nat-from-response
           response 'view-base-presentation-generation))
         (expected-token
          (skg--nat-from-response
           response 'expected-client-application-token))
         (result-token
          (skg--nat-from-response
           response 'resulting-client-application-token))
         (applied nil)
         (client-token expected-token))
    (when (and terms content)
      (let ((buf (or (skg-find-buffer-by-id client-buffer-id)
                     (and uri (skg-find-buffer-by-uri uri)))))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (let ((old-point (point)))
              (condition-case err
                  (progn
                    (skg-replace-buffer-with-new-content
                     nil content nil
                     (list
                      :client-buffer-id client-buffer-id
                      :view-uri uri
                      :base-server-revision base-revision
                      :base-graph-generation base-graph-generation
                      :base-presentation-generation
                      base-presentation-generation
                      :expected-application-token expected-token
                      :graph-generation graph-generation
                      :presentation-generation presentation-generation
                      :server-revision result-revision
                      :application-token result-token
                      :require-clean t))
                    (setq applied t
                          client-token skg--application-token)
                    (goto-char (min old-point (point-max))))
                (error
                 (setf (skg--buffer-record-search-stale
                        skg--buffer-record) t)
                 (skg-log 'error 'search
                          "search enrichment refused for %s: %S"
                          (buffer-name) err))))
            (setq buffer-read-only nil)
            (when applied (message "Search results enriched.")) )) ))
    (when operation-id
      (skg-register-response-handler 'collateral-applied #'ignore t)
      (skg-submit-request
       tcp-proc
       (concat
        (prin1-to-string
         `((request . "apply collateral")
           (operation-id . ,operation-id)
           (view-uri . ,uri)
           (applied . ,(if applied "true" "false"))
           (graph-generation . ,graph-generation)
           (presentation-generation . ,presentation-generation)
           (viewforest-base-revision . ,base-revision)
           (client-token . ,client-token)))
        "\n")))
    (when warnings
      (skg-big-nonfatal-message
       "*SKG Search Warnings*"
       "Search enrichment completed with warnings"
       (skg-errors-and-warnings-to-org-string nil warnings)))))

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
        (let* ((record skg--buffer-record)
               (buffer-contents (skg-buffer-raw-text))
               (request-s-exp
                (concat (prin1-to-string
                         `((request . "snapshot response")
                           (terms . ,terms)
                           (client-buffer-id
                            . ,(skg--buffer-record-id record))
                           (graph-generation
                            . ,(skg--buffer-record-graph-generation record))
                           (presentation-generation
                            . ,(skg--buffer-record-presentation-generation
                                record))
                           (server-revision
                            . ,(skg--buffer-record-server-revision record))
                           (client-application-token
                            . ,(skg--buffer-record-application-token record))))
                        "\n")))
          (skg-submit-request-continuation
           tcp-proc request-s-exp buffer-contents))))))

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
