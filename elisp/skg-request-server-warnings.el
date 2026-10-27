;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Show what the server warned about as it started (unknown
;;; skgconfig.toml keys, say), and each view's buffer corrections.
;;;
;;; Startup warnings: `skg-tcp-connect-to-rust' sends the request once
;;; per connection, as soon as the connection is up and so before any
;;; view is requested. A non-empty answer is appended to
;;; `skg-server-warnings-buffer-name', under a headline "Startup
;;; warnings", keeping what it held (the buffer corrections gathered
;;; before a reconnect); an empty one opens nothing.
;;;
;;; Buffer corrections: a save's save-result and a rerender stream's
;;; rerender-done can name, per view the server refreshed, the
;;; headlines it corrected. `skg-show-corrections-in-server-warnings'
;;; appends them to the same buffer, under a headline "Buffer
;;; corrections in" the view's name, keeping what it held.
;;;
;;; DATA USED/ASSUMED: See docs/api-and-formats.org, "Server warnings".

(require 'skg-length-prefix)
(require 'skg-request-save) ; for skg--message-list-nonempty-p
(require 'skg-buffer) ; for skg-find-buffer-by-view-id, skg--org-mode-with-options
(require 'skg-state)

(defconst skg-server-warnings-buffer-name "*SKG Server Warnings*"
  "The buffer that shows the server's startup warnings, and the
buffer corrections the server made to each view's headlines.")

(defun skg-request-server-warnings (tcp-proc)
  "Ask the server on TCP-PROC for its startup warnings.
A one-shot handler shows a non-empty answer.
PITFALL: No `skg-lp-reset' here, unlike most request functions.
This request is sent from inside `skg-tcp-connect-to-rust', and the
caller's own request follows it at once."
  (skg-register-response-handler
   'server-warnings
   #'skg--server-warnings-handler
   t)
  (process-send-string
   tcp-proc
   "((request . \"server warnings\"))\n"))

(defun skg--server-warnings-handler (_tcp-proc payload)
  "Append the warnings in a server-warnings response PAYLOAD, if any,
to `skg-server-warnings-buffer-name': a level-1 headline \"Startup
warnings\", each warning a level-2 headline under it."
  (condition-case err
      (let ((warnings (cadr (assoc 'warnings (read payload)))))
        (when (skg--message-list-nonempty-p warnings)
          (skg--append-to-server-warnings
           (concat "* Startup warnings\n"
                   (mapconcat (lambda (warning)
                                (concat "** " warning "\n"))
                              warnings ""))
           (format "The skg server reported %d startup warning%s - see %s"
                   (length warnings)
                   (if (= 1 (length warnings)) "" "s")
                   skg-server-warnings-buffer-name))))
    (error
     (message "skg: server-warnings handler error: %S" err))))

(defun skg--append-to-server-warnings (org-text message-text)
  "Append ORG-TEXT to `skg-server-warnings-buffer-name', keeping what
it held; show the buffer with point at the appended text, and
MESSAGE-TEXT in the echo area."
  (let* ((buffer (get-buffer-create skg-server-warnings-buffer-name))
         (appended-start ;; where the new text begins
          (with-current-buffer buffer
            (unless (derived-mode-p 'org-mode)
              (skg--org-mode-with-options))
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (unless (bolp) (insert "\n"))
              (prog1 (point)
                (insert org-text)
                (set-buffer-modified-p nil)))))
         (window (display-buffer buffer)))
    (with-current-buffer buffer
      (goto-char appended-start))
    (when window
      (set-window-point window appended-start))
    (message "%s" message-text)))

(defun skg-show-corrections-in-server-warnings (corrections)
  "Append CORRECTIONS to `skg-server-warnings-buffer-name' and show it.
CORRECTIONS is the (corrections ...) field of a save-result or a
rerender-done, the buffer corrections: one entry (VIEW-ID MESSAGE
...) per view the server corrected. Each view becomes a level-1
headline, \"Buffer corrections in\" and its name (see
`skg--corrections-to-org-string'), and each of its messages a level-2
headline under it. What the buffer already held stays.
Does nothing when CORRECTIONS is empty."
  (when (skg--message-list-nonempty-p corrections)
    (let ((view-count (length corrections))
          (headline-count
           (apply #'+ (mapcar (lambda (view-entry)
                                (length (cdr view-entry)))
                              corrections))))
      (skg--append-to-server-warnings
       (skg--corrections-to-org-string corrections)
       (format "skg: the server corrected %d headline%s in %d view%s - see %s"
               headline-count (if (= 1 headline-count) "" "s")
               view-count (if (= 1 view-count) "" "s")
               skg-server-warnings-buffer-name)))))

(defun skg--corrections-to-org-string (corrections)
  "Org text for CORRECTIONS: per view, a level-1 headline, \"Buffer
corrections in\" and the name of the buffer whose `skg-view-id' is
the view's ID, or the ID itself when no buffer has it; under it each
message as a level-2 headline. A message's own subheadlines are
already level 3, so the message is inserted as it is. The view ID may
be a symbol or a string."
  (mapconcat
   (lambda (view-entry)
     (let* ((view-id (format "%s" (car view-entry)))
            (view-buffer (skg-find-buffer-by-view-id view-id)))
       (concat "* Buffer corrections in "
               (if view-buffer (buffer-name view-buffer) view-id) "\n"
               (mapconcat (lambda (correction)
                            (let ((headline (concat "** " correction)))
                              (if (string-suffix-p "\n" headline)
                                  headline
                                (concat headline "\n"))))
                          (cdr view-entry) ""))))
   corrections ""))

(provide 'skg-request-server-warnings)
