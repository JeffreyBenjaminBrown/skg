;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Show what the server warned about as it started (unknown
;;; skgconfig.toml keys, say), and each view's buffer corrections.
;;;
;;; Startup warnings: `skg-tcp-connect-to-rust' sends the request once
;;; per connection, as soon as the connection is up and so before any
;;; view is requested. A non-empty answer opens
;;; `skg-server-warnings-buffer-name', replacing what it held; an empty
;;; one opens nothing.
;;;
;;; Buffer corrections: a save's save-result and a rerender stream's
;;; rerender-done can name, per view the server refreshed, the
;;; headlines it corrected. `skg-show-corrections-in-server-warnings'
;;; appends them to the same buffer, under a headline "Buffer
;;; corrections in" the view's name, keeping what it held.
;;;
;;; DATA USED/ASSUMED: See docs/api-and-formats.org, "Server warnings".

(require 'skg-length-prefix)
(require 'skg-request-save) ; for skg-big-nonfatal-message, skg-messages-to-org-string
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
  "Show the warnings in a server-warnings response PAYLOAD, if any."
  (condition-case err
      (let ((warnings (cadr (assoc 'warnings (read payload)))))
        (when (skg--message-list-nonempty-p warnings)
          (skg-big-nonfatal-message
           skg-server-warnings-buffer-name
           (format "The skg server reported %d startup warning%s - see %s"
                   (length warnings)
                   (if (= 1 (length warnings)) "" "s")
                   skg-server-warnings-buffer-name)
           (skg-messages-to-org-string warnings))))
    (error
     (message "skg: server-warnings handler error: %S" err))))

(defun skg-show-corrections-in-server-warnings (corrections)
  "Append CORRECTIONS to `skg-server-warnings-buffer-name' and show it.
CORRECTIONS is the (corrections ...) field of a save-result or a
rerender-done, the buffer corrections: one entry (VIEW-ID MESSAGE
...) per view the server corrected. Each view becomes a level-1
headline, \"Buffer corrections in\" and its name (see
`skg--corrected-view-name'), and each of its messages a level-2
headline under it. What the buffer already held stays.
Does nothing when CORRECTIONS is empty."
  (when (skg--message-list-nonempty-p corrections)
    (let* ((buffer (get-buffer-create skg-server-warnings-buffer-name))
           (view-count (length corrections))
           (headline-count
            (apply #'+ (mapcar (lambda (view-entry)
                                 (length (cdr view-entry)))
                               corrections)))
           (appended-start ;; where the new text begins
            (with-current-buffer buffer
              (unless (derived-mode-p 'org-mode)
                (skg--org-mode-with-options))
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                (unless (bolp) (insert "\n"))
                (prog1 (point)
                  (insert (skg--corrections-to-org-string corrections))
                  (set-buffer-modified-p nil)))))
           (window (display-buffer buffer)))
      (with-current-buffer buffer
        (goto-char appended-start))
      (when window
        (set-window-point window appended-start))
      (message "skg: the server corrected %d headline%s in %d view%s - see %s"
               headline-count (if (= 1 headline-count) "" "s")
               view-count (if (= 1 view-count) "" "s")
               skg-server-warnings-buffer-name))))

(defun skg--corrections-to-org-string (corrections)
  "Org text for CORRECTIONS: per view, a level-1 headline naming it
\"Buffer corrections in\" the view's name,
and under it each message as a level-2 headline. A message's own
subheadlines are already level 3, so the message is inserted as it is."
  (mapconcat
   (lambda (view-entry)
     (concat "* Buffer corrections in "
             (skg--corrected-view-name (car view-entry)) "\n"
             (mapconcat (lambda (correction)
                          (let ((headline (concat "** " correction)))
                            (if (string-suffix-p "\n" headline)
                                headline
                              (concat headline "\n"))))
                        (cdr view-entry) "")))
   corrections ""))

(defun skg--corrected-view-name (view-id)
  "The name of the buffer whose `skg-view-id' is VIEW-ID, or, when no
buffer has it, VIEW-ID itself. VIEW-ID may be a symbol or a string."
  (let* ((view-id-string (format "%s" view-id))
         (buffer (skg-find-buffer-by-view-id view-id-string)))
    (if buffer
        (buffer-name buffer)
      view-id-string)))

(provide 'skg-request-server-warnings)
