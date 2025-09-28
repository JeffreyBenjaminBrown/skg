;; -*- lexical-binding: t; -*-

;; DATA USED/ASSUMED: See /api.md.

(require 'skg-length-prefix)
(require 'skg-metadata)

(defun skg-request-containerward-view (&optional tcp-proc)
  "Asks Rust for containerward view of current headline's node,
sending the full headline text for Rust to parse.
Replaces the current headline and its body with the result.
Optional TCP-PROC allows reusing an existing connection."
  (interactive)
  (condition-case err
      (let ((headline-text (skg-get-current-headline-text))
            (replacement-bounds (skg-find-headline-replacement-bounds))
            (tcp-proc (or tcp-proc (skg-tcp-connect-to-rust)))
            (request-s-exp
             (concat (prin1-to-string
                      `((request . "containerward view")
                        (headline . ,headline-text)))
                     "\n")))
        (setq ;; Prepare LP state and handler
         skg-lp--buf        (unibyte-string) ;; empty string
         skg-lp--bytes-left nil
         skg-doc--response-handler
         (lambda (tcp-proc chunk)
           (skg-lp-handle-generic-chunk
            (lambda (tcp-proc payload)
              (skg-replace-headline-with-containerward-view
               replacement-bounds payload))
            tcp-proc chunk)))
        (process-send-string tcp-proc request-s-exp))
    (error (message "Error: %s"
                    (error-message-string err)))))

(defun skg-find-headline-replacement-bounds ()
  "Find the bounds of current headline and its body text.
Returns a cons cell (START . END) representing the region to replace."
  (save-excursion
    (org-back-to-heading)
    (let ((start (point))
          (regexp (concat "^" (org-get-limited-outline-regexp))))
      (end-of-line)
      (let ((end (if (re-search-forward regexp nil :move)
                     (progn
                       (forward-line 0)
                       (point))
                   (point-max))))
        (cons start end)))))

(defun skg-replace-headline-with-containerward-view
    (replacement-bounds payload)
  "Replace the region specified by REPLACEMENT-BOUNDS with PAYLOAD."
  (let ((start (car replacement-bounds))
        (end (cdr replacement-bounds)))
    (save-excursion
      (goto-char start)
      (delete-region start end)
      (insert payload)
      (unless (string-suffix-p "\n" payload) (insert "\n")))))

(provide 'skg-request-containerward-view)
