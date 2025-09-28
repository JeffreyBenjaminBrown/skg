;; -*- lexical-binding: t; -*-

;; DATA USED/ASSUMED: See /api.md.

(require 'skg-length-prefix)
(require 'skg-metadata)

(defun skg-request-node-aliases (&optional tcp-proc)
  "Ask Rust for aliases of current headline's node, inserting at appropriate position.
Sends the full headline text for Rust to parse, then inserts aliases
after the headline and body.
Optional TCP-PROC allows reusing an existing connection."
  (interactive)
  (condition-case err
      (let* ((headline-text (skg-get-current-headline-text))
             (insertion-point (skg-find-branch-insertion-point))
             (tcp-proc (or tcp-proc (skg-tcp-connect-to-rust)))
             (request-s-exp
              (concat (prin1-to-string
                       `((request . "node aliases")
                         (headline . ,headline-text)))
                      "\n")))
        (setq ;; Prepare LP state and handler
         skg-lp--buf        (unibyte-string) ;; empty string
         skg-lp--bytes-left nil
         skg-doc--response-handler
         (lambda (tcp-proc chunk)
           (skg-lp-handle-generic-chunk
            (lambda (tcp-proc payload)
              (skg-insert-aliases-at-point
               insertion-point payload))
            tcp-proc chunk)))
        (process-send-string tcp-proc request-s-exp))
    (error (message "Error: %s"
                    (error-message-string err)))))

(defun skg-find-branch-insertion-point ()
  "Find the point after current headline and its body text.
Returns a marker pointing to where aliases should be inserted."
  (save-excursion
    (org-back-to-heading)
    (let ((regexp (concat "^" (org-get-limited-outline-regexp))))
      (end-of-line)
      (if (re-search-forward regexp nil :move)
          (progn
            (forward-line 0)
            (set-marker (make-marker) (point)))
        (set-marker (make-marker) (point-max))))))

(defun skg-insert-aliases-at-point
    (insertion-marker payload)
  "Insert PAYLOAD at INSERTION-MARKER position."
  (save-excursion
    (goto-char insertion-marker)
    (unless (bolp) (insert "\n"))
    (insert payload)
    (unless (string-suffix-p "\n" payload) (insert "\n"))))

(provide 'skg-request-node-aliases)
