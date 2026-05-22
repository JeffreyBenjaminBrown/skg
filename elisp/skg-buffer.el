;;; -*- lexical-binding: t; -*-

(require 'org-id)
(require 'cl-lib)
(require 'subr-x)
(require 'heralds-minor-mode)
(require 'skg-sexpr-search)
(require 'skg-keymaps-and-aliases)


(defun skg--org-mode-with-options ()
  "Activate org-mode with skg-appropriate buffer-local settings."
  (org-mode)
  (setq-local org-adapt-indentation nil))

(define-derived-mode skg-content-view-mode org-mode "SKG"
  "Major mode for skg content view buffers, derived from org-mode.
Rebinds C-x C-s to save via the skg server,
and provides C-c prefix keybindings for skg commands."
  (setq-local org-adapt-indentation nil))

(defvar-local skg-view-uri nil
  "Unique view URI for this skg buffer.")
(put 'skg-view-uri
     'permanent-local ; to survive major-mode changes
     t)

(defun skg-content-view-buffer-name (org-text)
  "Generate buffer name for content view from ORG-TEXT."
  (let ((title (skg-extract-top-headline-title org-text)))
    (if title
        (concat "*" (skg-sanitize-buffer-name
                     (skg-normalize-buffer-name-links title)) "*")
      (error "skg: content view has no headline (first 200 chars: %s)"
             (substring (or org-text "") 0 (min 200 (length (or org-text ""))))))))

(defun skg-search-buffer-name (search-terms)
  "Generate buffer name for title search with SEARCH-TERMS."
  (concat "*?" (skg-sanitize-buffer-name search-terms) "*"))

(defun skg-normalize-buffer-name-links (name)
  "Return NAME with org id links shortened for buffer display.
Every [[id:ID][LABEL]] link is rendered as [[LABEL]], which keeps
the buffer name readable when a title starts with a link."
  (replace-regexp-in-string
   "\\[\\[id:[^]]+\\]\\[\\([^]]+\\)\\]\\]"
   "[[\\1]]"
   name))

(defun skg-extract-top-headline-title (org-text)
  "Extract the title from the first headline in ORG-TEXT.
Strips any leading (skg ...) metadata from the title.
Returns nil if no headline is found."
  (when (and org-text (string-match "^\\*+ +\\(.+\\)$" org-text))
    (let ((after-stars (match-string 1 org-text)))
      (if (string-prefix-p "(skg" after-stars)
          (let ((sexp-end-pos (skg-find-sexp-end after-stars)))
            (if sexp-end-pos
                (string-trim (substring after-stars sexp-end-pos))
              after-stars))
        after-stars))))

(defun skg-sanitize-buffer-name (name)
  "Sanitize NAME for use as a buffer name.
Removes null characters and newlines, trims whitespace,
and truncates to a reasonable length."
  (let* ((no-nulls (replace-regexp-in-string "\0" "" name))
         (no-newlines (replace-regexp-in-string "[\n\r]" " " no-nulls))
         (trimmed (string-trim no-newlines))
         (max-len 80))
    (if (> (length trimmed) max-len)
        (concat (substring trimmed 0 (- max-len 3)) "...")
      trimmed)))

;; Retaining for tests. Not user-facing;
;; dominated by 'skg-view-new-empty'.
(defun skg-open-empty-content-view ()
  "Open a new, empty skg content view buffer."
  (skg-open-org-buffer-from-text
   nil "" "*skg-empty*"))

(defun skg-open-org-buffer-from-text (_tcp-proc org-text buffer-name &optional view-uri)
  "Open a new buffer and insert ORG-TEXT, enabling org-mode.
If VIEW-URI is provided, set it as the buffer's skg-view-uri;
otherwise generate a new UUID."
  (let ((buffer (get-buffer-create buffer-name))
        (uri (or view-uri (org-id-uuid))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert org-text)
        (skg-content-view-mode)
        (heralds-minor-mode))
      (setq skg-view-uri uri)
      (add-hook 'kill-buffer-hook #'skg-send-close-view nil t)
      (add-hook 'first-change-hook
                #'skg-warn-if-other-buffer-modified nil t)
      (set-buffer-modified-p nil)
      (goto-char (point-min)))
    (switch-to-buffer buffer)))

(defun skg-send-close-view ()
  "Send a close-view message to the server for this buffer's view URI."
  (when (and skg-view-uri
             (boundp 'skg-rust-tcp-proc)
             skg-rust-tcp-proc
             (process-live-p skg-rust-tcp-proc))
    (let ((request (concat (prin1-to-string
                            `((request . "close view")
                              (view-uri . ,skg-view-uri)))
                           "\n")))
      (process-send-string skg-rust-tcp-proc request))))

(defun skg-warn-if-other-buffer-modified ()
  "Warn if another skg buffer has unsaved modifications."
  (let ((other-modified
         (cl-some (lambda (buf)
                    (and (not (eq buf (current-buffer)))
                         (buffer-local-value 'skg-view-uri buf)
                         (buffer-modified-p buf)))
                  (buffer-list))))
    (when other-modified
      (message "WARNING: Another skg buffer has unsaved modifications. Saving is ill-defined when multiple buffers have unsaved edits.")) ))

(defun skg-find-buffer-by-uri (uri)
  "Find the buffer whose skg-view-uri matches URI."
  (cl-find-if (lambda (buf)
                (string= uri (buffer-local-value 'skg-view-uri buf)))
              (buffer-list)))

(defun skg-buffer-p (buf)
  "Return non-nil if BUF appears to be a skg buffer.
Tries, in order: the buffer-local `skg-view-uri', derivation
from `skg-content-view-mode', and the shape of the first line
\(a heading whose remainder begins with `\(skg'). The final
fallback lets `skg-close-all-skg-buffers' recognise skg buffers
even after `skg-reload' has cleared the first two indicators:
`unload-feature' wipes buffer-local bindings for vars declared
in the unloaded file and unbinds the major-mode function
\(degrading the buffer to `org-mode')."
  (and (buffer-live-p buf)
       (with-current-buffer buf
         (or (and (boundp 'skg-view-uri) skg-view-uri)
             (derived-mode-p 'skg-content-view-mode)
             (save-excursion
               (goto-char (point-min))
               (looking-at "^\\*+ +(skg"))))))

(defun skg-close-all-skg-buffers ()
  "Kill all skg buffers (see `skg-buffer-p' for what counts).
Clears each buffer's modified flag first, since stale views
are not worth saving. Each kill triggers `skg-send-close-view'
via the buffer's `kill-buffer-hook' (which is a no-op after
`skg-reload' has stripped the hook)."
  (interactive)
  (let ((bufs (cl-remove-if-not #'skg-buffer-p (buffer-list))))
    (dolist (buf bufs)
      (with-current-buffer buf
        (set-buffer-modified-p nil))
      (kill-buffer buf))
    (message "Closed %d skg buffer(s)." (length bufs))))

(provide 'skg-buffer)
