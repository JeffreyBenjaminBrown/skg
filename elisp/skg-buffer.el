;;; -*- lexical-binding: t; -*-

(require 'org-id)
(require 'cl-lib)
(require 'subr-x)
(require 'heralds-minor-mode)
(require 'skg-link-annotations)
(require 'skg-sexpr-search)
(require 'skg-keymaps-and-aliases)
(require 'skg-shared)


(defun skg--org-mode-with-options ()
  "Activate org-mode with skg-appropriate buffer-local settings."
  (org-mode)
  (setq-local org-adapt-indentation nil))

(define-derived-mode skg-content-view-mode org-mode "SKG"
  "Major mode for skg content view buffers, derived from org-mode.
Rebinds C-x C-s to save via the skg server,
and provides C-c prefix keybindings for skg commands."
  (setq-local org-adapt-indentation nil)
  (skg--dictate-view-faces)
  (add-hook 'first-change-hook
            #'skg--confirm-before-dirtying-another-view nil t))

(defun skg--view-face-attributes (entry default-background)
  "The face attributes of ENTRY, one of the view_faces in
'shared/herald-styles.json'. Every attribute the entry leaves out gets
a fixed value, so nothing of the user's theme shows through: the
background is DEFAULT-BACKGROUND, the weight normal, and so on."
  (list :foreground (alist-get 'foreground entry)
        :background (or (alist-get 'background entry) default-background)
        :weight (intern (or (alist-get 'weight entry) "normal"))
        :slant 'normal
        :underline (and (alist-get 'underline entry) t)
        :overline nil :strike-through nil :box nil :inverse-video nil))

(defun skg--dictate-view-faces ()
  "Remap, in this buffer, the faces a view uses to Skg's own, from the
view_faces of 'shared/herald-styles.json'. Colors and weights only; the
typeface stays the user's."
  (let* ((entries (alist-get 'view_faces skg-shared-herald-styles))
         (default-background
          (alist-get 'background
                     (cl-find "default" entries
                              :key (lambda (entry) (alist-get 'name entry))
                              :test #'equal))))
    (dolist (entry entries)
      (let ((attributes (skg--view-face-attributes entry default-background)))
        (dolist (face (alist-get 'emacs entry))
          (face-remap-add-relative (intern face) attributes))))))

(defvar skg--inhibit-dirty-view-confirmation nil
  "Non-nil while skg itself edits view text (re-rendering, save
buffer snapshots), so `skg--confirm-before-dirtying-another-view' stays quiet.")

(defun skg--unsaved-view-buffers (&optional except)
  "Return every live view with unsaved edits, other than EXCEPT.
A view is a buffer with a non-nil `skg-view-id': a content view or
search results, but not the fork-confirmation buffer."
  (cl-remove-if-not
   (lambda (buf)
     (and (not (eq buf except))
          (buffer-local-value 'skg-view-id buf)
          (buffer-modified-p buf)))
   (buffer-list)))

(defvar skg--dirtying-view-approved nil
  "The view whose next first edit the user approved despite another
view's unsaved edits, or nil.  That edit consumes the approval, and
point leaving the view revokes it (see
`skg--revoke-dirtying-view-approval-if-point-left').")

(defun skg--confirm-before-dirtying-another-view ()
  "On `first-change-hook': if another view already has unsaved edits,
cancel this view's first edit, then ask whether to make it anyway.
The question waits until the editing command has finished, because a
prompt opened mid-edit inherits that command's temporary state.  (E.g.
`newline' adds a function to `post-self-insert-hook' that returns point
to the start of the line, so an answer typed there came out reversed.)"
  (cond
   ((eq skg--dirtying-view-approved (current-buffer))
    (skg--forget-dirtying-view-approval))
   ((and skg-view-id
         (not skg--inhibit-dirty-view-confirmation)
         (skg--unsaved-view-buffers (current-buffer)))
    (run-at-time 0 nil #'skg--ask-to-dirty-another-view
                 (current-buffer)
                 (and this-command ;; nil when no command made the edit
                      (this-command-keys-vector)))
    (user-error "Edit paused: another skg buffer has unsaved edits"))))

(defun skg--ask-to-dirty-another-view (buffer keys)
  "Ask whether to edit BUFFER despite another view's unsaved edits.
On yes, approve BUFFER's next first edit, and replay KEYS (the keys of
the cancelled command) if they would reach BUFFER."
  (when (and (buffer-live-p buffer)
             (not (buffer-modified-p buffer)))
    (if (not (yes-or-no-p "WARNING: Another buffer has unsaved edits. If you edit this one as well, your edits could clobber each other. Edit anyway? "))
        (message "Edit cancelled")
      (setq skg--dirtying-view-approved buffer)
      (add-hook 'post-command-hook
                #'skg--revoke-dirtying-view-approval-if-point-left)
      (if (and (> (length keys) 0)
               (eq buffer (window-buffer (selected-window))))
          (setq unread-command-events
                (append (listify-key-sequence keys) unread-command-events))
        (message "Approved: repeat your edit.")))))

(defun skg--revoke-dirtying-view-approval-if-point-left ()
  "On `post-command-hook': revoke the approval once point is in another
buffer, even if the approved view is still visible.  While the
minibuffer is active, the window it will return to is what counts, so
an edit command typed via M-x keeps the approval."
  (unless (eq skg--dirtying-view-approved
              (window-buffer (if (minibufferp)
                                 (minibuffer-selected-window)
                               (selected-window))))
    (skg--forget-dirtying-view-approval)))

(defun skg--forget-dirtying-view-approval ()
  (setq skg--dirtying-view-approved nil)
  (remove-hook 'post-command-hook
               #'skg--revoke-dirtying-view-approval-if-point-left))

(defvar-local skg-view-id nil
  "Unique view ID for this skg buffer.")
(put 'skg-view-id
     'permanent-local ; to survive major-mode changes
     t)

(defvar-local skg-clean-baseline nil
  "Exact normalized text received at the last clean view replacement.")
(put 'skg-clean-baseline 'permanent-local t)

(defvar-local skg-clean-baseline-context nil
  "Context recorded with `skg-clean-baseline'.")
(put 'skg-clean-baseline-context 'permanent-local t)

(defvar-local skg--search-enrichment-includes-user-edits nil
  "Non-nil when enrichment replaced a dirty search-buffer snapshot.")
(put 'skg--search-enrichment-includes-user-edits 'permanent-local t)

(defvar-local skg--search-buffer-snapshot-was-dirty nil
  "Whether the buffer snapshot currently being enriched contained unsaved edits.")
(put 'skg--search-buffer-snapshot-was-dirty 'permanent-local t)

(defvar-local skg-contentView-initialRoot-repo nil
  "Repo of the initial first root in this skg content view.
Captured when the view opens and retained only to disambiguate its
buffer name if another content view opens with the same title.  Later
view-forest edits do not change it.")
(put 'skg-contentView-initialRoot-repo 'permanent-local t)

(defun skg--capture-clean-baseline ()
  "Record the current normalized view text and available context."
  ;; A baseline is protocol/recovery data, not display data.  Interactive
  ;; fontification can attach face and other text properties to the buffer;
  ;; retaining them makes `prin1-to-string' emit Emacs's #(...) syntax when
  ;; this baseline later travels in a save envelope.
  (setq skg-clean-baseline
        (buffer-substring-no-properties (point-min) (point-max)))
  (setq skg-clean-baseline-context
        (list :git-diff-mode
              (and (boundp 'skg--git-diff-mode-enabled)
                   skg--git-diff-mode-enabled)
              :repo-set "unavailable"))
  (setq skg--search-enrichment-includes-user-edits nil))

(defun skg-content-view-buffer-name (org-text)
  "Generate buffer name for content view from ORG-TEXT."
  (let ((title (skg-extract-top-headline-title org-text)))
    (if title
        (concat "*" (skg-sanitize-buffer-name
                     (skg-normalize-buffer-name-links title)) "*")
      (error "skg: content view has no headline (first 200 chars: %s)"
             (substring (or org-text "") 0 (min 200 (length (or org-text ""))))))))

(defun skg-content-view-repo-name (org-text)
  "Return the initial first root's repo from ORG-TEXT, or nil.
The first headline of a content view begins with a skg metadata sexp.
Malformed or absent metadata is tolerated here because it should not
prevent a view from opening."
  (condition-case nil
      (when (and org-text
                 (string-match "^\\*+ +(skg\\_>" org-text))
        (let* ((start (match-beginning 0))
               (sexp-start (string-match "(skg\\_>" org-text start))
               (sexp (car (read-from-string org-text sexp-start)))
               (kind (or (assoc 'node (cdr sexp))
                         (assoc 'diffPhantom (cdr sexp))
                         (assoc 'deleted (cdr sexp))))
               (skgrepo (and kind (cadr (assoc 'repo (cdr kind))))))
          (when skgrepo (format "%s" skgrepo))))
    (error nil)))

(defun skg--repo-qualified-buffer-name (buffer-name skgrepo)
  "Append REPO in angle brackets to BUFFER-NAME."
  (format "%s <%s>" buffer-name (skg-sanitize-buffer-name skgrepo)))

(defun skg--generate-contentView-buffer (buffer-name skgrepo)
  "Generate a new content-view buffer named from BUFFER-NAME and REPO.
When BUFFER-NAME is occupied by an skg view from another known repo,
rename that view and the new one with repo qualifiers.  Otherwise use
Emacs's conventional numeric suffix.  Never reuse or erase an existing
buffer."
  (let ((existing (get-buffer buffer-name)))
    (if (not existing)
        (generate-new-buffer buffer-name)
      (let* ((existing-skgrepo
              (and (skg-buffer-p existing)
                   (buffer-local-value
                    'skg-contentView-initialRoot-repo existing)))
             (existing-name
              (and existing-skgrepo
                   (skg--repo-qualified-buffer-name
                    buffer-name existing-skgrepo)))
             (new-name
              (and skgrepo
                   (skg--repo-qualified-buffer-name buffer-name skgrepo))))
        (if (and existing-skgrepo skgrepo
                 (not (string= existing-skgrepo skgrepo))
                 (not (get-buffer existing-name))
                 (not (get-buffer new-name)))
            (progn
              (with-current-buffer existing
                (rename-buffer existing-name))
              (generate-new-buffer new-name))
          (generate-new-buffer buffer-name))))))

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

(defun skg-open-empty-content-view ()
  "Open a new, empty skg content view buffer."
  (skg-open-org-buffer-from-text
   nil "" "*skg-empty*"))

(defun skg-open-org-buffer-from-text (_tcp-proc org-text buffer-name &optional view-id)
  "Open a new buffer and insert ORG-TEXT, enabling org-mode.
If VIEW-ID is provided, set it as the buffer's skg-view-id;
otherwise generate a new UUID."
  (let* ((skgrepo (skg-content-view-repo-name org-text))
         (buffer (skg--generate-contentView-buffer buffer-name skgrepo))
        (view-id (or view-id (org-id-uuid))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (skg--inhibit-dirty-view-confirmation t))
        (erase-buffer)
        (insert org-text)
        (skg-content-view-mode)
        (heralds-minor-mode)
        (skg-link-annotations-mode 1))
      (setq skg-view-id view-id)
      (setq skg-contentView-initialRoot-repo skgrepo)
      (add-hook 'kill-buffer-hook #'skg-send-close-view nil t)
      (set-buffer-modified-p nil)
      (skg--capture-clean-baseline)
      (goto-char (point-min)))
    (switch-to-buffer buffer)))

(defun skg-send-close-view-id (tcp-proc view-id)
  "Send a close-view message for VIEW-ID over TCP-PROC."
  (when (and view-id tcp-proc (process-live-p tcp-proc))
    (let ((request (concat (prin1-to-string
                            `((request . "close view")
                              (view-id . ,view-id)))
                           "\n")))
      (process-send-string tcp-proc request))))

(defun skg-send-close-view ()
  "Send a close-view message to the server for this buffer's view ID."
  (when (boundp 'skg-rust-tcp-proc)
    (skg-send-close-view-id skg-rust-tcp-proc skg-view-id)))

(defun skg--other-unsaved-skg-buffers ()
  "Return modified skg view buffers other than the current buffer."
  (let ((self (current-buffer))
        (result nil))
    (dolist (buf (buffer-list))
      (when (and (not (eq buf self))
                 (buffer-local-value 'skg-view-id buf)
                 (buffer-modified-p buf))
        (push buf result)))
    result))

(defun skg-find-buffer-by-view-id (view-id)
  "Find the buffer whose skg-view-id matches VIEW-ID."
  (cl-find-if (lambda (buf)
                (string= view-id (buffer-local-value 'skg-view-id buf)))
              (buffer-list)))

(defun skg-buffer-p (buf)
  "Return non-nil if BUF is a skg view buffer.
A buffer qualifies if it carries the buffer-local `skg-view-id'
or derives from `skg-content-view-mode'. Both are set solely by
skg's own view code (`skg-open-org-buffer-from-text') and both
survive `skg-reload' (which deliberately leaves `skg-buffer'
loaded), so they never match a file the user merely opened --
e.g. a real .skg.org file whose first headline begins with
`(skg', which must never be reaped by
`skg-close-all-skg-buffers'."
  (and (buffer-live-p buf)
       (with-current-buffer buf
         (or (and (boundp 'skg-view-id) skg-view-id)
             (derived-mode-p 'skg-content-view-mode)))))

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
