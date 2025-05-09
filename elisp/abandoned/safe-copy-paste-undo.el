;; ABANDONED. Too hard.
;; Instead I just changed the spec --
;; an ID *can* be `contained` in more than one place.
;; If the user wants to avoid that (as in org-roam),
;; they can. If they don't,
;; following a hyperlink with multiple targets will
;; bring up a menu of options to the user.

;; Define advice functions for all kill/copy/paste operations
(defun skg-advice-for-kill (&rest _)
  (message "region moved with ID property into kill-ring"))

(defun skg-advice-for-copy (&rest _)
  (message "region copied *without* ID property into kill-ring"))

(defun skg-paste-advice (&rest _)
  "After pasting, check for ID properties in the pasted text.
If found, remove only the ID properties from the kill-ring entry
while preserving all other text properties."
  (let* ((start (save-excursion
                  (backward-char (length (current-kill 0)))
                  (point)))
         (end (point))
         (has-id nil))

    ;; Check if the pasted text has any ID properties
    (save-excursion
      (goto-char start)
      (while (and (< (point) end)
                  (not has-id))
        (let ((id-prop (get-text-property (point) 'id)))
          (when id-prop
            (setq has-id t)))
        (forward-char 1)))

    ;; If ID properties were found, modify the kill-ring entry
    (when has-id
      ;; Get the current kill-ring text
      (let* ((current-text (current-kill 0))
             ;; Create a new string with the same content
             (clean-text (copy-sequence current-text)))

        ;; Remove only the 'id' property from the entire string
        (remove-text-properties 0 (length clean-text) '(id nil) clean-text)

        ;; Replace just the first entry in the kill-ring
        (setcar kill-ring clean-text)

        (message "Pasted with ID property. ID property stripped from copy that remains in kill-ring.")))

    ;; If no ID property was found, just leave everything as is
    (unless has-id
      (message "Pasted text without ID property."))))

;; Functions to add/remove all advice
(defun skg-enable-all-advice ()
  ;; Modify kill commands
  (advice-add 'kill-region :after #'skg-advice-for-kill)
  (advice-add 'kill-line :after #'skg-advice-for-kill)
  (advice-add 'kill-word :after #'skg-advice-for-kill)
  (advice-add 'backward-kill-word :after #'skg-advice-for-kill)
  (advice-add 'kill-sentence :after #'skg-advice-for-kill)
  (advice-add 'kill-sexp :after #'skg-advice-for-kill)
  (advice-add 'org-kill-line :after #'skg-advice-for-kill)

  ;; Modify copy command
  (advice-add 'kill-ring-save :after #'skg-advice-for-copy)

  ;; Modify paste commands
  (advice-add 'yank :after #'skg-advice-for-paste)
  (advice-add 'yank-pop :after #'skg-advice-for-paste)
  (advice-add 'org-yank :after #'skg-advice-for-paste))

(defun skg-disable-all-advice ()
  "Disable advice for all kill/copy/paste commands."
  ;; Kill commands
  (advice-remove 'kill-region #'skg-advice-for-kill)
  (advice-remove 'kill-line #'skg-advice-for-kill)
  (advice-remove 'kill-word #'skg-advice-for-kill)
  (advice-remove 'backward-kill-word #'skg-advice-for-kill)
  (advice-remove 'kill-sentence #'skg-advice-for-kill)
  (advice-remove 'kill-sexp #'skg-advice-for-kill)
  (advice-remove 'org-kill-line #'skg-advice-for-kill)

  ;; Copy commands
  (advice-remove 'kill-ring-save #'skg-advice-for-copy)

  ;; Paste commands
  (advice-remove 'yank #'skg-advice-for-paste)
  (advice-remove 'yank-pop #'skg-advice-for-paste)
  (advice-remove 'org-yank #'skg-advice-for-paste))

(define-minor-mode skg-minor-mode
  "Toggle SKG minor mode.
When enabled, copy/kill/paste operations handle ID properties specially."
  :init-value nil
  :lighter " SKG"
  :keymap skg-minor-mode-map
  :group 'org
  (if skg-minor-mode
      (progn
        (skg-enable-all-advice)
        (message "SKG minor mode enabled"))
    (progn
      (skg-disable-all-advice)
      (message "SKG minor mode disabled"))))

(provide 'skg-minor-mode)
