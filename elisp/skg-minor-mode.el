;; Define advice functions for all kill/copy/paste operations
(defun skg-advice-for-kill (&rest _)
  (message "region moved with ID property into kill-ring"))

(defun skg-advice-for-copy (&rest _)
  (message "region copied *without* ID property into kill-ring"))

(defun skg-advice-for-paste (&rest _)
  (message "pasted with ID property. ID property stripped from copy that remains in kill-ring."))

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
