;;; Shared polling utilities for integration tests.
;;; Load with (load-file "../test-wait.el") from any test directory.
;;;
;;; These replace fixed (sleep-for N) calls with adaptive polling,
;;; so tests don't flake under heavy parallel server load.

(defun skg-test-fontify-before-save (&rest _ignored)
  "Materialize the same Org text properties interactive buffers carry.
Batch Emacs does not fontify undisplayed buffers on its own.  Without this,
save integrations exercise only unpropertized strings and miss wire-format
bugs caused by normal interactive fontification."
  (when (derived-mode-p 'org-mode)
    (font-lock-ensure (point-min) (point-max))))

;; Every Emacs integration runs in its own process, but guard this so the
;; shared helper also remains safe to reload while debugging a test.
(unless (advice-member-p #'skg-test-fontify-before-save
                         'skg-request-save-buffer)
  (advice-add 'skg-request-save-buffer
              :before #'skg-test-fontify-before-save))

(defun skg-test-wait-for (predicate &optional timeout-secs)
  "Poll PREDICATE every 0.1s, processing network I/O between polls.
Returns the first non-nil value from PREDICATE, or nil after
TIMEOUT-SECS (default 15) seconds."
  (let ((deadline (+ (float-time) (or timeout-secs 15)))
        result)
    (while (and (not (setq result (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.1))
    result))

(defun skg-test-wait-for-buffer (buffer-name &optional timeout-secs)
  "Wait for a buffer named BUFFER-NAME to exist.
Returns the buffer, or nil on timeout."
  (skg-test-wait-for
   (lambda () (get-buffer buffer-name))
   timeout-secs))

(defun skg-test-wait-for-response (&optional timeout-secs)
  "Wait for the current request's response to be fully processed.
Polls until no one-shot handlers are pending and the LP machine
is idle. Returns t on success, nil on timeout."
  (skg-test-wait-for
   (lambda () (and (= 0 skg-lp--pending-count)
                   (null skg-lp--bytes-left)
                   (= 0 (length skg-lp--buf))))
   timeout-secs))

(provide 'test-wait)
