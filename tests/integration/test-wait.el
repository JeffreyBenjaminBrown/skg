;;; Shared polling utilities for integration tests.
;;; Load with (load-file "../test-wait.el") from any test directory.
;;;
;;; These replace fixed (sleep-for N) calls with adaptive polling,
;;; so tests don't flake under heavy parallel TypeDB contention.

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
Polls until skg-doc--response-handler becomes nil, which happens
when the LP completion handler finishes (for content-view and save
requests). Returns t on success, nil on timeout."
  (skg-test-wait-for
   (lambda () (null skg-doc--response-handler))
   timeout-secs))

(provide 'test-wait)
