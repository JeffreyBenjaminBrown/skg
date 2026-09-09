;;; Shared polling utilities for integration tests.
;;; Load with (load-file "../test-wait.el") from any test directory.
;;;
;;; These replace fixed (sleep-for N) calls with adaptive polling,
;;; so tests don't flake under heavy parallel test contention.

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

(defun skg-test-register-new-empty-view (buffer)
  "Give a hand-built integration BUFFER honest new-view save authority.
Older integration fixtures predate explicit client application records.  They
still construct their text directly so they can test folding and malformed
input, but production saves now require the same typed record as every public
constructor.  Reusing an already registered BUFFER preserves its server-issued
revision and token across successive fixture edits."
  (with-current-buffer buffer
    (if skg--buffer-record
        (setq skg-view-uri
              (skg--buffer-record-view-uri skg--buffer-record))
      (setq skg-view-uri (or skg-view-uri (org-id-uuid)))
      (skg-register-buffer
       buffer 'new-empty-content-view
       :lifecycle 'live-view :disposable nil
       :view-uri skg-view-uri :recipe '((kind . "new-empty"))
       :last-fetched ""
       :graph-generation
       (or (cdr (assq 'graph-generation skg--server-store-state)) 1)
       :presentation-generation 0
       :server-revision 0 :application-token 1))))

(provide 'test-wait)
