;;; Request the 'kindness' content view and measure wall-clock time.

(load-file "../elisp/skg-init.el")
(load-file "integration/test-wait.el")

(defvar skg-port 1730)
(let ((test-port (getenv "SKG_TEST_PORT")))
  (when test-port
    (setq skg-port (string-to-number test-port))))

(message "=== Profiling: requesting kindness content view ===")
(message "Port: %d" skg-port)

(let ((t0 (float-time)))
  (skg-request-single-root-content-view-from-id
   "0d863b6d-1652-4ffb-897a-99e73198ce16")
  (message "Request sent. Waiting for response...")

  ;; Wait up to 120s for the buffer to appear
  (let ((buf (skg-test-wait-for-buffer "*kindness*" 120)))
    (if buf
        (progn
          (message "=== Response received in %.3f seconds ==="
                   (- (float-time) t0))
          (with-current-buffer buf
            (message "Buffer size: %d bytes" (buffer-size))
            (message "First 200 chars: %s"
                     (buffer-substring-no-properties
                      (point-min)
                      (min (point-max) (+ (point-min) 200))))))
      (message "=== TIMEOUT after %.1f seconds ===" (- (float-time) t0))))

  (kill-emacs 0))
