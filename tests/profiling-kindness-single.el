;;; Request a view for a LEAF node (no children) to measure
;;; single-node query overhead.

(load-file "../elisp/skg-init.el")
(load-file "integration/test-wait.el")

(defvar skg-port 1730)

(message "=== Profiling: requesting view of leaf node ===")

;; Use one of kindness's leaf children
(let ((t0 (float-time)))
  (skg-request-single-root-content-view-from-id
   "f7f3be5e-0901-4892-bd12-fb3821ed308a")
  (message "Request sent. Waiting for response...")
  (let ((buf (skg-test-wait-for
              (lambda ()
                (cl-find-if
                 (lambda (b) (with-current-buffer b
                 (derived-mode-p 'skg-content-view-mode)))
                 (buffer-list)))
              60)))
    (if buf
        (progn
          (message "=== Response received in %.3f seconds ==="
                   (- (float-time) t0))
          (with-current-buffer buf
            (message "Buffer: %s" (buffer-name buf))
            (message "Buffer size: %d bytes" (buffer-size))))
      (message "=== TIMEOUT ===")))
  (kill-emacs 0))
