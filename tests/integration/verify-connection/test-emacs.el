;;; Integration test for skg verify-connection
;;; This script tests the verify-connection functionality end-to-end

;; Load the project elisp configuration
(load-file "../../../elisp/skg-init.el")
(load-file "../test-wait.el")

;; Test result tracking
(defvar integration-test-completed nil)

(defun integration-test-verify-connection ()
  "Integration test for verify connection functionality."
  (message "Starting integration test...")

  ;; Set the port from environment variable if available
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))
      (message "Using test port: %d" skg-port)))

  ;; Send the request using the standard function
  (skg-connection-verify)
  (message "Sent verify connection request...")

  ;; Wait for response
  (if (skg-test-wait-for-response 10)
      (progn
        (message "PASS: Integration test successful!")
        (setq integration-test-completed t)
        (kill-emacs 0))
    (progn
      (message "TIMEOUT: No response received!")
      (kill-emacs 1))))

;; Set a timeout in case things hang
(run-at-time 10 nil (lambda ()
                      (message "TIMEOUT: Integration test timed out!")
                      (kill-emacs 1)))

;; Run the test
(integration-test-verify-connection)
