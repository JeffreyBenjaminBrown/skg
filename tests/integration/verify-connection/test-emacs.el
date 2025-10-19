;;; Integration test for skg verify-connection
;;; This script tests the verify-connection functionality end-to-end

;; Load the project elisp configuration
(load-file "../../../elisp/skg-init.el")

;; Test result tracking
(defvar integration-test-result nil)
(defvar integration-test-completed nil)

(defun integration-test-verify-connection-handler (tcp-proc string)
  "Custom handler for integration test to capture the response."
  (let ((response (string-trim string)))
    (message "Received response: %s" response)
    (setq integration-test-result response)
    (setq integration-test-completed t)

    ;; Check if we got the expected response
    (if (string-match-p "This is the skg server verifying the connection" response)
        (progn
          (message "PASS: Integration test successful!")
          (kill-emacs 0))
      (progn
        (message "FAIL: Integration test failed!")
        (message "Expected response containing 'This is the skg server verifying the connection'")
        (message "Got: %s" response)
        (kill-emacs 1)))))

(defun integration-test-verify-connection ()
  "Integration test for verify connection functionality."
  (message "Starting integration test...")

  ;; Set the port from environment variable if available
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))
      (message "Using test port: %d" skg-port)))

  ;; Wait a moment for server to be fully ready
  (sleep-for 0.25)

  ;; Send the request with our custom handler
  (let* ((tcp-proc (skg-tcp-connect-to-rust))
         (request-sexp "((request . \"verify connection\"))\n"))
    (setq skg-doc--response-handler #'integration-test-verify-connection-handler)
    (process-send-string tcp-proc request-sexp)
    (message "Sent verify connection request..."))

  ;; Wait for response with timeout
  (let ((timeout 0))
    (while (and (not integration-test-completed) (< timeout 50))
      (sleep-for 0.25)
      (setq timeout (1+ timeout))))

  ;; If we got here without completion, it's a timeout
  (unless integration-test-completed
    (message "TIMEOUT: No response received!")
    (kill-emacs 1)))

;; Set a timeout in case things hang
(run-at-time 10 nil (lambda ()
                      (message "TIMEOUT: Integration test timed out!")
                      (kill-emacs 1)))

;; Run the test
(integration-test-verify-connection)