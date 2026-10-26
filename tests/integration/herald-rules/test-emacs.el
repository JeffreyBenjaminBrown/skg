;;; Integration test for the herald-rules fetch path.
;;; Fetches the herald rule table from a running server and checks
;;; that it lands in the heralds cache and drives the lens engine.

;; Load the project elisp configuration
(load-file "../../../elisp/skg-init.el")
(load-file "../test-wait.el")

(defun integration-test-herald-rules ()
  "Fetch the herald rule table over TCP and verify it installs."
  (message "Starting herald-rules integration test...")
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))
      (message "Using test port: %d" skg-port)))
  (skg-request-herald-rules)
  (message "Sent herald rules request...")
  (if (not (skg-test-wait-for-response 10))
      (progn
        (message "TIMEOUT: No response received!")
        (kill-emacs 1))
    (if (not (and (listp heralds--transform-rules)
                  (eq (car heralds--transform-rules) 'skg)))
        (progn
          (message "FAIL: rule table not installed: %S"
                   heralds--transform-rules)
          (kill-emacs 1))
      ;; The fetched table should drive the lens engine end to end.
      (let ((herald (heralds-from-metadata
                     "(skg node (gnid 1) (skgRepos (nodeRepo main)) writeProtected)"))
            ;; The label table served beside the rules labels a boundary.
            (boundary (heralds-from-metadata
                       "(skg node (gnid 1) (skgRepos (nodeRepo main)) (view repoBoundary))")))
        (cond ((not (and herald (string-match-p "☮" herald)))
               (message "FAIL: fetched table did not produce the writeProtected herald: %S"
                        herald)
               (kill-emacs 1))
              ((not (and boundary (string-match-p "⌂:m" boundary)))
               (message "FAIL: the served label table did not label the repo boundary: %S"
                        boundary)
               (kill-emacs 1))
              (t
               (message "PASS: Integration test successful!")
               (kill-emacs 0)))))))

;; Set a timeout in case things hang
(run-at-time 15 nil (lambda ()
                      (message "TIMEOUT: Integration test timed out!")
                      (kill-emacs 1)))

;; Run the test
(integration-test-herald-rules)
