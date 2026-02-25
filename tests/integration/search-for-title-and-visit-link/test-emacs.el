;;; Integration test for skg title-matches functionality
;;; This script tests the title-matches API end-to-end including link visiting

;; Load the project elisp configuration
(load-file "../../../elisp/skg-init.el")
(load-file "../../../elisp/skg-test-utils.el")

;; Test result tracking
(defvar integration-test-phase "starting")
(defvar integration-test-completed nil)

(defun test-title-search ()
  "Search for 'apples' and verify search results."
  (message "=== PHASE 1: Requesting title matches for 'apples' ===")
  (skg-request-title-matches "apples")
  (message "Called skg-request-title-matches")

  ;; Wait a moment for the response to be processed
  (sleep-for 0.25)

  ;; Check if the title search buffer was created and contains expected content
  (let ((search-buffer (get-buffer (skg-search-buffer-name "apples"))))
    (if search-buffer
        (with-current-buffer search-buffer
          (let ((content (buffer-substring-no-properties (point-min) (point-max))))
            (message "Search results received")
            (message "Content: %s" content)
            (if (string-match-p "\\[\\[id:apples\\]" content)
                (progn
                  (message "✓ PASS: Found apples link in search results")
                  (setq integration-test-phase "search-complete"))
              (progn
                (message "✗ FAIL: Expected apples link not found in search results")
                (message "Expected pattern: \\[\\[id:apples\\]")
                (message "Got: %s" content)
                (kill-emacs 1)))))
      (progn
        (message "✗ FAIL: No skg search buffer was created")
        (kill-emacs 1)))))

(defun test-visit-link ()
  "Visit the apples link from search results."
  (message "=== PHASE 2: Testing link visit ===")

  ;; Get the search results buffer
  (let ((search-buffer (get-buffer (skg-search-buffer-name "apples"))))
    (if search-buffer
        (with-current-buffer search-buffer
          (goto-char (point-min))
          ;; Find the apples link and position cursor on it
          (if (re-search-forward "\\[\\[id:apples\\]" nil t)
              (progn
                (message "✓ Found apples link, positioning cursor")
                (goto-char (match-beginning 0))
                (message "Line content: %s"
                         (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position)))

                ;; Visit the link - this will set up its own handler
                (message "Calling skg-visit-link...")
                (skg-visit-link)

                ;; Wait a moment for the content view to be created
                (sleep-for 0.25)

                (let ;; Check if the content view buffer was created successfully
                    ((content-buffer (find-skg-content-buffer)))
                  (if content-buffer
                      (with-current-buffer content-buffer
                        (let ((content (buffer-substring-no-properties
                                        (point-min) (point-max))))
                          (message "Content view buffer created successfully")
                          (message "Content: %s" content)
                          (if (string-match-p "apples" content)
                              (message "✓ PASS: Found apples content in view")
                            (progn
                              (message "✗ FAIL: Expected apples content not found")
                              (message "Got: %s" content)
                              (kill-emacs 1)))))
                    (progn
                      (message "✗ FAIL: Content view buffer was not created")
                      (kill-emacs 1)))))
            (progn
              (message "✗ FAIL: Could not find apples link in search buffer")
              (message "Buffer content:")
              (message "%s" (buffer-substring-no-properties (point-min) (point-max)))
              (kill-emacs 1))))
      (progn
        (message "✗ FAIL: No skg search buffer found")
        (kill-emacs 1)))))

(defun integration-test-title-matches ()
  "Main orchestrator function."
  (message "=== SKG Title Matches Integration Test ===")

  ;; Set the port from environment variable if available
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))
      (message "Using test port: %d" skg-port)))

  ;; Wait a moment for server to be fully ready
  (sleep-for 0.25)

  (test-title-search)
  (test-visit-link)

  (message "✓ PASS: Integration test successful!")
  (setq integration-test-completed t)
  (kill-emacs 0))

(progn ;; Run the test with a timeout in case things hang.
  (run-at-time
   15 nil
   (lambda ()
     (message "TIMEOUT: Integration test timed out!")
     (message "Last phase: %s" integration-test-phase)
     (kill-emacs 1)))
  ( ;; the test
   integration-test-title-matches))
