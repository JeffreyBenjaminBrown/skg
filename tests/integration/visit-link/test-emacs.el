;;; Integration test for skg-visit-link traversal
;;; Tests following links: title-search → src → dest

;; Load the project elisp configuration
(load-file "../../../elisp/skg-init.el")
(load-file "../../../elisp/skg-test-utils.el")

;; Test result tracking
(defvar integration-test-phase "starting")
(defvar integration-test-completed nil)

;;; Utility functions

(defun test-fail (message &rest args)
  "Report test failure and exit."
  (apply #'message (concat "✗ FAIL: " message) args)
  (kill-emacs 1))

(defun test-pass (message &rest args)
  "Report test success."
  (apply #'message (concat "✓ PASS: " message) args))

(defun goto-link-center ()
  "Move point to first '][' on current line (inside a link).
This positions point where skg-visit-link can find the link."
  (beginning-of-line)
  (if (search-forward "][" (line-end-position) t)
      (progn
        (goto-char (match-beginning 0))
        t)
    nil))

(defun wait-for-buffer (buffer-name pattern &optional timeout-seconds)
  "Wait for a buffer named BUFFER-NAME to exist and contain PATTERN.
Returns t if found, nil if timeout. TIMEOUT-SECONDS defaults to 5."
  (let ((timeout (or timeout-seconds 5))
        (start-time (float-time))
        (found nil))
    (while (and (not found)
                (< (- (float-time) start-time) timeout))
      ;; Process pending network input
      (accept-process-output nil 0.1)
      ;; Check if buffer exists and has the pattern
      (let ((buf (get-buffer buffer-name)))
        (when buf
          (with-current-buffer buf
            (let ((content (buffer-substring-no-properties (point-min) (point-max))))
              (when (and (> (length content) 0)
                         (string-match-p pattern content))
                (setq found t)))))))
    found))

(defun wait-for-content-view (pattern &optional timeout-seconds)
  "Wait for a content view buffer to exist and contain PATTERN.
Returns t if found, nil if timeout. TIMEOUT-SECONDS defaults to 5."
  (let ((timeout (or timeout-seconds 5))
        (start-time (float-time))
        (found nil))
    (while (and (not found)
                (< (- (float-time) start-time) timeout))
      (accept-process-output nil 0.1)
      (let ((buf (find-skg-content-buffer)))
        (when buf
          (with-current-buffer buf
            (let ((content (buffer-substring-no-properties (point-min) (point-max))))
              (when (and (> (length content) 0)
                         (string-match-p pattern content))
                (setq found t)))))))
    found))

;;; Test phases

(defun phase-0-search ()
  "Search for 'has' and wait for results."
  (message "=== PHASE 0: Searching for 'has' ===")
  (skg-request-title-matches "has")
  (message "Sent title-matches request")

  ;; Wait for search results buffer to contain the expected link
  (unless (wait-for-buffer (skg-search-buffer-name "has") "\\[\\[id:src\\]" 5)
    (test-fail "Timeout waiting for search results")))

(defun phase-1-visit-src ()
  "From search results, visit src."
  (message "=== PHASE 1: Visiting src from search results ===")

  (let ((search-buffer (get-buffer (skg-search-buffer-name "has"))))
    (if search-buffer
        (with-current-buffer search-buffer
          (let ((content (buffer-substring-no-properties (point-min) (point-max))))
            (message "Search results: %s" content)

            ;; Verify search found src
            (unless (string-match-p "\\[\\[id:src\\]" content)
              (test-fail "Expected src link in search results, got: %s" content))

            (test-pass "Found src in search results")

            ;; Unfold headline (TAB in org-mode cycles visibility)
            (goto-char (point-min))
            (when (re-search-forward "^\\*" nil t)
              (org-cycle))

            ;; Find and position on the src link
            (goto-char (point-min))
            (if (search-forward "[[id:src]" nil t)
                (progn
                  (goto-char (match-beginning 0))
                  ;; Now find '][' and position there
                  (if (goto-link-center)
                      (progn
                        (message "Line: %s"
                                 (buffer-substring-no-properties
                                  (line-beginning-position) (line-end-position)))
                        (message "Positioned on src link, calling skg-visit-link...")
                        (skg-visit-link)

                        ;; Wait for content view buffer to show src's content
                        (unless (wait-for-content-view "\\[\\[id:dest\\]" 5)
                          (test-fail "Timeout waiting for src content view")))
                    (test-fail "Could not find '][' in src link")))
              (test-fail "Could not find [[id:src] link"))))
      (test-fail "Search buffer not found"))))

(defun phase-2-visit-dest ()
  "From src's content view, follow link to dest."
  (message "=== PHASE 2: Following link from src to dest ===")

  (let ((content-buffer (find-skg-content-buffer)))
    (if content-buffer
        (with-current-buffer content-buffer
          (let ((content (buffer-substring-no-properties (point-min) (point-max))))
            (message "Src content view: %s" content)

            ;; Verify we're at src
            (unless (string-match-p "\\[\\[id:dest\\]" content)
              (test-fail "Expected src with link to dest, got: %s" content))

            (test-pass "At src content view with link to dest")

            ;; Find and position on the dest link
            (goto-char (point-min))
            (if (search-forward "[[id:dest]" nil t)
                (progn
                  (goto-char (match-beginning 0))
                  ;; Now find '][' and position there
                  (if (goto-link-center)
                      (progn
                        (message "Positioned on link, calling skg-visit-link...")
                        (skg-visit-link)

                        ;; Wait for content view buffer to show dest's content
                        (unless (wait-for-content-view "Dest need not be aware" 5)
                          (test-fail "Timeout waiting for dest content view")))
                    (test-fail "Could not find '][' in dest link")))
              (test-fail "Could not find [[id:dest] link in src"))))
      (test-fail "Content view buffer not found after visiting src"))))

(defun phase-3-verify-dest ()
  "Verify we arrived at dest's content view."
  (message "=== PHASE 3: Verifying dest content view ===")

  (let ((content-buffer (find-skg-content-buffer)))
    (if content-buffer
        (with-current-buffer content-buffer
          (let ((content (buffer-substring-no-properties (point-min) (point-max))))
            (message "Content view content: %s" content)
            (if (string-match-p "Dest need not be aware" content)
                (progn
                  (test-pass "Arrived at dest content view!")
                  (test-pass "Link traversal test complete!"))
              (test-fail "Expected dest content, got: %s" content))))
      (test-fail "Content view buffer not found after visiting dest"))))

(defun integration-test-main ()
  "Main test: search for 'has', then traverse links src → dest."
  (message "=== SKG Visit-Link Traversal Integration Test ===")

  ;; Set port from environment
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))
      (message "Using test port: %d" skg-port)))

  ;; Wait for server
  (sit-for 0.25)

  (phase-0-search)
  (phase-1-visit-src)
  (phase-2-visit-dest)
  (phase-3-verify-dest)

  (setq integration-test-completed t)
  (kill-emacs 0))

;;; Run test with timeout

(progn
  (run-at-time
   15 nil
   (lambda ()
     (message "TIMEOUT: Integration test timed out!")
     (message "Last phase: %s" integration-test-phase)
     (kill-emacs 1)))
  (integration-test-main))
