;;; Regression test: an ordinary visit to overridden Z opens Z raw.
;;; It must not inject overrider R as an independent sibling. Z and R
;;; deliberately share the title "cooking", come from different
;;; sources, and subscribe to each other.

;; Load the project elisp configuration
(load-file "../../../elisp/skg-init.el")
(load-file "../test-wait.el")

(defun test-fail (message &rest args)
  "Report test failure and exit."
  (apply #'message (concat "✗ FAIL: " message) args)
  (kill-emacs 1))

(defun integration-test-overridden-direct-visit ()
  "Keep same-title raw views distinct, then revisit Z."
  (message "Starting overridden direct-visit integration test...")
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))
      (message "Using test port: %d" skg-port)))
  (let (public-buf cheese-buf reopened-public)
    (skg-request-single-root-content-view-from-id "Z")
    (setq public-buf
          (skg-test-wait-for
           (lambda ()
             (seq-find
              (lambda (buf)
                (with-current-buffer buf
                  (and (boundp 'skg-view-uri) skg-view-uri
                       (equal skg-contentView-initialRoot-source
                              "public"))))
              (buffer-list)))
           10))
    (unless public-buf
      (test-fail "raw public view of Z did not open"))
    (with-current-buffer public-buf
      (let ((content (buffer-substring-no-properties
                      (point-min) (point-max))))
        (unless (string-match-p (regexp-quote "(id Z)") content)
          (test-fail "raw view lacks requested root Z:\n%s" content))
        (when (string-match-p
               "^\\*\\* (skg (node (id R).*affectsParent false"
               content)
          (test-fail
           "ordinary visit injected overrider R as an independent sibling:\n%s"
           content))))
    (when (string-prefix-p
           "override-menu:"
           (buffer-local-value 'skg-view-uri public-buf))
      (test-fail "ordinary visit was registered as an override menu"))

    ;; Opening same-titled R must not overwrite Z's raw view.
    (skg-request-single-root-content-view-from-id "R")
    (setq cheese-buf
          (skg-test-wait-for
           (lambda ()
             (seq-find
              (lambda (buf)
                (with-current-buffer buf
                  (and (boundp 'skg-view-uri) skg-view-uri
                       (equal skg-contentView-initialRoot-source
                              "Cheese"))))
              (buffer-list)))
           10))
    (unless cheese-buf
      (test-fail "same-titled Cheese view did not open"))
    (unless (equal (buffer-name public-buf) "*cooking* <public>")
      (test-fail "unexpected public buffer name: %s" (buffer-name public-buf)))
    (unless (equal (buffer-name cheese-buf) "*cooking* <Cheese>")
      (test-fail "unexpected Cheese buffer name: %s" (buffer-name cheese-buf)))
    (when (string-prefix-p
           "override-menu:"
           (buffer-local-value 'skg-view-uri public-buf))
      (test-fail "opening R changed Z's raw-view URI"))

    ;; A repeat visit receives switch-to-view and displays the old raw view.
    (skg-request-single-root-content-view-from-id "Z")
    (unless (skg-test-wait-for
             (lambda ()
               (eq (window-buffer (selected-window)) public-buf))
             10)
      (test-fail "revisiting Z did not display its existing raw buffer"))
    (message "✓ same-title views remained distinct and Z was revisited")

    ;; Killing both views must close both server registrations.
    (kill-buffer public-buf)
    (kill-buffer cheese-buf)
    (skg-request-single-root-content-view-from-id "Z")
    (setq reopened-public
          (skg-test-wait-for
           (lambda ()
             (seq-find
              (lambda (buf)
                (with-current-buffer buf
                  (and (boundp 'skg-view-uri) skg-view-uri
                       (equal skg-contentView-initialRoot-source
                              "public"))))
              (buffer-list)))
           10))
    (unless (and reopened-public (not (eq reopened-public public-buf)))
      (test-fail "Z's raw view did not reopen after both views were closed"))
    (kill-buffer reopened-public)
    (message "✓ close-view lifecycle allowed the raw view to reopen"))
  (message "PASS: Integration test successful!")
  (kill-emacs 0))

;; Set a timeout in case things hang
(run-at-time 30 nil (lambda ()
                      (message "TIMEOUT: Integration test timed out!")
                      (kill-emacs 1)))

;; Run the test
(integration-test-overridden-direct-visit)
