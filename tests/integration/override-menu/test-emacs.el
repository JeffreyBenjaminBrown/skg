;;; Integration test for the override-choice menu fetch path.
;;; Visiting overridden Z yields the menu buffer (registered under
;;; the server-assigned "override-menu:Z" URI, showing the overrider
;;; R).  Z and R deliberately share the title "cooking", come from
;;; different sources, and subscribe to each other.

;; Load the project elisp configuration
(load-file "../../../elisp/skg-init.el")
(load-file "../test-wait.el")

(defun test-fail (message &rest args)
  "Report test failure and exit."
  (apply #'message (concat "✗ FAIL: " message) args)
  (kill-emacs 1))

(defun integration-test-override-menu ()
  "Keep same-title menu and overrider views distinct, then revisit Z."
  (message "Starting override-menu integration test...")
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))
      (message "Using test port: %d" skg-port)))
  (let (menu-buf cheese-buf reopened-menu)
    (skg-request-single-root-content-view-from-id "Z")
    (setq menu-buf
          (skg-test-wait-for
           (lambda () (skg-find-buffer-by-uri "override-menu:Z")) 10))
    (unless menu-buf
      (test-fail "no buffer with the override-menu:Z URI appeared"))
    (with-current-buffer menu-buf
      (let ((content (buffer-substring-no-properties
                      (point-min) (point-max))))
        (unless (and (string-match-p (regexp-quote "(id Z)") content)
                     (string-match-p (regexp-quote "(id R)") content))
          (test-fail "menu lacks Z or its overrider R:\n%s" content))))

    ;; Opening same-titled R must not overwrite Z's menu.
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
    (unless (equal (buffer-name menu-buf) "*cooking* <public>")
      (test-fail "unexpected public buffer name: %s" (buffer-name menu-buf)))
    (unless (equal (buffer-name cheese-buf) "*cooking* <Cheese>")
      (test-fail "unexpected Cheese buffer name: %s" (buffer-name cheese-buf)))
    (unless (equal (buffer-local-value 'skg-view-uri menu-buf)
                   "override-menu:Z")
      (test-fail "opening R changed Z's menu URI"))

    ;; A repeat visit receives switch-to-view and must display the old menu.
    (skg-request-single-root-content-view-from-id "Z")
    (unless (skg-test-wait-for
             (lambda ()
               (eq (window-buffer (selected-window)) menu-buf))
             10)
      (test-fail "revisiting Z did not display its existing menu buffer"))
    (message "✓ same-title views remained distinct and Z was revisited")

    ;; Killing both views must close both server registrations.
    (kill-buffer menu-buf)
    (kill-buffer cheese-buf)
    (skg-request-single-root-content-view-from-id "Z")
    (setq reopened-menu
          (skg-test-wait-for
           (lambda () (skg-find-buffer-by-uri "override-menu:Z")) 10))
    (unless (and reopened-menu (not (eq reopened-menu menu-buf)))
      (test-fail "Z's menu did not reopen after both views were closed"))
    (kill-buffer reopened-menu)
    (message "✓ close-view lifecycle allowed the menu to reopen"))
  (message "PASS: Integration test successful!")
  (kill-emacs 0))

;; Set a timeout in case things hang
(run-at-time 30 nil (lambda ()
                      (message "TIMEOUT: Integration test timed out!")
                      (kill-emacs 1)))

;; Run the test
(integration-test-override-menu)
