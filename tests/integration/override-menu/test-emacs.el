;;; Integration test for the override-choice menu fetch path.
;;; Visiting overridden Z yields the menu buffer (registered under
;;; the server-assigned "override-menu:Z" URI, showing the overrider
;;; R); visiting Z with override-choice bypass yields the raw node.

;; Load the project elisp configuration
(load-file "../../../elisp/skg-init.el")
(load-file "../test-wait.el")

(defun test-fail (message &rest args)
  "Report test failure and exit."
  (apply #'message (concat "✗ FAIL: " message) args)
  (kill-emacs 1))

(defun integration-test-override-menu ()
  "Visit overridden Z twice: menu, then bypass."
  (message "Starting override-menu integration test...")
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))
      (message "Using test port: %d" skg-port)))
  (progn ;; Visiting overridden Z yields the menu.
    (skg-request-single-root-content-view-from-id "Z")
    (let ((menu-buf (skg-test-wait-for
                     (lambda ()
                       (skg-find-buffer-by-uri "override-menu:Z"))
                     10)))
      (unless menu-buf
        (test-fail "no buffer with the override-menu:Z URI appeared"))
      (with-current-buffer menu-buf
        (let ((content (buffer-substring-no-properties
                        (point-min) (point-max))))
          (unless (string-match-p (regexp-quote "(id Z)") content)
            (test-fail "menu lacks the requested root Z:\n%s" content))
          (unless (string-match-p (regexp-quote "(id R)") content)
            (test-fail "menu lacks the overrider R:\n%s" content))
          (message "✓ menu buffer shows Z with overrider R")))))
  (progn ;; Bypass opens the raw node, as its own buffer.
    (skg-request-single-root-content-view-from-id "Z" nil t)
    (let ((raw-buf (skg-test-wait-for
                    (lambda ()
                      (seq-find
                       (lambda (buf)
                         (with-current-buffer buf
                           (and (boundp 'skg-view-uri)
                                skg-view-uri
                                (not (string-prefix-p
                                      "override-menu:" skg-view-uri))
                                (string-match-p
                                 (regexp-quote "(id Z)")
                                 (buffer-substring-no-properties
                                  (point-min) (point-max))))))
                       (buffer-list)))
                    10)))
      (unless raw-buf
        (test-fail "bypass did not open a raw view of Z"))
      (with-current-buffer raw-buf
        (let ((content (buffer-substring-no-properties
                        (point-min) (point-max))))
          (when (string-match-p (regexp-quote "overriderCol") content)
            ;; fine -- the raw view may show R in the overriderCol
            nil)
          (unless (string-match-p "the overridden node" content)
            (test-fail "raw view lacks Z's title:\n%s" content))
          (message "✓ bypass opened the raw node Z")))))
  (message "PASS: Integration test successful!")
  (kill-emacs 0))

;; Set a timeout in case things hang
(run-at-time 30 nil (lambda ()
                      (message "TIMEOUT: Integration test timed out!")
                      (kill-emacs 1)))

;; Run the test
(integration-test-override-menu)
