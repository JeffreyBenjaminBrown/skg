;;; Integration test: search enrichment (background containerward paths)
;;;
;;; Searches for "bravo" (matches leaf-b).
;;; Phase 1: search results appear immediately (no paths).
;;; Phase 2: after background enrichment, the buffer should contain
;;; the containerward path from leaf-b to container-a
;;; (i.e. "container alpha" appears in the buffer).

(load-file "../../../elisp/skg-init.el")
(load-file "../test-wait.el")

(defvar integration-test-phase "starting")

(defun integration-test-search-enrichment ()
  (message "=== SKG Search Enrichment Integration Test ===")

  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))
      (message "Using test port: %d" skg-port)))

  ;; Phase 1: search and verify results appear
  (message "=== PHASE 1: search for 'bravo' ===")
  (skg-request-title-matches "bravo")
  (setq integration-test-phase "search-sent")

  (let ((buf (skg-test-wait-for-buffer
              (skg-search-buffer-name "bravo"))))
    (unless buf
      (message "✗ FAIL: search buffer never created")
      (kill-emacs 1))
    (message "✓ search buffer created")
    (setq integration-test-phase "phase1-done"))

  ;; Phase 2: wait for enrichment to add "container alpha"
  (message "=== PHASE 2: waiting for containerward path enrichment ===")
  (let ((found (skg-test-wait-for
                (lambda ()
                  (let ((buf (get-buffer
                              (skg-search-buffer-name "bravo"))))
                    (and buf
                         (with-current-buffer buf
                           (let ((content
                                  (buffer-substring-no-properties
                                   (point-min) (point-max))))
                             (string-match-p "container alpha"
                                             content))))))
                30)))
    (let* ((buf (get-buffer (skg-search-buffer-name "bravo")))
           (content (and buf (with-current-buffer buf
                               (buffer-substring-no-properties
                                (point-min) (point-max))))))
      (if found
          (progn
            (message "✓ PASS: containerward path enrichment arrived")
            (message "Buffer content:\n%s" content)
            (kill-emacs 0))
        (progn
          (message "✗ FAIL: containerward path 'container alpha' not found after 30s")
          (message "Buffer content:\n%s" content)
          (kill-emacs 1))))))

(progn
  (run-at-time
   45 nil
   (lambda ()
     (message "TIMEOUT: test timed out at phase: %s"
              integration-test-phase)
     (kill-emacs 1)))
  (integration-test-search-enrichment))
