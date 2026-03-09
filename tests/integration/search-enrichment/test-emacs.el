;;; Integration test: search enrichment (background containerward paths + graphnodestats)
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
  (skg-search-titles-everywhere "bravo")
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
      (unless found
        (message "✗ FAIL: containerward path 'container alpha' not found after 30s")
        (message "Buffer content:\n%s" content)
        (kill-emacs 1))
      (message "✓ containerward path enrichment arrived")
      ;; Phase 3: verify graphnodestats in enriched results.
      ;; container-a is a root with 1 content,
      ;; so its line should include (graphStats (containers 0) (contents 1)).
      (if (string-match-p "graphStats" content)
          (progn
            (message "✓ PASS: graphnodestats present in enriched search results")
            (message "Buffer content:\n%s" content)
            (kill-emacs 0))
        (progn
          (message "✗ FAIL: graphnodestats not found in enriched search results")
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
