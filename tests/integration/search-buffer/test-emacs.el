;;; Integration test: a search is answered in one message.
;;;
;;; Searches for "bravo" (matches leaf-b). The search buffer, when it
;;; first appears, already shows leaf-b's containers folder, folded,
;;; holding container-a, and leaf-b's node relationship stats.

(load-file "../../../elisp/skg-init.el")
(load-file "../test-wait.el")

(defvar integration-test-phase "starting")

(defun search-root-line (content)
  (let ((text (skg-test-without-viewnode-ids content)))
    (and (string-match "^\\* (skg node (gnid leaf-b) .*$" text)
         (match-string 0 text))))

(defun assert-leaf-b-root-is-content (content)
  (let ((line (search-root-line content)))
    (unless line
      (message "✗ FAIL: leaf-b search root not found")
      (message "Buffer content:\n%s" content)
      (kill-emacs 1))
    (when (string-match-p "noClaim" line)
      (message "✗ FAIL: leaf-b search root is false: %S" line)
      (kill-emacs 1))
    (when (string-match-p "noClaim writeProtected (view (rels (contains (out 1 (ancestors 1)))) (birth (contains out 1)))" line)
      (message "✗ FAIL: leaf-b search root is content: %S" line)
      (kill-emacs 1))
    (when (string-match-p "noClaim writeProtected (view (rels (linksTo (out (ancestors 1)))) (birth (linksTo out 1)))" line)
      (message "✗ FAIL: leaf-b search root is linksToParent: %S" line)
      (kill-emacs 1))))

(defun integration-test-search-buffer ()
  (message "=== SKG Search In One Message Integration Test ===")

  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))
      (message "Using test port: %d" skg-port)))

  (message "=== search for 'bravo' ===")
  (skg--request-text-search "bravo" nil nil nil)
  (setq integration-test-phase "search-sent")

  (let ((buf (skg-test-wait-for-buffer
              (skg-search-buffer-name "bravo"))))
    (unless buf
      (message "✗ FAIL: search buffer never created")
      (kill-emacs 1))
    (setq integration-test-phase "buffer-created")
    (let ((content (with-current-buffer buf
                     (buffer-substring-no-properties
                      (point-min) (point-max)))))
      (assert-leaf-b-root-is-content content)
      (unless (string-match-p "(skgRepos (nodeRepo \"Mr Cheese\"))" content)
        (message "✗ FAIL: spaced repo name was not quoted")
        (kill-emacs 1))
      (unless (string-match-p
               "^\\* (skg node (gnid leaf-b) .*\n\\*\\* (skg folded containerFolder.*\n\\*\\*\\* (skg node (gnid container-a) (skgRepos (nodeRepo \"Mr Cheese\")) writeProtected "
               (skg-test-without-viewnode-ids content))
        (message "✗ FAIL: the first search buffer lacks leaf-b's folded containers folder holding container-a")
        (message "Buffer content:\n%s" content)
        (kill-emacs 1))
      ;; leaf-b has one container, so its line should include
      ;; (rels (contains (in 1))).
      (if (string-match-p "(rels (contains (in 1)))" content)
          (progn
            (message "✓ PASS: the first search buffer holds what the enrichment draws, with node relationship stats")
            (message "Buffer content:\n%s" content)
            (kill-emacs 0))
        (progn
          (message "✗ FAIL: node relationship stats not found in the search buffer")
          (message "Buffer content:\n%s" content)
          (kill-emacs 1))))))

(progn
  (run-at-time
   45 nil
   (lambda ()
     (message "TIMEOUT: test timed out at phase: %s"
              integration-test-phase)
     (kill-emacs 1)))
  (integration-test-search-buffer))
