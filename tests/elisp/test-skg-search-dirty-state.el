;;; test-skg-search-dirty-state.el --- Search enrichment baselines

(require 'ert)
(require 'skg-client)
(require 'skg-request-text-search)

(ert-deftest test-skg-dirty-search-enrichment-preserves-baseline-and-dirty-state ()
  "Enrichment of edited results keeps the original clean baseline."
  (let ((buffer (get-buffer-create (skg-search-buffer-name "dirty-test"))))
    (unwind-protect
        (with-current-buffer buffer
          (skg-content-view-mode)
          (setq skg-view-uri "search:dirty-test")
          (skg--replace-search-content "* clean")
          (let ((baseline skg-clean-baseline))
            (goto-char (point-max))
            (insert "user edit\n")
            (setq skg--search-snapshot-was-dirty t)
            (skg--display-search-enrichment
             "((terms \"dirty-test\") (content \"* enriched\\nuser edit\") (warnings ()))")
            (should (buffer-modified-p))
            (should (equal baseline skg-clean-baseline))
            (should skg--search-enrichment-includes-user-edits)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(ert-deftest test-skg-clean-search-enrichment-establishes-new-baseline ()
  "Enrichment of clean results records the enriched normalized text."
  (let ((buffer (get-buffer-create (skg-search-buffer-name "clean-test"))))
    (unwind-protect
        (with-current-buffer buffer
          (skg-content-view-mode)
          (setq skg-view-uri "search:clean-test")
          (skg--replace-search-content "* clean")
          (setq skg--search-snapshot-was-dirty nil)
          (skg--display-search-enrichment
           "((terms \"clean-test\") (content \"* enriched\") (warnings ()))")
          (should-not (buffer-modified-p))
          (should (equal "* enriched\n" skg-clean-baseline)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(provide 'test-skg-search-dirty-state)
