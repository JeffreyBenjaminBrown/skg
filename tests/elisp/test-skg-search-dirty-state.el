;;; test-skg-search-dirty-state.el --- Search enrichment baselines

(require 'ert)
(require 'skg-client)
(require 'skg-request-text-search)

(ert-deftest test-skg-local-search-send-failure-unwinds-stream-and-handlers ()
  (let ((skg-response-handler-map nil)
        (skg-lp--pending-count 0)
        (skg--stream-in-progress nil))
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust)
               (lambda () 'fake-process))
              ((symbol-function 'process-send-string)
               (lambda (&rest _) (error "synthetic send failure"))))
      (should-error
       (skg--request-text-search "fails" nil nil nil)
       :type 'error))
    (should-not skg--stream-in-progress)
    (should (= skg-lp--pending-count 0))
    (dolist (response-type
             '(search-results search-enrichment request-snapshot
               overPrivateText-telescope-confirmation))
      (should-not (assoc response-type skg-response-handler-map)))))

(ert-deftest test-skg-stream-guard-blocks-search-and-save-in-both-orders ()
  (let ((skg--stream-in-progress "save"))
    (should-error
     (skg--request-text-search "blocked" nil nil nil)
     :type 'error)
    (should (equal skg--stream-in-progress "save")))
  (let ((skg--stream-in-progress "search enrichment")
        (buffer (generate-new-buffer " *skg blocked save*")))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local skg-view-uri "blocked-uri")
          (should-error (skg-request-save-buffer) :type 'error)
          (should-not skg--save-lock-overlay))
      (kill-buffer buffer))))

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
