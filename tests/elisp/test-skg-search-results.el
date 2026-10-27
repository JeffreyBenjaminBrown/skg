;;; test-skg-search-results.el --- The search buffer, from one message

(require 'ert)
(require 'skg-client)
(require 'skg-request-text-search)

(ert-deftest test-skg-local-search-send-failure-unwinds-handlers ()
  (let ((skg-response-handler-map nil)
        (skg-lp--pending-count 0))
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust)
               (lambda () 'fake-process))
              ((symbol-function 'process-send-string)
               (lambda (&rest _) (error "synthetic send failure"))))
      (should-error
       (skg--request-text-search "fails" nil nil nil)
       :type 'error))
    (should (= skg-lp--pending-count 0))
    (dolist (response-type
             '(search-results overPrivateText-telescope-confirmation))
      (should-not (assoc response-type skg-response-handler-map)))))

(ert-deftest test-skg-search-leaves-the-stream-in-progress-guard-alone ()
  "A search neither waits for a streaming request nor blocks one."
  (let ((skg-response-handler-map nil)
        (skg-lp--pending-count 0)
        (sent nil)
        (skg--stream-in-progress "save"))
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust)
               (lambda () 'fake-process))
              ((symbol-function 'process-send-string)
               (lambda (_process string) (push string sent))))
      (skg--request-text-search "while saving" nil nil nil)
      (should sent)
      (should (equal skg--stream-in-progress "save"))
      (setq skg--stream-in-progress nil)
      (skg--request-text-search "then" nil nil nil)
      (should-not skg--stream-in-progress))))

(ert-deftest test-skg-search-results-establish-a-clean-baseline ()
  "The one message makes the search buffer, clean, with its baseline."
  (let ((buffer-name (skg-search-buffer-name "clean-test")))
    (unwind-protect
        (progn
          (skg--display-search-results
           "((response-type search-results) (content \"* (skg node (gnid a)) a hit\") (warnings ()))"
           "clean-test")
          (with-current-buffer buffer-name
            (should (equal "search:clean-test" skg-view-id))
            (should-not (buffer-modified-p))
            (should (equal "* (skg node (gnid a)) a hit\n" skg-clean-baseline))))
      (when (get-buffer buffer-name)
        (with-current-buffer buffer-name
          (remove-hook 'kill-buffer-hook #'skg-send-close-view t)
          (set-buffer-modified-p nil))
        (kill-buffer buffer-name)))))

(provide 'test-skg-search-results)
