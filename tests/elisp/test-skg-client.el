;;; test-skg-client.el --- Tests for SKG client connection behavior.

(defconst test-skg-client--this-dir
  (file-name-directory load-file-name))

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             test-skg-client--this-dir))
(require 'ert)
(load-file (expand-file-name "../../elisp/skg-init.el"
                             test-skg-client--this-dir))

(ert-deftest test-skg-connect-failure-points-to-startup-logs ()
  (let* ((tmp-dir (make-temp-file "skg-client-test" t))
         (skg-config-dir (file-name-as-directory tmp-dir))
         (skg-port 1731)
         (skg-rust-tcp-proc nil))
    (cl-letf (((symbol-function 'make-network-process)
               (lambda (&rest _args)
                 (error "connection refused"))))
      (let ((err
             (should-error (skg-tcp-connect-to-rust)
                           :type 'user-error)))
        (should (string-match-p
                 "Could not connect to the SKG server on port 1731"
                 (error-message-string err)))
        (should (string-match-p
                 (regexp-quote
                  (expand-file-name "logs/server-to-user.log"
                                    skg-config-dir))
                 (error-message-string err)))
        (should (string-match-p
                 (regexp-quote
                  (expand-file-name "logs/cargo-watch.log"
                                    skg-config-dir))
                 (error-message-string err)))
        (should (string-match-p
                 "connection refused"
                 (error-message-string err)))))))

(ert-deftest test-skg-connection-verify-defers-to-new-connection-handshake ()
  "A fresh connection owns the handshake; verification does not send another."
  (let ((skg-rust-tcp-proc nil)
        registered
        submitted)
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust)
               (lambda () 'new-process))
              ((symbol-function 'skg-register-response-handler)
               (lambda (&rest _args) (setq registered t)))
              ((symbol-function 'skg-submit-request)
               (lambda (&rest _args) (setq submitted t))))
      (should-not (skg-connection-verify))
      (should-not registered)
      (should-not submitted))))

(ert-deftest test-skg-first-handshake-initializes-only-unbound-new-empty-view ()
  "The first handshake supplies authority absent at offline construction."
  (let ((new-empty (generate-new-buffer " *skg-unbound-new-empty*"))
        (old-new-empty (generate-new-buffer " *skg-old-new-empty*"))
        (content (generate-new-buffer " *skg-unbound-content*"))
        (skg--buffer-registry (make-hash-table :test #'equal))
        (skg--server-store-state nil)
        (skg--active-source-set-name "server-default"))
    (unwind-protect
        (progn
          (skg-register-buffer
           new-empty 'new-empty-content-view
           :lifecycle 'live-view :disposable nil :view-uri "new"
           :recipe '((kind . "new-empty")) :last-fetched "")
          (skg-register-buffer
           old-new-empty 'new-empty-content-view
           :lifecycle 'live-view :disposable nil :view-uri "old"
           :recipe '((kind . "new-empty")) :last-fetched ""
           :graph-generation 3)
          (skg-register-buffer
           content 'content-view
           :lifecycle 'live-view :disposable nil :view-uri "content"
           :recipe '((kind . "single-root") (root-id . "root"))
           :root-ids '("root") :last-fetched "")
          (skg-adopt-unbound-new-empty-authority 7 "all")
          (with-current-buffer new-empty
            (should (= 7 (skg--buffer-record-graph-generation
                          skg--buffer-record)))
            (should (equal "all" (skg--buffer-record-source-set
                                  skg--buffer-record))))
          (with-current-buffer old-new-empty
            (should (= 3 (skg--buffer-record-graph-generation
                          skg--buffer-record)))
            (should (equal "server-default"
                           (skg--buffer-record-source-set
                            skg--buffer-record))))
          (with-current-buffer content
            (should-not (skg--buffer-record-graph-generation
                         skg--buffer-record))))
      (kill-buffer new-empty)
      (kill-buffer old-new-empty)
      (kill-buffer content))))

(provide 'test-skg-client)
