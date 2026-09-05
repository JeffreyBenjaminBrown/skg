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

(provide 'test-skg-client)
