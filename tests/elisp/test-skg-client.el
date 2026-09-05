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

(ert-deftest test-skg-init-does-not-call-a-stalled-connection-success ()
  "A retained half-dead process must not turn a herald timeout into success."
  (let* ((tmp-dir (make-temp-file "skg-client-test" t))
         (config-file (expand-file-name "skgconfig.toml" tmp-dir))
         (skg-port 1731)
         (skg-config-dir nil)
         (skg-rust-tcp-proc 'stalled-process)
         (skg--connection-handshake-state 'sent)
         (skg--connection-handshake-error nil)
         herald-requested
         (heralds--transform-rules '(skg stale)))
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust)
               (lambda () skg-rust-tcp-proc))
              ((symbol-function 'skg-port-from-toml)
               (lambda (_file) 1731))
              ((symbol-function 'skg-connection-handshake-ensure)
               (lambda () nil))
              ((symbol-function 'skg-herald-rules-ensure)
               (lambda () (setq herald-requested t)))
              ((symbol-function 'process-live-p)
               (lambda (_process) t)))
      (let* ((error-data
              (should-error (skg-client-init config-file)
                            :type 'user-error))
             (text (error-message-string error-data)))
        (should (string-match-p "SKG client initialization failed" text))
        (should (string-match-p "did not complete its handshake" text))
        (should (string-match-p "The client is not ready" text))
        (should-not herald-requested)
        (should-not (string-match-p "skg connected" text))
        (should-not (string-match-p "came back EMPTY" text))))))

(ert-deftest test-skg-init-reports-a-connection-lost-during-startup ()
  (let* ((tmp-dir (make-temp-file "skg-client-test" t))
         (config-file (expand-file-name "skgconfig.toml" tmp-dir))
         (skg-port 1731)
         (skg-config-dir nil)
         (skg-rust-tcp-proc 'closed-process)
         (skg--connection-handshake-state nil)
         (skg--connection-handshake-error nil)
         (heralds--transform-rules '(skg prior-rules)))
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust)
               (lambda () skg-rust-tcp-proc))
              ((symbol-function 'skg-port-from-toml)
               (lambda (_file) 1731))
              ((symbol-function 'skg-connection-handshake-ensure)
               (lambda () nil))
              ((symbol-function 'skg-herald-rules-ensure)
               (lambda () nil))
              ((symbol-function 'process-live-p)
               (lambda (_process) nil)))
      (let ((text (error-message-string
                   (should-error (skg-client-init config-file)
                                 :type 'user-error))))
        (should (string-match-p
                 "server connection closed before initialization completed"
                 text))
        (should (string-match-p "server-to-user.log" text))
        (should (string-match-p "cargo-watch.log" text))))))

(ert-deftest test-skg-init-reports-a-verified-invalid-herald-response ()
  (let* ((tmp-dir (make-temp-file "skg-client-test" t))
         (config-file (expand-file-name "skgconfig.toml" tmp-dir))
         (skg-port 1731)
         (skg-config-dir nil)
         (skg-rust-tcp-proc 'live-process)
         (skg--connection-handshake-state 'verified)
         (skg--connection-handshake-error nil)
         (heralds--transform-rules '(skg prior-rules)))
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust)
               (lambda () skg-rust-tcp-proc))
              ((symbol-function 'skg-port-from-toml)
               (lambda (_file) 1731))
              ((symbol-function 'skg-connection-handshake-ensure)
               (lambda () t))
              ((symbol-function 'skg-herald-rules-ensure)
               (lambda () nil))
              ((symbol-function 'process-live-p)
               (lambda (_process) t)))
      (let ((text (error-message-string
                   (should-error (skg-client-init config-file)
                                 :type 'user-error))))
        (should (string-match-p "server handshake completed" text))
        (should (string-match-p "no valid herald rule table" text))))))

(ert-deftest test-skg-init-says-ready-only-after-rules-arrive ()
  (let* ((tmp-dir (make-temp-file "skg-client-test" t))
         (config-file (expand-file-name "skgconfig.toml" tmp-dir))
         (skg-port 1731)
         (skg-config-dir nil)
         (skg-rust-tcp-proc 'live-process)
         (skg--connection-handshake-state 'verified)
         (skg--connection-handshake-error nil)
         (heralds--transform-rules '(skg prior-rules)))
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust)
               (lambda () skg-rust-tcp-proc))
              ((symbol-function 'skg-port-from-toml)
               (lambda (_file) 1731))
              ((symbol-function 'skg-connection-handshake-ensure)
               (lambda () t))
              ((symbol-function 'skg-herald-rules-ensure)
               (lambda () '(skg first-rule second-rule))))
      (should (equal (skg-client-init config-file)
                     "skg ready on port 1731 -- 2 herald rules loaded.")))))

(ert-deftest test-skg-init-surfaces-the-exact-census-rejection-once ()
  (let* ((tmp-dir (make-temp-file "skg-client-test" t))
         (config-file (expand-file-name "skgconfig.toml" tmp-dir))
         (skg-port 1731)
         (skg-config-dir nil)
         (skg-rust-tcp-proc 'live-process)
         (skg--connection-handshake-state 'failed)
         (skg--connection-handshake-error
          "a replacement editor cannot adopt maintenance before archive-ready")
         herald-requested
         (heralds--transform-rules '(skg prior-rules)))
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust)
               (lambda () skg-rust-tcp-proc))
              ((symbol-function 'skg-port-from-toml)
               (lambda (_file) 1731))
              ((symbol-function 'skg-connection-handshake-ensure)
               (lambda () nil))
              ((symbol-function 'skg-herald-rules-ensure)
               (lambda () (setq herald-requested t))))
      (let ((text (error-message-string
                   (should-error (skg-client-init config-file)
                                 :type 'user-error))))
        (should (string-match-p
                 "replacement editor cannot adopt maintenance" text))
        (should-not herald-requested)))))

(ert-deftest test-skg-handshake-wait-stops-at-complete-census ()
  (let ((skg--connection-handshake-state 'census)
        (skg--connection-handshake-error nil)
        (skg-rust-tcp-proc 'live-process)
        accepted)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_process) t))
              ((symbol-function 'accept-process-output)
               (lambda (_process _seconds)
                 (setq accepted t
                       skg--connection-handshake-state 'verified))))
      (should (skg-connection-handshake-ensure))
      (should accepted))))

(ert-deftest test-skg-handshake-records-the-server-census-error-exactly ()
  (let ((skg--connection-handshake-state 'census)
        (skg--connection-handshake-error nil))
    (skg--record-connection-handshake-error
     nil
     "((content \"replacement census was refused\") (terminal-status failed))")
    (should (eq skg--connection-handshake-state 'failed))
    (should (equal skg--connection-handshake-error
                   "replacement census was refused"))))

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
