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

(provide 'test-skg-client)

(defmacro test-skg-client--with-live-connection (port-var &rest body)
  "Connect `skg-rust-tcp-proc' to a throwaway local server around BODY,
binding PORT-VAR to that server's port."
  (declare (indent 1))
  `(let* (( server (make-network-process
                    :name "test-skg-server" :server t
                    :host "127.0.0.1" :service t) )
          ( ,port-var (process-contact server :service) )
          ( skg-rust-tcp-proc (make-network-process
                               :name "test-skg-client"
                               :host "127.0.0.1" :service ,port-var) ))
     (unwind-protect (progn ,@body)
       (when (process-live-p skg-rust-tcp-proc)
         (delete-process skg-rust-tcp-proc))
       (delete-process server))))

(ert-deftest test-skg-init-keeps-a-connection-to-the-same-port ()
  (test-skg-client--with-live-connection port
    (skg--end-connection-to-another-port port)
    (should (process-live-p skg-rust-tcp-proc))))

(ert-deftest test-skg-init-ends-a-connection-to-another-port ()
  "With no skg buffers open, switching ports ends the old connection."
  (test-skg-client--with-live-connection port
    (let (( process skg-rust-tcp-proc ))
      (cl-letf (( (symbol-function 'skg-buffer-p) #'ignore ))
        (skg--end-connection-to-another-port (1+ port)))
      (should-not (process-live-p process))
      (should-not skg-rust-tcp-proc))))

(ert-deftest test-skg-init-refuses-another-port-while-skg-buffers-are-open ()
  "Views from the old server must not be saved into the new one."
  (test-skg-client--with-live-connection port
    (let (( view (generate-new-buffer "*test-port-switch*") ))
      (with-current-buffer view (setq skg-view-id "test-view"))
      (unwind-protect
          (progn
            (should-error (skg--end-connection-to-another-port (1+ port))
                          :type 'user-error)
            (should (process-live-p skg-rust-tcp-proc)))
        (kill-buffer view)))))
