;;; test-skg-server-warnings.el --- The server's startup warnings, client side -*- lexical-binding: t; -*-

(defconst test-skg-server-warnings--this-dir
  (file-name-directory load-file-name))

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             test-skg-server-warnings--this-dir))
(require 'cl-lib)
(require 'ert)
(load-file (expand-file-name "../../elisp/skg-init.el"
                             test-skg-server-warnings--this-dir))

(defun test-skg-server-warnings--kill-buffer ()
  (when (get-buffer skg-server-warnings-buffer-name)
    (kill-buffer skg-server-warnings-buffer-name)))

(ert-deftest test-skg-a-new-connection-asks-for-the-server-warnings ()
  "The request goes out once, when the connection is made."
  (let* ((server (make-network-process
                  :name "test-skg-warnings-server" :server t
                  :host "127.0.0.1" :service t))
         (skg-port (process-contact server :service))
         (skg-rust-tcp-proc nil)
         (skg-response-handler-map nil)
         (skg-lp--pending-count 0)
         (sent nil))
    (unwind-protect
        (cl-letf (((symbol-function 'process-send-string)
                   (lambda (_proc text) (push text sent))))
          (skg-tcp-connect-to-rust)
          (skg-tcp-connect-to-rust) ; already connected: sends nothing
          (should (equal sent '("((request . \"server warnings\"))\n")))
          (should (assoc 'server-warnings skg-response-handler-map)))
      (when (process-live-p skg-rust-tcp-proc)
        (delete-process skg-rust-tcp-proc))
      (delete-process server))))

(ert-deftest test-skg-nonempty-server-warnings-open-their-buffer ()
  (test-skg-server-warnings--kill-buffer)
  (unwind-protect
      (progn
        (skg--server-warnings-handler
         nil
         (prin1-to-string
          '((response-type server-warnings)
            (warnings ("skgconfig.toml sets 'max_role_tree_depth', a key skg does not know, so it has no effect. Its current spelling is 'max_roleTree_depth'.")))))
        (with-current-buffer skg-server-warnings-buffer-name
          (should (derived-mode-p 'org-mode))
          (should (string-match-p "^\\* skgconfig.toml sets 'max_role_tree_depth'"
                                  (buffer-string)))
          (should (string-match-p "max_roleTree_depth" (buffer-string)))))
    (test-skg-server-warnings--kill-buffer)))

(ert-deftest test-skg-empty-server-warnings-open-nothing ()
  (test-skg-server-warnings--kill-buffer)
  (skg--server-warnings-handler
   nil "((response-type server-warnings) (warnings ()))")
  (should-not (get-buffer skg-server-warnings-buffer-name)))

(provide 'test-skg-server-warnings)
