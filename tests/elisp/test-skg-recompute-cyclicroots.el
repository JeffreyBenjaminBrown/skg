;;; test-skg-recompute-cyclicroots.el --- recompute command wire identity -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "../../elisp" default-directory))
(require 'skg-request-recompute-cyclicroots)

(ert-deftest test-skg-recompute-cyclicroots-wire-has-operation-and-session ()
  (let ((skg--server-session-id "11111111-2222-4333-8444-555555555555")
        wires)
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust)
               (lambda () 'process))
              ((symbol-function 'skg-connection-handshake-ensure)
               (lambda () t))
              ((symbol-function 'skg-register-response-handler)
               (lambda (&rest _) nil))
              ((symbol-function 'skg-submit-request)
               (lambda (_tcp wire) (push wire wires))))
      (skg-recompute-cyclicroots)
      (skg-recompute-cyclicroots))
    (let* ((first (read (car wires)))
           (second (read (cadr wires)))
           (first-id (cdr (assq 'operation-id first)))
           (second-id (cdr (assq 'operation-id second))))
      (should (equal "recompute cyclic roots" (cdr (assq 'request first))))
      (should (equal skg--server-session-id
                     (cdr (assq 'server-session-id first))))
      (should (string-match-p
               "\\`[[:xdigit:]]\\{8\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{12\\}\\'"
               first-id))
      (should (string-match-p
               "\\`[[:xdigit:]]\\{8\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{12\\}\\'"
               second-id))
      (should-not (equal first-id second-id)))))

(ert-deftest test-skg-recompute-cyclicroots-refuses-before-verification ()
  (let (submitted registered)
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust)
               (lambda () 'process))
              ((symbol-function 'skg-connection-handshake-ensure)
               (lambda () nil))
              ((symbol-function 'skg-register-response-handler)
               (lambda (&rest _) (setq registered t)))
              ((symbol-function 'skg-submit-request)
               (lambda (&rest _) (setq submitted t))))
      (should-error (skg-recompute-cyclicroots))
      (should-not registered)
      (should-not submitted))))
