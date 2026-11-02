;;; test-skg-extend-diff-view.el --- Extend-diff-view client tests -*- lexical-binding: t; -*-

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'cl-lib)
(require 'ert)
(require 'org)
(require 'skg-buffer)
(require 'skg-request-extend-diff-view)

(defun test--extend-diff-view-run (body)
  "Run BODY with the network faked. BODY receives a function giving
the sent requests (newest first) and the handlers."
  (let ((sent nil) (handlers (make-hash-table :test 'eq)))
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust) (lambda () 'fake-proc))
              ((symbol-function 'process-send-string)
               (lambda (_proc text) (push text sent)))
              ((symbol-function 'skg--begin-stream) #'ignore)
              ((symbol-function 'skg--lock-all-skg-buffers) #'ignore)
              ((symbol-function 'skg--register-rerender-stream-handlers) #'ignore)
              ((symbol-function 'skg-lp-reset) #'ignore)
              ((symbol-function 'skg-register-response-handler)
               (lambda (type handler _one-shot) (puthash type handler handlers))))
      (funcall body (lambda () sent) handlers))))

(ert-deftest test-extend-diff-view-refuses-a-modified-buffer ()
  (with-temp-buffer
    (skg-content-view-mode)
    (setq-local skg-view-id "diff-view: all changes")
    (insert "* (skg node (gnid a)) a\n")
    (set-buffer-modified-p t)
    (test--extend-diff-view-run
     (lambda (sent _handlers)
       (let ((err (should-error (skg-diffView-extend)
                                :type 'user-error)))
         (should (equal (cadr err) "Save the buffer first.")))
       (should (null (funcall sent)))))))

(ert-deftest test-extend-diff-view-sends-the-request-for-a-clean-buffer ()
  (with-temp-buffer
    (skg-content-view-mode)
    (setq-local skg-view-id "diff-view: all changes")
    (insert "* (skg node (gnid a)) a\n")
    (set-buffer-modified-p nil)
    (test--extend-diff-view-run
     (lambda (sent handlers)
       (skg-diffView-extend)
       (should (= 1 (length (funcall sent))))
       (should (string-match-p
                (regexp-quote "(request . \"extend diff view\")")
                (car (funcall sent))))
       (should (string-match-p
                (regexp-quote "(view-id . \"diff-view: all changes\")")
                (car (funcall sent))))
       (should (gethash 'extend-diff-view handlers))))))

(ert-deftest test-extend-diff-view-shows-the-summary ()
  (let ((shown nil))
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq shown (apply #'format format-string args)))))
      (skg--extend-diff-view-handler
       "((readiness-summary (ready 1) (changed 3) (foreign 0) (order-only 0) (notEditable 0)) (errors ()) (warnings ()))")
      (should (equal shown "1 of 3 changed files are ready"))
      (skg--extend-diff-view-handler
       "((errors (\"View x is not a buffer of the diff view.\")) (warnings ()))")
      (should (equal shown "View x is not a buffer of the diff view.")))))
