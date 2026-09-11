;;; test-skg-delete-references-to-absent-node.el --- Cleanup-command client tests

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'cl-lib)
(require 'ert)
(require 'org)
(require 'skg-buffer)
(require 'skg-request-delete-references-to-absent-node)

(defun test--with-unknown-view (body)
  "Run BODY at a PhantomUnknown headline in a temporary SKG view."
  (with-temp-buffer
    (insert "* (skg (node (id owner) (source main))) owner\n"
            "** (skg (unknown (id gone)))\n")
    (skg-content-view-mode)
    (setq-local skg-view-uri "test-unknown-view")
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (forward-line 1)
    (funcall body)))

(ert-deftest test-delete-absent-references-requires-an-unknown-and-clean-views ()
  (with-temp-buffer
    (insert "* (skg (node (id owner) (source main))) owner\n")
    (skg-content-view-mode)
    (should-error (skg-delete-references-to-absent-node) :type 'user-error))
  (test--with-unknown-view
   (lambda ()
     (set-buffer-modified-p t)
     (let ((err (should-error (skg-delete-references-to-absent-node)
                              :type 'user-error)))
       (should (string-match-p "Save or revert" (cadr err)))))))

(ert-deftest test-delete-absent-references-retries-with-server-token ()
  (let ((sent nil) (handlers (make-hash-table :test 'eq))
        (skg--rerender-after-empty-stream nil))
    (unwind-protect
        (test--with-unknown-view
         (lambda ()
           (cl-letf (((symbol-function 'skg-tcp-connect-to-rust)
                      (lambda () 'fake-proc))
                     ((symbol-function 'process-send-string)
                      (lambda (_proc text) (push text sent)))
                     ((symbol-function 'skg--begin-stream) #'ignore)
                     ((symbol-function 'skg--lock-all-skg-buffers) #'ignore)
                     ((symbol-function 'skg--register-rerender-stream-handlers) #'ignore)
                     ((symbol-function 'skg--delete-absent-wrap-rerender-done) #'ignore)
                     ((symbol-function 'skg-lp-reset) #'ignore)
                     ((symbol-function 'skg-register-response-handler)
                      (lambda (type handler _one-shot)
                        (puthash type handler handlers)))
                     ((symbol-function 'yes-or-no-p) (lambda (_prompt) t))
                     ((symbol-function 'pop-to-buffer) (lambda (&rest _) nil)))
             (skg-delete-references-to-absent-node)
             (should (string-match-p
                      (regexp-quote "(request . \"delete references to absent node\")")
                      (car sent)))
             (should (string-match-p (regexp-quote "(id . \"gone\")") (car sent)))
             (funcall (gethash 'delete-references-confirmation handlers)
                      'fake-proc
                      "((approved-preview \"opaque-token\") (content \"* Warning\"))")
             (should skg--rerender-after-empty-stream)
             (funcall skg--rerender-after-empty-stream)
             (should (= 2 (length sent)))
             (should (string-match-p (regexp-quote "(approved-preview . \"opaque-token\")")
                                     (car sent))))))
      (let ((buffer (get-buffer "*skg absent-reference warning*")))
        (when buffer (kill-buffer buffer))))))

(ert-deftest test-delete-absent-references-shows-result-and-error ()
  (let ((handlers (make-hash-table :test 'eq)) (shown nil) (reported nil))
    (test--with-unknown-view
     (lambda ()
       (cl-letf (((symbol-function 'skg-tcp-connect-to-rust) (lambda () 'fake-proc))
                 ((symbol-function 'process-send-string) #'ignore)
                 ((symbol-function 'skg--begin-stream) #'ignore)
                 ((symbol-function 'skg--lock-all-skg-buffers) #'ignore)
                 ((symbol-function 'skg--register-rerender-stream-handlers) #'ignore)
                 ((symbol-function 'skg--delete-absent-wrap-rerender-done) #'ignore)
                 ((symbol-function 'skg-lp-reset) #'ignore)
                 ((symbol-function 'skg-register-response-handler)
                  (lambda (type handler _one-shot)
                    (puthash type handler handlers)))
                 ((symbol-function 'skg-big-nonfatal-message)
                  (lambda (&rest args) (setq shown args)))
                 ((symbol-function 'message)
                  (lambda (&rest args) (setq reported args))))
         (skg-delete-references-to-absent-node)
         (funcall (gethash 'delete-references-result handlers) 'fake-proc
                  "((content \"* Finished\"))")
         (should (equal (car (last shown)) "* Finished"))
         (funcall (gethash 'error handlers) 'fake-proc
                  "((content \"stale preview\"))")
         (should (string-match-p "stale preview" (apply #'format reported))))))))
