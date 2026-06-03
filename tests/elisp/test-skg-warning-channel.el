;;; test-skg-warning-channel.el --- Tests for structured error/warning responses.

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'skg-request-single-root-content-view)
(require 'skg-request-rerender-all-views)

(ert-deftest test-content-view-success-with-warnings-opens-content-and-shows-warning ()
  (let ((opened nil)
        (shown nil))
    (cl-letf (((symbol-function 'skg-open-org-buffer-from-text)
               (lambda (_tcp-proc content buffer-name view-uri)
                 (setq opened (list content buffer-name view-uri))))
              ((symbol-function 'skg-big-nonfatal-message)
               (lambda (buffer-name message-text content)
                 (setq shown (list buffer-name message-text content)))))
      (skg-handle-content-view-sexp
       nil
       (prin1-to-string
        '((content "* root\n")
          (errors ())
          (warnings ("audit warning"))))
       "view-1")
      (should (equal (car opened) "* root\n"))
      (should (equal (nth 2 opened) "view-1"))
      (should (equal (car shown) "*SKG Content View Messages*"))
      (should (string-match-p "^\\* warnings\n\\*\\* audit warning"
                              (nth 2 shown))))))

(ert-deftest test-content-view-failure-with-errors-shows-error_without_opening ()
  (let ((opened nil)
        (shown nil))
    (cl-letf (((symbol-function 'skg-open-org-buffer-from-text)
               (lambda (&rest _args)
                 (setq opened t)))
              ((symbol-function 'skg-big-nonfatal-message)
               (lambda (buffer-name message-text content)
                 (setq shown (list buffer-name message-text content)))))
      (skg-handle-content-view-sexp
       nil
       (prin1-to-string
        '((content "")
          (errors ("inactive source"))
          (warnings ())))
       "view-1")
      (should-not opened)
      (should (equal (car shown) "*SKG Content View Messages*"))
      (should (string-match-p "^\\* errors\n\\*\\* inactive source"
                              (nth 2 shown))))))

(ert-deftest test-rerender-done-with-errors-and-warnings-shows-both ()
  (let ((shown nil)
        (ended nil)
        (unlocked nil)
        (skg-response-handler-map '((rerender-view . ignore))))
    (cl-letf (((symbol-function 'skg--end-stream)
               (lambda () (setq ended t)))
              ((symbol-function 'skg--unlock-all-save-locked)
               (lambda () (setq unlocked t)))
              ((symbol-function 'skg-big-nonfatal-message)
               (lambda (buffer-name message-text content)
                 (setq shown (list buffer-name message-text content)))))
      (skg--register-rerender-stream-handlers)
      (let ((handler (cadr (assoc 'rerender-done skg-response-handler-map))))
        (funcall handler
                 nil
                 (prin1-to-string
                  '((errors ("view failed"))
                    (warnings ("audit warning"))))))
      (should ended)
      (should unlocked)
      (should-not (assoc 'rerender-view skg-response-handler-map))
      (should (equal (car shown) "*skg rerender messages*"))
      (should (string-match-p "^\\* errors\n\\*\\* view failed"
                              (nth 2 shown)))
      (should (string-match-p "^\\* warnings\n\\*\\* audit warning"
                              (nth 2 shown))))))

(provide 'test-skg-warning-channel)
