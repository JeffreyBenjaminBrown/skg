;;; test-skg-warning-channel.el --- Tests for structured error/warning responses.

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'skg-request-single-root-content-view)
(require 'skg-request-rerender-all-views)
(require 'skg-request-verify-connection)

(ert-deftest test-connection-warning-opens-persistent-telescope-buffer ()
  (let (shown)
    (cl-letf (((symbol-function 'skg-big-nonfatal-message)
               (lambda (name message-text content)
                 (setq shown (list name message-text content)))))
      (skg--show-handshake-telescope-warnings
       '((telescope-warnings
          (((pid X) (kind ignored-foreign-pid-collision)
            (message "owned telescope won")
            (winning-paths (/data/owned/X.skg))
            (ignored-paths (/data/foreign/X.skg)))))))
      (should (equal (car shown) "*SKG Telescope Warnings*"))
      (should (string-match-p "^WARNING:" (cadr shown)))
      (should (string-match-p "retained owned files" (nth 2 shown)))
      (should (string-match-p "/data/foreign/X.skg" (nth 2 shown))))))

(ert-deftest test-content-view-success-with-warnings-opens-content-and-shows-warning ()
  (let ((opened nil)
        (shown nil))
    (cl-letf (((symbol-function 'skg-open-org-buffer-from-text)
               (lambda (_tcp-proc content buffer-name view-uri &rest _registry)
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

(ert-deftest test-switch-to-view-is-displayed-by-deferred-callback ()
  (let ((target (generate-new-buffer " *skg-switch-target*"))
        displayed timer-called)
    (unwind-protect
        (progn
          (with-current-buffer target
            (setq skg-view-uri "existing-uri"))
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (_secs _repeat function &rest args)
                       (setq timer-called t)
                       (apply function args)))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (buffer &rest _args)
                       (setq displayed buffer))))
            (skg-handle-content-view-sexp
             nil "((switch-to-view existing-uri))" "unused-uri" "node-x")
            (should timer-called)
            (should (eq displayed target))))
      (when (buffer-live-p target) (kill-buffer target)))))

(ert-deftest test-missing-switch-uri-closes-and-retries-only-once ()
  (let (closed retried visible)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_secs _repeat function &rest args)
                 (apply function args)))
              ((symbol-function 'skg-send-close-view-uri)
               (lambda (tcp uri) (setq closed (list tcp uri))))
              ((symbol-function 'skg-request-single-root-content-view-from-id)
               (lambda (&rest args) (setq retried args)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq visible (apply #'format format-string args)))))
      (skg-handle-content-view-sexp
       'test-tcp "((switch-to-view stale-uri))" "unused-uri"
       "node-x" t '("approved-pid") nil)
      (should (equal closed '(test-tcp "stale-uri")))
      (should (equal retried
                     '("node-x" test-tcp t ("approved-pid") nil t)))
      (setq closed nil retried nil)
      (skg-handle-content-view-sexp
       'test-tcp "((switch-to-view still-stale))" "unused-uri"
       "node-x" t nil t)
      (should-not closed)
      (should-not retried)
      (should (string-match-p
               "server twice returned a missing view (still-stale)"
               visible)))))

(ert-deftest test-rerender-done-with-errors-and-warnings-shows-both ()
  (let ((shown nil)
        (ended nil)
        (unlocked nil)
        (skg--request-draft nil))
    (cl-letf (((symbol-function 'skg--end-stream)
               (lambda () (setq ended t)))
              ((symbol-function 'skg--unlock-all-save-locked)
               (lambda () (setq unlocked t)))
              ((symbol-function 'skg-big-nonfatal-message)
               (lambda (buffer-name message-text content)
                 (setq shown (list buffer-name message-text content)))))
      (skg--register-rerender-stream-handlers)
      (let ((handler
             (cadr (assoc
                    'rerender-done
                    (skg--request-record-handlers skg--request-draft)))))
        (funcall handler
                 nil
                 (prin1-to-string
                  '((errors ("view failed"))
                    (warnings ("audit warning"))))))
      (should ended)
      (should unlocked)
      (should (equal (car shown) "*skg rerender messages*"))
      (should (string-match-p "^\\* errors\n\\*\\* view failed"
                              (nth 2 shown)))
      (should (string-match-p "^\\* warnings\n\\*\\* audit warning"
                              (nth 2 shown))))))

(provide 'test-skg-warning-channel)
