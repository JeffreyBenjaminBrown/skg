;;; test-skg-diff-view.el --- Tests for the diff view command.  -*- lexical-binding: t; -*-

(defconst test-skg-diff-view--this-dir
  (file-name-directory load-file-name))

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             test-skg-diff-view--this-dir))
(require 'ert)
(require 'skg-request-diff-view)

(defun test-skg-diff-view--run (answers)
  "Run `skg-diffView-open' against a fake server that answers each
buffer's request from ANSWERS, an alist from buffer name to payload.
Returns (REQUESTED-BUFFERS OPENED-BUFFER-NAMES)."
  (let ((handler nil)
        (requested '())
        (opened '()))
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust)
               (lambda () :fake-proc))
              ((symbol-function 'skg-register-response-handler)
               (lambda (_type new-handler _one-shot)
                 (setq handler new-handler)))
              ((symbol-function 'skg-lp-reset) (lambda () nil))
              ((symbol-function 'run-at-time)
               (lambda (_time _repeat function &rest args)
                 (apply function args)))
              ((symbol-function 'skg-open-org-buffer-from-text)
               (lambda (_proc _text buffer-name _view-id)
                 (push buffer-name opened)))
              ((symbol-function 'skg-big-nonfatal-message)
               (lambda (&rest _) nil))
              ((symbol-function 'process-send-string)
               (lambda (_proc string)
                 (let* ((request (car (read-from-string string)))
                        (buffer (cdr (assoc 'buffer request))))
                   (push buffer requested)
                   (should (equal (cdr (assoc 'request request))
                                  "diff view"))
                   (should (cdr (assoc 'view-id request)))
                   (funcall handler :fake-proc
                            (cdr (assoc buffer answers)))))))
      (skg-diffView-open))
    (list (nreverse requested) (nreverse opened))))

(defconst test-skg-diff-view--empty
  "((empty . t) (errors ()) (warnings ()))")

(defun test-skg-diff-view--content (text)
  (prin1-to-string `((content ,text) (errors ()) (warnings ()))))

(ert-deftest test-skg-diff-view-opens-only-nonempty-buffers ()
  (pcase-let ((`(,requested ,opened)
               (test-skg-diff-view--run
                `(("all changes"
                   . ,(test-skg-diff-view--content "* (skg node (gnid a)) a\n"))
                  ("merges" . ,test-skg-diff-view--empty)
                  ("skgrepo changes"
                   . ,(test-skg-diff-view--content "* (skg node (gnid b)) b\n"))
                  ("order-only changes" . ,test-skg-diff-view--empty)))))
    (should (equal requested skg-diffView-buffers))
    (should (equal opened '("diff-view: all changes"
                            "diff-view: skgrepo changes")))))

(ert-deftest test-skg-diff-view-a-refusal-ends-the-sequence ()
  (pcase-let ((`(,requested ,opened)
               (test-skg-diff-view--run
                `(("all changes"
                   . ,(prin1-to-string
                       '((content "")
                         (errors ("The diff view requires no skgrepo restriction"))
                         (warnings ()))))))))
    (should (equal requested '("all changes")))
    (should (null opened))))

(ert-deftest test-skg-diff-view-shows-the-readiness-summary ()
  (let ((shown nil))
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq shown (apply #'format format-string args)))))
      (test-skg-diff-view--run
       `(("all changes"
          . ,(prin1-to-string
              '((content "* (skg node (gnid a)) a\n")
                (errors ()) (warnings ())
                (readiness-summary (ready 1) (changed 3) (foreign 0)
                                   (order-only 0) (notEditable 2)))))
         ("merges" . ,test-skg-diff-view--empty)
         ("skgrepo changes" . ,test-skg-diff-view--empty)
         ("order-only changes" . ,test-skg-diff-view--empty))))
    (should (equal shown
                   (concat "1 of 3 changed files are ready, 2 changes are "
                           "not editable in this view; C-c r u extends the diff view to them")))))

(ert-deftest test-skg-diff-view-a-buffer-names-itself-on-save ()
  "A buffer of the diff view keeps its buffer's name and sends it on
save as (diff-view-buffer . NAME); a content view sends none."
  (let ((point-position '(:point-lines-below-focused-headline 0
                          :point-column 0
                          :point-screen-lines-below-window-start 0))
        (diffView-buffer nil)
        (content-buffer nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'message) #'ignore))
            (skg--handle-diffView-response
             nil (test-skg-diff-view--content "* (skg node (gnid a)) a\n")
             "all changes" "diff-view-id"))
          (setq diffView-buffer (skg-find-buffer-by-view-id "diff-view-id"))
          (setq content-buffer (skg-open-org-buffer-from-text
                                nil "* (skg node (gnid b)) b\n"
                                "*test-skg-content-view*" "content-view-id"))
          (with-current-buffer diffView-buffer
            (should (equal (assoc 'diff-view-buffer
                                  (skg--save-request-sexp
                                   skg-view-id point-position))
                           '(diff-view-buffer . "all changes"))))
          (with-current-buffer content-buffer
            (should-not (assoc 'diff-view-buffer
                               (skg--save-request-sexp
                                skg-view-id point-position)))))
      (dolist (buffer (list diffView-buffer content-buffer))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq-local kill-buffer-hook nil)
            (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(provide 'test-skg-diff-view)
