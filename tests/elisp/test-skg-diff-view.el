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
                   . ,(test-skg-diff-view--content "* (skg (node (id a))) a\n"))
                  ("merges" . ,test-skg-diff-view--empty)
                  ("skgrepo changes"
                   . ,(test-skg-diff-view--content "* (skg (node (id b))) b\n"))
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

(provide 'test-skg-diff-view)
