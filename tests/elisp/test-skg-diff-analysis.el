;;; test-skg-diff-analysis.el --- Tests for skg diff report requests.

(defconst test-skg-diff-analysis--this-dir
  (file-name-directory load-file-name))

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             test-skg-diff-analysis--this-dir))
(require 'ert)
(require 'skg-request-diff-analysis)

(defmacro test-skg-diff-analysis--capture-request (answers &rest body)
  "Run BODY with y-or-n-p ANSWERS and capture the sent request."
  (declare (indent 1))
  `(let ((sent-request nil)
         (answers-list (copy-sequence ,answers))
         (registered-type nil))
     (cl-letf (((symbol-function 'y-or-n-p)
                (lambda (_prompt)
                  (pop answers-list)))
               ((symbol-function 'skg-tcp-connect-to-rust)
                (lambda () :fake-proc))
               ((symbol-function 'skg-register-response-handler)
                (lambda (response-type _handler _one-shot)
                  (setq registered-type response-type)))
               ((symbol-function 'skg-lp-reset)
                (lambda () nil))
               ((symbol-function 'process-send-string)
                (lambda (_proc string)
                  (setq sent-request string))))
       ,@body
       (list sent-request registered-type answers-list))))

(ert-deftest test-skg-diff-report-staged-no-sends-staged-false-unstaged-true ()
  (pcase-let ((`(,sent ,registered ,remaining)
               (test-skg-diff-analysis--capture-request (list nil)
                 (skg-diff-report))))
    (should (equal registered 'diff-analysis))
    (should (null remaining))
    (should (string-match-p "(include-staged \\. \"false\")" sent))
    (should (string-match-p "(include-unstaged \\. \"true\")" sent))))

(ert-deftest test-skg-diff-report-staged-yes-unstaged-no ()
  (pcase-let ((`(,sent ,registered ,remaining)
               (test-skg-diff-analysis--capture-request (list t nil)
                 (skg-diff-report))))
    (should (equal registered 'diff-analysis))
    (should (null remaining))
    (should (string-match-p "(include-staged \\. \"true\")" sent))
    (should (string-match-p "(include-unstaged \\. \"false\")" sent))))

(ert-deftest test-skg-diff-report-staged-yes-unstaged-yes ()
  (pcase-let ((`(,sent ,registered ,remaining)
               (test-skg-diff-analysis--capture-request (list t t)
                 (skg-diff-report))))
    (should (equal registered 'diff-analysis))
    (should (null remaining))
    (should (string-match-p "(include-staged \\. \"true\")" sent))
    (should (string-match-p "(include-unstaged \\. \"true\")" sent))))

(ert-deftest test-skg-diff-analysis-handler-enables-analysis-keymap ()
  (unwind-protect
      (progn
        (skg--diff-analysis-handler
         nil
         (prin1-to-string '((content "* affected nodes\n"))))
        (with-current-buffer "*skg diff analysis*"
          (should skg-diff-analysis-mode)
          (should (eq (lookup-key skg-diff-analysis-mode-map
                                  (kbd "C-c f RET"))
                      #'skg-search))
          (should (eq (lookup-key skg-diff-analysis-mode-map
                                  (kbd "C-c g m"))
                      #'skg-goto-in-magit))
          (should (eq (lookup-key skg-diff-analysis-mode-map
                                  (kbd "C-c G M"))
                      #'skg-goto-in-magit-parent-and-close-this))
          (should (eq (lookup-key skg-diff-analysis-mode-map
                                  (kbd "C-c v e"))
                      #'skg-view-new-empty))
          (should (eq (lookup-key skg-diff-analysis-mode-map
                                  (kbd "C-c u"))
                      #'skg-id-push))
          (should (eq (lookup-key skg-diff-analysis-mode-map
                                  (kbd "C-c O l"))
                      #'skg-pop-link))))
    (when (get-buffer "*skg diff analysis*")
      (kill-buffer "*skg diff analysis*"))))
