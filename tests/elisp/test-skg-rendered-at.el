;;; test-skg-rendered-at.el --- A buffer keeps the generation it was rendered at, and its save sends it -*- lexical-binding: t; -*-

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'cl-lib)
(require 'skg-buffer)
(require 'skg-request-save)

(defconst test--rendered-at-point-position
  '(:point-lines-below-focused-headline 0
    :point-column 0
    :point-screen-lines-below-window-start 0))

(ert-deftest test-a-view-opened-from-a-response-keeps-its-rendered-at ()
  "The generation a view was opened with rides out on its save."
  (let ((buf (skg-open-org-buffer-from-text
              nil "* hello\n" "*test-rendered-at-open*" "view-1" "7")))
    (unwind-protect
        (with-current-buffer buf
          (should (equal skg-rendered-at "7"))
          (should (equal (cdr (assoc 'rendered-at
                                     (skg--save-request-sexp
                                      skg-view-id
                                      test--rendered-at-point-position)))
                         "7")))
      (kill-buffer buf))))

(ert-deftest test-a-buffer-no-response-drew-saves-as-rendered-never ()
  "An empty view carries no rendered-at, so its save sends \"never\"."
  (with-temp-buffer
    (should-not skg-rendered-at)
    (should (equal (cdr (assoc 'rendered-at
                               (skg--save-request-sexp
                                "view-1" test--rendered-at-point-position)))
                   "never"))))

(ert-deftest test-a-streamed-view-update-replaces-rendered-at ()
  "A collateral or rerender view carries the generation of its rerender."
  (with-temp-buffer
    (skg-content-view-mode)
    (setq-local skg-view-id "view-streamed")
    (setq-local skg-rendered-at "3")
    (insert "* (skg node (gnid a)) a\n")
    (skg--apply-streamed-view-update
     "((view-id \"view-streamed\") (content \"* (skg node (gnid a)) a2\n\") (rendered-at \"5\"))"
     'rerender "rerender-view")
    (should (equal skg-rendered-at "5"))))

(ert-deftest test-a-saved-buffer-takes-the-save-results-rendered-at ()
  (with-temp-buffer
    (org-mode)
    (setq-local skg-rendered-at "2")
    (cl-letf (((symbol-function 'skg-replace-buffer-with-new-content)
               (lambda (_tcp-proc new-content &optional _position)
                 (erase-buffer)
                 (insert new-content))))
      (skg-handle-save-sexp
       (prin1-to-string
        '((content "* root\n") (errors ()) (warnings ())
          (rendered-at "4")))))
    (should (equal skg-rendered-at "4"))))

(ert-deftest test-a-stale-refusal-keeps-the-edits-and-the-rendered-at ()
  "A refused save carries no content: the buffer keeps its text and
its generation, and the refusal is shown as a save error."
  (with-temp-buffer
    (org-mode)
    (insert "* edited\n")
    (setq-local skg-rendered-at "2")
    (let ((shown nil))
      (cl-letf (((symbol-function 'skg-show-save-errors-and-warnings)
                 (lambda (errors _warnings _content)
                   (setq shown errors))))
        (skg-handle-save-sexp
         (prin1-to-string
          '((content nil)
            (errors ("NOTHING WAS SAVED\n\nValidation errors found in buffer.\n\n** Error 1\nA node this save would write changed after the buffer was rendered:\n- ID: a\n"))
            (warnings ())))))
      (should (string-match-p "changed after the buffer was rendered"
                              (car shown))))
    (should (equal (buffer-string) "* edited\n"))
    (should (equal skg-rendered-at "2"))))

(provide 'test-skg-rendered-at)
