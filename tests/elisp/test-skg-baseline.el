;;; test-skg-baseline.el --- A view keeps the text the server last drew, as its baseline -*- lexical-binding: t; -*-

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'cl-lib)
(require 'skg-buffer)
(require 'skg-request-save)

(ert-deftest test-a-view-opened-from-a-response-keeps-its-text-as-baseline ()
  "A view's baseline is the text it was opened with."
  (let ((buf (skg-open-org-buffer-from-text
              nil "* hello\n" "*test-baseline-open*" "view-1")))
    (unwind-protect
        (with-current-buffer buf
          (should (equal skg-clean-baseline "* hello\n")))
      (kill-buffer buf))))

(ert-deftest test-a-streamed-view-update-replaces-the-baseline ()
  "A collateral or rerender view's text becomes the buffer's baseline."
  (with-temp-buffer
    (skg-content-view-mode)
    (setq-local skg-view-id "view-streamed")
    (insert "* (skg node (gnid a)) a\n")
    (skg--capture-clean-baseline)
    (skg--apply-streamed-view-update
     "((view-id \"view-streamed\") (content \"* (skg node (gnid a)) a2\n\"))"
     'rerender "rerender-view")
    (should (equal skg-clean-baseline "* (skg node (gnid a)) a2\n"))))

(ert-deftest test-a-saved-buffer-takes-the-save-results-text-as-baseline ()
  (with-temp-buffer
    (org-mode)
    (insert "* edited\n")
    (setq-local skg-clean-baseline "* drawn\n")
    (cl-letf (((symbol-function 'skg-replace-buffer-with-new-content)
               (lambda (_tcp-proc new-content &optional _position)
                 (erase-buffer)
                 (insert new-content)
                 (skg--capture-clean-baseline))))
      (skg-handle-save-sexp
       (prin1-to-string
        '((content "* root\n") (errors ()) (warnings ())))))
    (should (equal skg-clean-baseline "* root\n"))))

(ert-deftest test-a-stale-refusal-keeps-the-edits-and-the-baseline ()
  "A refused save carries no content: the buffer keeps its text and
its baseline, and the refusal is shown as a save error."
  (with-temp-buffer
    (org-mode)
    (insert "* edited\n")
    (setq-local skg-clean-baseline "* drawn\n")
    (let ((shown nil))
      (cl-letf (((symbol-function 'skg-show-save-errors-and-warnings)
                 (lambda (errors _warnings _content)
                   (setq shown errors))))
        (skg-handle-save-sexp
         (prin1-to-string
          '((content nil)
            (errors ("NOTHING WAS SAVED\n\nValidation errors found in buffer.\n\n** Error 1\nA node this save would write changed after the buffer was rendered:\n- Title: a\n- ID: a\n- Changed since this buffer was rendered: the title (then \"x\", now \"a\").\n"))
            (warnings ())))))
      (should (string-match-p "changed after the buffer was rendered"
                              (car shown))))
    (should (equal (buffer-string) "* edited\n"))
    (should (equal skg-clean-baseline "* drawn\n"))))

(provide 'test-skg-baseline)
