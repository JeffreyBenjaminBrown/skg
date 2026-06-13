;;; test-skg-fork-confirmation.el --- Tests for the fork-confirmation client -*- lexical-binding: t; -*-

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'skg-request-save)

(ert-deftest test-save-request-sexp-omits-fork-approved-by-default ()
  "Without approval, the save request carries no fork-approved field."
  (let ((sexp (skg--save-request-sexp
               "uri-1"
               '(:point-lines-below-focused-headline 0
                 :point-screen-lines-below-window-start 0))))
    (should (equal (cdr (assoc 'request sexp)) "save buffer"))
    (should-not (assoc 'fork-approved sexp))))

(ert-deftest test-save-request-sexp-includes-fork-approved-when-set ()
  "With approval, the save request carries (fork-approved . \"true\")."
  (let ((sexp (skg--save-request-sexp
               "uri-1"
               '(:point-lines-below-focused-headline 0
                 :point-screen-lines-below-window-start 0)
               t)))
    (should (equal (cdr (assoc 'fork-approved sexp)) "true"))))

(ert-deftest test-show-fork-confirmation-builds-readonly-navigable-buffer ()
  "skg--show-fork-confirmation inserts the content into a read-only
content-view buffer, records the origin buffer, and binds approve/decline."
  (let ((origin (generate-new-buffer "*fork-origin*")))
    (unwind-protect
        (let ((buf (skg--show-fork-confirmation
                    "# FORK CONFIRMATION\n* (skg (node (id N) (source foreign) indef)) N\n"
                    origin)))
          (unwind-protect
              (with-current-buffer buf
                (should buffer-read-only)
                (should (eq skg--fork-origin-buffer origin))
                (should (derived-mode-p 'skg-content-view-mode))
                (should (string-match-p "(id N)" (buffer-string)))
                ;; approve / decline are reachable from the buffer
                (should (eq (key-binding (kbd "C-c C-c")) #'skg-approve-fork))
                (should (eq (key-binding (kbd "C-c C-k")) #'skg-decline-fork)))
            (kill-buffer buf)))
      (when (buffer-live-p origin) (kill-buffer origin)))))

(ert-deftest test-approve-fork-errors-when-origin-is-gone ()
  "skg-approve-fork refuses when the originating buffer is dead."
  (let ((origin (generate-new-buffer "*fork-origin-2*")))
    (let ((buf (skg--show-fork-confirmation "* (skg (node (id N) (source foreign) indef)) N\n"
                                            origin)))
      (kill-buffer origin) ;; origin dies before approval
      (unwind-protect
          (with-current-buffer buf
            (should-error (skg-approve-fork)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(provide 'test-skg-fork-confirmation)
