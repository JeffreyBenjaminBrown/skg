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
                 :point-column 0
                 :point-screen-lines-below-window-start 0))))
    (should (equal (cdr (assoc 'request sexp)) "save buffer"))
    (should-not (assoc 'fork-approved sexp))))

(ert-deftest test-save-request-sexp-includes-fork-approved-when-set ()
  "With approval, the save request carries (fork-approved . \"true\")."
  (let ((sexp (skg--save-request-sexp
               "uri-1"
               '(:point-lines-below-focused-headline 0
                 :point-column 0
                 :point-screen-lines-below-window-start 0)
               t)))
    (should (equal (cdr (assoc 'fork-approved sexp)) "true"))))

(ert-deftest test-save-request-sexp-omits-fork-sources-by-default ()
  "Without chosen sources, the save request carries no fork-sources field."
  (let ((sexp (skg--save-request-sexp
               "uri-1"
               '(:point-lines-below-focused-headline 0
                 :point-column 0
                 :point-screen-lines-below-window-start 0)
               t)))
    (should-not (assoc 'fork-sources sexp))))

(ert-deftest test-save-request-sexp-includes-fork-sources-when-set ()
  "With chosen sources, the request carries (fork-sources ((N . X) ...))."
  (let* ((sexp (skg--save-request-sexp
                "uri-1"
                '(:point-lines-below-focused-headline 0
                  :point-column 0
                  :point-screen-lines-below-window-start 0)
                t
                '(("N" . "owned2") ("M" . "owned"))))
         (entry (assoc 'fork-sources sexp)))
    (should entry)
    ;; The field is (fork-sources ((N . X) (M . Y))) -- a list, not a
    ;; dotted pair -- so the alist is the cadr.
    (should (equal (cadr entry)
                   '(("N" . "owned2") ("M" . "owned"))))))

(ert-deftest test-fork-sources-from-confirmation-buffer-walks-two-levels ()
  "skg--fork-sources-from-confirmation-buffer pairs each clone-to-be
parent's (source X) with each child's (id N)."
  (with-temp-buffer
    (insert "# FORK CONFIRMATION\n")
    (insert "* (skg (node (source owned2) (viewStats (sourceHerald ⌂:owned2)))) N-edited\n")
    (insert "** (skg (node (id N) (source foreign) (parentIs independent) indef (viewStats parentOverrides))) N-original\n")
    (org-mode)
    (should (equal (skg--fork-sources-from-confirmation-buffer)
                   '(("N" . "owned2"))))))

(ert-deftest test-fork-sources-walk-does-not-leak-source-across-clones ()
  "A metadata-less level-1 headline must not leak the previous clone's
source to a later fork's child (parent-source resets on every level 1)."
  (with-temp-buffer
    (insert "* (skg (node (source ownedA))) A-edited\n")
    (insert "** (skg (node (id N1) (source foreign) indef)) N1-original\n")
    ;; A stray/garbled level-1 headline with no skg metadata.
    (insert "* plain heading, no metadata\n")
    (insert "** (skg (node (id N2) (source foreign) indef)) N2-original\n")
    (org-mode)
    ;; N1 -> ownedA; N2 must NOT inherit ownedA (its parent has no source).
    (should (equal (skg--fork-sources-from-confirmation-buffer)
                   '(("N1" . "ownedA"))))))

(ert-deftest test-show-fork-confirmation-builds-editable-navigable-buffer ()
  "skg--show-fork-confirmation inserts the content into an EDITABLE
content-view buffer (so the user can rotate each clone's source), records
the origin, leaves skg-view-uri nil, and binds approve/decline plus an
ordinary-save refusal on C-x C-s."
  (let ((origin (generate-new-buffer "*fork-origin*")))
    (unwind-protect
        (let ((buf (skg--show-fork-confirmation
                    "# FORK CONFIRMATION\n* (skg (node (source owned))) N-edited\n** (skg (node (id N) (source foreign) (parentIs independent) indef (viewStats parentOverrides))) N-original\n"
                    origin)))
          (unwind-protect
              (with-current-buffer buf
                (should-not buffer-read-only)
                (should (null skg-view-uri))
                (should (eq skg--fork-origin-buffer origin))
                (should (derived-mode-p 'skg-content-view-mode))
                (should (string-match-p "(id N)" (buffer-string)))
                ;; approve / decline / save-refusal are reachable
                (should (eq (key-binding (kbd "C-c C-c")) #'skg-approve-fork))
                (should (eq (key-binding (kbd "C-c C-k")) #'skg-decline-fork))
                (should (eq (key-binding (kbd "C-x C-s"))
                            #'skg--fork-confirmation-refuse-save)))
            (kill-buffer buf)))
      (when (buffer-live-p origin) (kill-buffer origin)))))

(ert-deftest test-fork-confirmation-does-not-mutate-shared-mode-map ()
  "The buffer-local key overrides must not leak into the shared
skg-content-view-mode-map (which would break C-x C-s in real views)."
  (let ((origin (generate-new-buffer "*fork-origin-3*")))
    (let ((buf (skg--show-fork-confirmation
                "* (skg (node (source owned))) N-edited\n"
                origin)))
      (unwind-protect
          (should (eq (lookup-key skg-content-view-mode-map (kbd "C-x C-s"))
                      #'skg-request-save-buffer))
        (kill-buffer buf)
        (when (buffer-live-p origin) (kill-buffer origin))))))

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
