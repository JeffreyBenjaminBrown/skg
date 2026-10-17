;;; test-skg-strand-confirmation.el --- Tests for the strand-confirmation client -*- lexical-binding: t; -*-

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'cl-lib)
(require 'skg-request-save)

(defconst test--strand-point-position
  '(:point-lines-below-focused-headline 0
    :point-column 0
    :point-screen-lines-below-window-start 0))

(defconst test--strand-payload
  (concat "((response-type strand-confirmation)"
          " (strands (((relation \"contains\") (recorder \"q\") (member \"n\")"
          "            (relRepo \"private\") (default \"public\"))"
          "           ((relation \"contains\") (recorder \"r\") (member \"n\")"
          "            (relRepo \"private\") (default \"public\"))))"
          " (prompt \"Re-level them? \"))"))

(ert-deftest test-save-request-sexp-omits-the-strand-review-by-default ()
  (let ((sexp (skg--save-request-sexp "v" test--strand-point-position)))
    (should-not (assoc 'strands-reviewed sexp))
    (should-not (assoc 'relevel-strands sexp))))

(ert-deftest test-save-request-sexp-carries-a-strand-review ()
  (let ((sexp (skg--save-request-sexp
               "v" test--strand-point-position nil nil nil nil
               '(reviewed ("contains" "q" "n")))))
    (should (equal (cdr (assoc 'strands-reviewed sexp)) "true"))
    (should (equal (cdr (assoc 'relevel-strands sexp))
                   '(("contains" "q" "n")))))
  (let ((sexp (skg--save-request-sexp
               "v" test--strand-point-position nil nil nil nil
               '(reviewed))))
    (should (equal (cdr (assoc 'strands-reviewed sexp)) "true"))
    (should-not (assoc 'relevel-strands sexp))))

(ert-deftest test-strands-from-response ()
  (let ((strands (skg--strands-from-response (read test--strand-payload))))
    (should (= (length strands) 2))
    (should (equal (car strands)
                   '(:relation "contains" :recorder "q" :member "n"
                     :relRepo "private" :default "public")))))

(ert-deftest test-strand-review-from-choice ()
  (let ((strands (skg--strands-from-response (read test--strand-payload))))
    (should (equal (skg--strand-review-from-choice strands ?a #'ignore)
                   '(reviewed ("contains" "q" "n") ("contains" "r" "n"))))
    (should (equal (skg--strand-review-from-choice strands ?n #'ignore)
                   '(reviewed)))
    (should (equal (skg--strand-review-from-choice
                    strands ?c
                    (lambda (prompt) (string-match-p "^Re-level r " prompt)))
                   '(reviewed ("contains" "r" "n"))))
    (should-not (skg--strand-review-from-choice strands ?q #'ignore))))

(ert-deftest test-strand-confirmation-resaves-with-the-review-and-approvals ()
  "The handler is terminal for the save attempt, and re-issues the save
with the review, carrying the attempt's other approvals."
  (let ((skg-response-handler-map
         (list (cons 'save-result #'ignore)
               (cons 'collateral-view #'ignore)
               (cons 'telescope-hoist-confirmation #'ignore)))
        (skg-lp--pending-count 1)
        (noninteractive nil)
        (resave-args :none)
        (save-buffer (generate-new-buffer "strand-test")))
    (unwind-protect
        (cl-letf (((symbol-function 'skg--end-stream) #'ignore)
                  ((symbol-function 'skg--unlock-all-save-locked) #'ignore)
                  ((symbol-function 'skg--show-strands) #'ignore)
                  ((symbol-function 'read-multiple-choice)
                   (lambda (&rest _) '(?a "all")))
                  ((symbol-function 'skg-request-save-buffer)
                   (lambda (&rest args) (setq resave-args args))))
          (skg--strand-confirmation-handler
           save-buffer test--strand-payload
           t '(("n" . "public")) '("h") '("t"))
          (should-not (assoc 'save-result skg-response-handler-map))
          (should-not (assoc 'telescope-hoist-confirmation
                             skg-response-handler-map))
          (should (= skg-lp--pending-count 0))
          (should (equal resave-args
                         '(t (("n" . "public")) ("h") ("t")
                             (reviewed ("contains" "q" "n")
                                       ("contains" "r" "n"))))))
      (kill-buffer save-buffer))))

(provide 'test-skg-strand-confirmation)
