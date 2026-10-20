;;; test-skg-strandeds-confirmation.el --- Tests for the strandeds-confirmation client -*- lexical-binding: t; -*-

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'cl-lib)
(require 'skg-request-save)

(defconst test--strandeds-point-position
  '(:point-lines-below-focused-headline 0
    :point-column 0
    :point-screen-lines-below-window-start 0))

(defconst test--strandeds-payload
  (concat "((response-type strandeds-confirmation)"
          " (strandeds (((relation \"contains\") (recorder \"q\") (member \"n\")"
          "            (relRepo \"private\") (default \"public\"))"
          "           ((relation \"contains\") (recorder \"r\") (member \"n\")"
          "            (relRepo \"private\") (default \"public\"))))"
          " (prompt \"Reset them to default? \"))"))

(ert-deftest test-save-request-sexp-omits-the-strandeds-review-by-default ()
  (let ((sexp (skg--save-request-sexp "v" test--strandeds-point-position)))
    (should-not (assoc 'strandeds-reviewed sexp))
    (should-not (assoc 'reset-strandeds sexp))))

(ert-deftest test-save-request-sexp-carries-a-strandeds-review ()
  (let ((sexp (skg--save-request-sexp
               "v" test--strandeds-point-position nil nil nil nil
               '(reviewed ("contains" "q" "n")))))
    (should (equal (cdr (assoc 'strandeds-reviewed sexp)) "true"))
    (should (equal (cdr (assoc 'reset-strandeds sexp))
                   '(("contains" "q" "n")))))
  (let ((sexp (skg--save-request-sexp
               "v" test--strandeds-point-position nil nil nil nil
               '(reviewed))))
    (should (equal (cdr (assoc 'strandeds-reviewed sexp)) "true"))
    (should-not (assoc 'reset-strandeds sexp))))

(ert-deftest test-strandeds-from-response ()
  (let ((strandeds (skg--strandeds-from-response (read test--strandeds-payload))))
    (should (= (length strandeds) 2))
    (should (equal (car strandeds)
                   '(:relation "contains" :recorder "q" :member "n"
                     :relRepo "private" :default "public")))))

(ert-deftest test-strandeds-review-from-choice ()
  (let ((strandeds (skg--strandeds-from-response (read test--strandeds-payload))))
    (should (equal (skg--strandeds-review-from-choice strandeds ?a #'ignore)
                   '(reviewed ("contains" "q" "n") ("contains" "r" "n"))))
    (should (equal (skg--strandeds-review-from-choice strandeds ?n #'ignore)
                   '(reviewed)))
    (should (equal (skg--strandeds-review-from-choice
                    strandeds ?c
                    (lambda (prompt) (string-match-p "^Reset r " prompt)))
                   '(reviewed ("contains" "r" "n"))))
    (should-not (skg--strandeds-review-from-choice strandeds ?q #'ignore))))

(ert-deftest test-strandeds-confirmation-resaves-with-the-review-and-approvals ()
  "The handler is terminal for the save attempt, and re-issues the save
with the review, carrying the attempt's other approvals."
  (let ((skg-response-handler-map
         (list (cons 'save-result #'ignore)
               (cons 'collateral-view #'ignore)
               (cons 'telescope-hoist-confirmation #'ignore)))
        (skg-lp--pending-count 1)
        (noninteractive nil)
        (resave-args :none)
        (save-buffer (generate-new-buffer "strandeds-test")))
    (unwind-protect
        (cl-letf (((symbol-function 'skg--end-stream) #'ignore)
                  ((symbol-function 'skg--unlock-all-save-locked) #'ignore)
                  ((symbol-function 'skg--show-strandeds) #'ignore)
                  ((symbol-function 'read-multiple-choice)
                   (lambda (&rest _) '(?a "all")))
                  ((symbol-function 'skg-request-save-buffer)
                   (lambda (&rest args) (setq resave-args args))))
          (skg--strandeds-confirmation-handler
           save-buffer test--strandeds-payload
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

(provide 'test-skg-strandeds-confirmation)
