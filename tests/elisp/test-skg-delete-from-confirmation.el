;;; test-skg-delete-from-confirmation.el --- Tests for the delete-from confirmation client -*- lexical-binding: t; -*-

;; Under a skgrepo restriction, a save whose deletes would strip records
;; the skgrepo restriction keeps out stops with a delete-from-confirmation.
;; The client asks "Delete from: (c)ancel, (v)isible records only, (a)ll
;; owned records" and re-issues the same save with the choice, which rides
;; that save and its retries only.

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'cl-lib)
(require 'skg-request-save)

(defconst test--delete-from-point-position
  '(:point-lines-below-focused-headline 0
    :point-column 0
    :point-screen-lines-below-window-start 0))

(defconst test--delete-from-payload
  (concat "((response-type delete-from-confirmation)"
          " (count 2)"
          " (prompt \"2 records in skgrepos the skgrepo restriction keeps out name a graphnode this save deletes. Nothing was saved.\"))"))

(ert-deftest test-save-request-sexp-omits-delete-from-by-default ()
  (should-not (assoc 'delete-from
                     (skg--save-request-sexp "v" test--delete-from-point-position))))

(ert-deftest test-save-request-sexp-carries-delete-from ()
  (let ((sexp (skg--save-request-sexp
               "v" test--delete-from-point-position nil nil nil nil
               "visible")))
    (should (equal (cdr (assoc 'delete-from sexp)) "visible"))))

(ert-deftest test-delete-from-of-choice ()
  (should (equal (skg--delete-from-of-choice ?v) "visible"))
  (should (equal (skg--delete-from-of-choice ?a) "all"))
  (should-not (skg--delete-from-of-choice ?c)))

(defun test--delete-from-handler-resaves-with (key)
  "Run the delete-from handler on the test payload with KEY as the answer;
return the arguments it re-saved with, :none when it did not re-save, and
the prompt it asked."
  (let ((skg-response-handler-map
         (list (cons 'save-result #'ignore)
               (cons 'collateral-view #'ignore)
               (cons 'strandeds-confirmation #'ignore)))
        (skg-lp--pending-count 1)
        (noninteractive nil)
        (resave-args :none)
        (asked nil)
        (save-buffer (generate-new-buffer "delete-from-test")))
    (unwind-protect
        (cl-letf (((symbol-function 'skg--end-stream) #'ignore)
                  ((symbol-function 'skg--unlock-all-save-locked) #'ignore)
                  ((symbol-function 'read-char-choice)
                   (lambda (prompt _chars) (setq asked prompt) key))
                  ((symbol-function 'skg-request-save-buffer)
                   (lambda (&rest args) (setq resave-args args))))
          (skg--delete-from-confirmation-handler
           save-buffer test--delete-from-payload
           t '(("n" . "public")) '("h") '(reviewed))
          (should-not (assoc 'save-result skg-response-handler-map))
          (should-not (assoc 'strandeds-confirmation skg-response-handler-map))
          (should (= skg-lp--pending-count 0))
          (list resave-args asked))
      (kill-buffer save-buffer))))

(ert-deftest test-delete-from-confirmation-resaves-with-the-choice-and-approvals ()
  "The handler is terminal for the save attempt, asks where to delete
from, and re-issues the save with the choice, carrying the attempt's
other approvals."
  (pcase-let ((`(,args ,asked) (test--delete-from-handler-resaves-with ?v)))
    (should (string-match-p
             "Delete from: (c)ancel, (v)isible records only, (a)ll owned records"
             asked))
    (should (equal args '(t (("n" . "public")) ("h") (reviewed) "visible"))))
  (pcase-let ((`(,args ,_) (test--delete-from-handler-resaves-with ?a)))
    (should (equal (car (last args)) "all"))))

(ert-deftest test-delete-from-confirmation-cancel-saves-nothing ()
  (pcase-let ((`(,args ,_) (test--delete-from-handler-resaves-with ?c)))
    (should (eq args :none))))

(ert-deftest test-strandeds-confirmation-carries-the-delete-from-choice ()
  "A strandeds confirmation that arises on the retry after a delete-from
choice re-issues the save with that choice still on it."
  (let ((skg-response-handler-map (list (cons 'save-result #'ignore)))
        (skg-lp--pending-count 1)
        (noninteractive nil)
        (resave-args :none)
        (save-buffer (generate-new-buffer "delete-from-strandeds-test")))
    (unwind-protect
        (cl-letf (((symbol-function 'skg--end-stream) #'ignore)
                  ((symbol-function 'skg--unlock-all-save-locked) #'ignore)
                  ((symbol-function 'skg--show-strandeds) #'ignore)
                  ((symbol-function 'read-multiple-choice)
                   (lambda (&rest _) '(?n "none")))
                  ((symbol-function 'skg-request-save-buffer)
                   (lambda (&rest args) (setq resave-args args))))
          (skg--strandeds-confirmation-handler
           save-buffer
           (concat "((response-type strandeds-confirmation)"
                   " (strandeds (((relation \"contains\") (recorder \"q\") (member \"n\")"
                   " (relRepo \"private\") (default \"public\"))))"
                   " (prompt \"Reset? \"))")
           nil nil nil "visible")
          (should (equal resave-args '(nil nil nil (reviewed) "visible"))))
      (kill-buffer save-buffer))))

(provide 'test-skg-delete-from-confirmation)
