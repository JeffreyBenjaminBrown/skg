;;; PURPOSE: ERT tests for org-to-sexp module

;; USAGE
;; From command line:
;;   emacs -batch -l tests/tests.el
;; From within Emacs:
;;   Load the test file first
;;     (load-file "tests/org-to-sexp.el")
;;   Run specific test
;;     (ert-run-tests-interactively "test-first-property-on-line")
;;   Run all org-to-sexp tests
;;     (ert-run-tests-interactively "test-org-to-sexp")
;;   Run all tests
;;     (ert-run-tests-interactively t)

(require 'ert)
(require 'org-to-sexp)
(require 'skg-util)


(ert-deftest test-first-property-on-line ()
  "Tests the first-property-on-line function, by creating a temp buffer with three lines and various text properties attached to the words therein."
  (with-temp-buffer
    (org-mode)
    (progn;; Insert the test data
      (insert "line one\n")
      (insert "line two\n")
      (insert "line three\n") )
    (progn ;; Set up properties on line 1
      (goto-char (point-min))
      (when ;; Find "line" in first line and attach property "one" with value "line"
          (search-forward "line" (line-end-position) t)
        (put-text-property
         (match-beginning 0) (match-end 0) 'one "line"))
      (when ;; Find "one" in first line and attach property "one" with value "one"
          (search-forward "one" (line-end-position) t)
        (put-text-property
         (match-beginning 0) (match-end 0) 'one "one")))
    (progn ;; Set up properties on line 2
      (goto-char (point-min))
      (forward-line 1)
      (when (search-forward "line" (line-end-position) t)
        (put-text-property
         (match-beginning 0) (match-end 0) 'other t)))
    (progn ;; Set up properties on line 3
      (goto-char (point-min))
      (forward-line 2)
      (when (search-forward "line" (line-end-position) t)
        (put-text-property
         (match-beginning 0) (match-end 0) 'three "line"))
      (beginning-of-line)
      (when (search-forward "three" (line-end-position) t)
        (put-text-property
         (match-beginning 0) (match-end 0) 'three "three")))
    (progn ;; Test line 1
      (goto-char (point-min))
      (should (equal (first-property-on-line 'one)
                     "line")))
    (progn ;; Test line 2
      (goto-char (point-min))
      (forward-line 1)
      (forward-char 4) ; Move to the space between "line" and "two"
      (should (equal (first-property-on-line 'two)
                     nil)))
    (progn ;; Test line 3
      (goto-char (point-min))
      (forward-line 2)
      (forward-char 4) ; Move to the space between "line" and "three"
      (should (equal (first-property-on-line 'three)
                     "line")))))

(ert-deftest test-org-to-sexp-parse-heading-at-point ()
  "Test parsing headings at different positions on each line."
  (with-temp-buffer
    (org-mode)
    (insert "Not a heading.
* 1
** 2
Not a heading. TODO: What if this line is extant but empty?
** 3
* 4
Not a heading.")
    (dolist (test-case '((0 error)
                         (1 "1")
                         (2 "2")
                         (3 error)
                         (4 "3")
                         (5 "4")
                         (6 error)))
      (let ((line-num (nth 0 test-case))
            (expected (nth 1 test-case)))
        (progn ;; Go to a line
          (goto-char (point-min))
          (forward-line line-num))
        (let* ;; Get line boundaries for testing different positions
            ((line-start (line-beginning-position))
             (line-end (line-end-position))
             (line-length (- line-end line-start))
             (line-middle (+ line-start (/ line-length 2)))
             (positions (list line-start line-middle line-end))
             (results '()))
          (dolist (pos positions)
            (goto-char pos)
            (message "Buffer contents:\n%s" (buffer-string))
            (message "Point is at line %d, content: '%s'"
                     (line-number-at-pos)
                     (string-trim (thing-at-point 'line t)))
            (if (eq expected 'error)
                (should-error (org-to-sexp-parse-heading-at-point)
                              :type 'error)
              (let ((result (org-to-sexp-parse-heading-at-point)))
                (push (alist-get 'heading result) results))))
          (when (not (eq expected 'error))
            (should (equal results
                           (list expected expected expected))))
          )))))

(ert-deftest test-org-to-sexp-parse-heading-at-point-and-maybe-body ()
  "Test parsing headings with their bodies."
  (with-temp-buffer
    (org-mode)
    (insert "This line should err.
* 1
  1 has a body.
** 2 has an empty body.

** 3
   3 has a
   multi-line body.
* 4 is followed by two empty lines, which collapse to \"\".


")
    (let ((focused-line 4)
          (test-cases
           '((0 . error)
             (1 . (("heading" . "1")
                   ("body" . "  1 has a body.")))
             (3 . (("heading" . "2 has an empty body.")
                   ("body" . "")
                   ("focused" . t)))
             (5 . (("heading" . "3")
                   ("body" . "   3 has a\n   multi-line body.")))
             (8 . (("heading" . "4 is followed by two empty lines, which collapse to \"\".")
                   ("body" . ""))))))
      (dolist (test-case test-cases)
        (let ((line-num (car test-case))
              (expected (cdr test-case))
              (remaining-cases (cdr (member test-case test-cases))))
          (save-excursion
            (goto-char (point-min))
            (forward-line line-num)
            (if (eq expected 'error)
                (should-error
                 (org-to-sexp-parse-heading-at-point-and-maybe-body
                  focused-line)
                 :type 'error)
              (let ((result
                     (org-to-sexp-parse-heading-at-point-and-maybe-body
                      focused-line)))
                (should (equal (alist-get 'heading result)
                               (cdr (assoc "heading" expected))))
                (should (equal (alist-get 'body result)
                               (cdr (assoc "body" expected))))
                (if (assoc "focused" expected)
                    (should (equal (alist-get 'focused result) t))
                  (should (not (alist-get 'focused result))))
                (should ;; Ensure no other fields are present.
                 (equal (length result) (length expected)))
                (if ;; verify cursor lands on the next heading
                    remaining-cases
                    (should (equal (line-number-at-pos)
                                   (1+ (car (car remaining-cases)))))
                  (should ;; or EOF if there are no more headings
                   (eobp)))))))))))

(ert-deftest test-org-to-sexp-parse-all-branches ()
  "PURPOSE: Test org-to-sexp-parse-all-branches.
HOW IT WORKS:
This test relies on ./org-to-sexp/fixtures/1.org.
It will open that file in a buffer,
unfold the first line (titled '1'),
move point down two lines,
unfold that line (titled '11'),
and move point down one more line
(to the body of the previous heading,
which reads '11 body').
It will also add hidden text properties to the file:
Each heading will be given an `id` equal to its title.
The second instance of the heading '1'
will also be given the property `(repeated . t)`."

  (let ((test-file
         (expand-file-name
          "org-to-sexp/fixtures/1.org"
          (file-name-directory
           (or load-file-name buffer-file-name default-directory)) ))
        (expected-result
         (with-temp-buffer
         (insert-file-contents
          (expand-file-name
           "org-to-sexp/fixtures/1.el"
           (file-name-directory (or load-file-name
                                    buffer-file-name
                                    default-directory)) )) )) )
    (with-current-buffer
        (get-buffer-create "*test-org-to-sexp*")
      (erase-buffer)  ; Clear any existing content
      (insert-file-contents test-file)
      (org-mode)
      (add-id-properties-to-all-headings)
      (progn ;; Add (repeated . t) to second heading equal to '1'
        (goto-char (point-min))
        (let ((first-one-found nil))
          (while (re-search-forward "^\\*+ 1$" nil t)
            (if first-one-found
              ;; This is the second instance
                (put-text-property
                 (match-beginning 0)
                 (match-end 0)
                 'repeated t)
              (setq first-one-found t)) )) )
      (progn ;; Navigate and unfold as in docstring
        (org-overview)
        (goto-char (point-min)) ; The first heading is here.
        (org-flag-subtree nil)  ; Unfold it
        (forward-line 2)        ; Move down two lines (to '11' heading)
        (org-flag-subtree nil)  ; Unfold it
        (forward-line 1))       ; Move down to '11 body'
      (let ;; Parse and test
          ((actual-result (org-to-sexp-parse-all-branches)))
        (should (equal actual-result expected-result))))))
