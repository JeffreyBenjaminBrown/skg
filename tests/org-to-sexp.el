(require 'org-to-sexp)

(defun test-org-to-sexp-parse-all-branches ()
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

  (interactive)
  (let ((test-file
         (expand-file-name
          "org-to-sexp/fixtures/1.org"
          (file-name-directory
           (or load-file-name buffer-file-name))))
        (expected-result
         `(content
           . (((heading . "1")
               (id . "1")
               (body . "  1 body")
               (content
                . (((heading . "11")
                    (id . "11")
                    (body . "   11 body")
                    (focused . t))
                   ((heading . "12")
                    (id . "12")
                    (folded . t)
                    (content
                     . (((heading . "121")
                         (id . "121")
                         (folded . t) )
                        ((heading . "1")
                         (id . "1")
                         (folded . t)
                         (repeated . t))))))))
                    ((heading . "2")
                     (id . "2")
                     (body . "  2 body")
                     (folded . t)
                     (content
                      . (((heading . "21")
                          (id . "21")
                          (folded . t))
                         ((heading . "22")
                          (id . "22")
                          (folded . t)))))))))

    (with-current-buffer
        (get-buffer-create "*test-org-to-sexp*")
      (erase-buffer)  ; Clear any existing content
      (insert-file-contents test-file)
      (org-mode)

      ;; Add id properties to each heading
      (goto-char (point-min))
      (while (re-search-forward "^\\*+ \\(.+\\)$" nil t)
        (let ((heading-text
               (match-string-no-properties 1))
              (heading-start (match-beginning 1))
              (heading-end (match-end 1)))
          (put-text-property
           heading-start heading-end 'id heading-text)))

      ;; Add (repeated . t) to second heading equal to '1'
      (goto-char (point-min))
      (let ((first-one-found nil))
        (while (re-search-forward "^\\*+ 1$" nil t)
          (if first-one-found
              ;; This is the second instance
              (put-text-property
               (match-beginning 0)
               (match-end 0)
               'repeated t)
            (setq first-one-found t))))

      ;; Navigate and unfold as described
      (org-overview)
      (goto-char (point-min)) ; The first heading is here.
      (org-flag-subtree nil)  ; Unfold it
      (forward-line 2)        ; Move down two lines (to '11' heading)
      (org-flag-subtree nil)  ; Unfold it
      (forward-line 1)        ; Move down one more line (to '11 body')

      ;; Parse and test
      (let ((actual-result (org-to-sexp-parse-all-branches)))
        (if (equal actual-result expected-result)
            (message "✓ Test passed: org-to-sexp-parse-all-branches works correctly")
          (progn
            (message "✗ Test failed: Results don't match")
            (message "Expected: %S" expected-result)
            (message "Actual: %S" actual-result)
            (message "Difference: %S"
                     (alist-diff ;; Neither result is a true alist (which is a list of cons cells), but rather a single cons cell with `content` as the car, so we compare their cdrs.
                      (cdr expected-result)
                      (cdr actual-result)))
            nil))))))

(defun test-first-property-on-line ()
  "Test the first-property-on-line function with various text properties."
  (interactive)
  (with-temp-buffer
    (org-mode)

    ;; Insert the test data
    (insert "line one\n")
    (insert "line two\n")
    (insert "line three\n")

    ;; Set up properties on line 1
    (goto-char (point-min))
    ;; Find "line" in first line and attach property "one" with value "line"
    (when (search-forward "line" (line-end-position) t)
      (put-text-property (match-beginning 0) (match-end 0) 'one "line"))
    ;; Find "one" in first line and attach property "one" with value "one"
    (when (search-forward "one" (line-end-position) t)
      (put-text-property (match-beginning 0) (match-end 0) 'one "one"))

    ;; Set up properties on line 2
    (goto-char (point-min))
    (forward-line 1)
    (when (search-forward "line" (line-end-position) t)
      (put-text-property (match-beginning 0) (match-end 0) 'other t))

    ;; Set up properties on line 3
    (goto-char (point-min))
    (forward-line 2)
    (when (search-forward "line" (line-end-position) t)
      (put-text-property (match-beginning 0) (match-end 0) 'three "line"))
    ;; Reset search position for this line
    (beginning-of-line)
    (when (search-forward "three" (line-end-position) t)
      (put-text-property (match-beginning 0) (match-end 0) 'three "three"))

    ;; Now run the tests
    (let ((results '())
          (expected '(("line" nil "line"))))

      ;; Test 1: Line 1, beginning, search for property "one"
      (goto-char (point-min))
      (let ((result1 (first-property-on-line 'one)))
        (push result1 results)
        (message "Line 1, property 'one': %S (expected: \"line\")" result1))

      ;; Test 2: Line 2, middle (space), search for property "two"
      (goto-char (point-min))
      (forward-line 1)
      (forward-char 4) ; Move to the space between "line" and "two"
      (let ((result2 (first-property-on-line 'two)))
        (push result2 results)
        (message "Line 2, property 'two': %S (expected: nil)" result2))

      ;; Test 3: Line 3, middle (space), search for property "three"
      (goto-char (point-min))
      (forward-line 2)
      (forward-char 4) ; Move to the space between "line" and "three"
      (let ((result3 (first-property-on-line 'three)))
        (push result3 results)
        (message "Line 3, property 'three': %S (expected: \"line\")" result3))

      ;; Check results
      (setq results (nreverse results))
      (let ((test1-pass (equal (nth 0 results) "line"))
            (test2-pass (equal (nth 1 results) nil))
            (test3-pass (equal (nth 2 results) "line")))

        (if (and test1-pass test2-pass test3-pass)
            (message "✓ All tests passed!")
          (progn
            (message "✗ Some tests failed:")
            (unless test1-pass
              (message "  Test 1 failed: got %S, expected \"line\"" (nth 0 results)))
            (unless test2-pass
              (message "  Test 2 failed: got %S, expected nil" (nth 1 results)))
            (unless test3-pass
              (message "  Test 3 failed: got %S, expected \"line\"" (nth 2 results)))))

        (message "Final results: %S" results)
        (and test1-pass test2-pass test3-pass)))))

(test-org-to-sexp-parse-all-branches)
(test-first-property-on-line)
