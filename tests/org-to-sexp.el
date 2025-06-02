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
               (body . "1 body")
               (content
                . (((heading . "11")
                    (id . "11")
                    (body . "11 body")
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
                     (body . "2 body")
                     (folded . t)
                     (content
                      . (((heading . "21")
                          (id . "21")
                          (folded . t))
                         ((heading . "22")
                          (id . "22")
                          (folded . t)))))))))
    (with-temp-buffer
      (insert-file-contents test-file)
      (org-mode)

      ;; Add id properties to each heading
      (goto-char (point-min))
      (while (re-search-forward "^\\*+ \\(.+\\)$" nil t)
        (let ((heading-text (match-string 1))
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
            nil))))))

;; Run the test
(test-org-to-sexp-parse-all-branches)
