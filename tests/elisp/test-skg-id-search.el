;;; test-skg-id-search.el --- Tests for skg-id-search functions

(add-to-list
 'load-path
 (expand-file-name "../../elisp"
                   (file-name-directory load-file-name)))

(require 'ert)
(require 'skg-id-search)

(ert-deftest test-skg-next-and-previous-id ()
  "Test navigation between ID occurrences with skg-next-id and skg-previous-id."
  (with-temp-buffer
    (insert "* (skg (id 1)) [[id:2][link to 2]]\n")
    (insert "* (skg (id 3)) [[id:2][link to 4]] hello [[id:2][link to 5]]\n")
    (insert "* (skg (fake metadata]] [[id:fake-link)(]]\n")
    (insert "* (skg (id 6)) just a title\n")
    (insert "* (skg (id 7)) [[id:8][link to 8]]\n")
    (goto-char (point-min))
    (search-forward "hello")
    (backward-char 5) ;; now on the 'h' in 'hello'
    (should (equal (char-after) ?h))
    (let ( ( line2-skg-start
             (save-excursion
               (goto-char (point-min))
               (forward-line 1)
               (forward-char 2)
               (point) ))
           ( line2-link-to-5-start
             (save-excursion
               (goto-char (point-min))
               (search-forward "[[id:2][link to 5]]")
               (- (point) (length "[[id:2][link to 5]]")) ))
           ( line3-skg-start
             (save-excursion
               (goto-char (point-min))
               (forward-line 3)
               (forward-char 2)
               (point) )) )
      (skg-next-id)
      (should (equal (point) line2-link-to-5-start))
      (skg-next-id)
      (should (equal (point) line3-skg-start))
      (skg-previous-id)
      (skg-previous-id)
      (skg-previous-id)
      (should (equal (point) line2-skg-start)) )))

(ert-deftest test-push-sides-of-line-to-id-stack ()
  "Test that push-sides-of-line-to-id-stack pushes correct pairs."
  (setq skg-id-stack nil)
  (with-temp-buffer
    (insert "a b c d e\n")
    (insert "f g h i j")
    (goto-char (point-min))
    (search-forward "c")
    (backward-char 1) ;; now on 'c'
    (should (equal (char-after) ?c))
    (push-sides-of-line-to-id-stack)
    (search-forward "g")
    (backward-char 1) ;; now on 'g'
    (should (equal (char-after) ?g))
    (push-sides-of-line-to-id-stack) )
  (should (equal skg-id-stack
                 '( ("f " "g h i j")
                    ("a b " "c d e") )) ))

(provide 'test-skg-id-search)
