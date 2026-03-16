;;; test-skg-id-search.el --- Tests for skg-id-search functions

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'skg-id-search)

(defconst test-skg-id-navigation-buffer
  (concat "* no id\n"
          "* (skg (node (id 1))) 1 has a [[id:2][link to 2]]\n"
          "* (skg (node (id 2))) 2\n"
          "* no id\n")
  "Test buffer for skg-id-next and skg-id-prev tests.")

(defconst test-skg-id-navigation-line2-paren
  (with-temp-buffer
    (insert test-skg-id-navigation-buffer)
    (goto-char (point-min))
    (forward-line 1)
    (search-forward "(")
    (1- (point)))
  "Position of first paren on line 2 of test-skg-id-navigation-buffer.")

(defconst test-skg-id-navigation-line2-bracket
  (with-temp-buffer
    (insert test-skg-id-navigation-buffer)
    (goto-char (point-min))
    (forward-line 1)
    (search-forward "[")
    (1- (point)))
  "Position of first bracket on line 2 of test-skg-id-navigation-buffer.")

(defconst test-skg-id-navigation-line3-paren
  (with-temp-buffer
    (insert test-skg-id-navigation-buffer)
    (goto-char (point-min))
    (forward-line 2)
    (search-forward "(")
    (1- (point)))
  "Position of first paren on line 3 of test-skg-id-navigation-buffer.")

(ert-deftest test-skg-id-next ()
  "Test forward navigation between ID occurrences."
  (with-temp-buffer
    (insert test-skg-id-navigation-buffer)
    (goto-char (point-min))
    (skg-id-next)
    (should (equal (point) test-skg-id-navigation-line2-paren))
    (skg-id-next)
    (should (equal (point) test-skg-id-navigation-line2-bracket))
    (skg-id-next)
    (should (equal (point) test-skg-id-navigation-line3-paren))
    (let (( pos-before (point) ))
      (skg-id-next)
      (should (equal (point) pos-before)) )))

(ert-deftest test-skg-id-prev ()
  "Test backward navigation between ID occurrences."
  (with-temp-buffer
    (insert test-skg-id-navigation-buffer)
    (goto-char (point-max))
    (skg-id-prev)
    (should (equal (point) test-skg-id-navigation-line3-paren))
    (skg-id-prev)
    (should (equal (point) test-skg-id-navigation-line2-bracket))
    (skg-id-prev)
    (should (equal (point) test-skg-id-navigation-line2-paren))
    (let (( pos-before (point) ))
      (skg-id-prev)
      (should (equal (point) pos-before)) )))

(ert-deftest test-skg-id-push ()
  "Test that skg-id-push pushes the metadata ID from the current line."
  (setq skg-id-stack nil)
  (with-temp-buffer
    (insert "* (skg (node (id 1))) [[id:2][link to 2]]\n")
    (insert "* (skg (node (id 3))) [[id:2][link to 4]] hello [[id:2][link to 5]]\n")
    (insert "* (skg (fake metadata]] [[id:fake-link)(]]\n")
    (insert "* (skg (node (id 6))) just a title\n")
    (insert "* (skg (node (id 7))) [[id:8][link to 8]]\n")
    (let* (( line3-start
             (progn (goto-char (point-min))
                    (forward-line 2)
                    (point)) )
           ( line3-end (line-end-position) ))
      (progn ;; Anywhere on a metadata line pushes that line's metadata ID
        (dolist (id '("1" "3" "6" "7"))
          (let (( len-before (length skg-id-stack) ))
            (goto-char (point-min))
            (search-forward (format "(skg (node (id %s)))" id))
            (skg-id-push)
            (should (equal (length skg-id-stack) (1+ len-before)))
            (should (equal (caar skg-id-stack) id)) )) )
      (progn ;; From the title area, still finds metadata ID
        (let (( len-before (length skg-id-stack) ))
          (goto-char (point-min))
          (search-forward "just a title")
          (backward-char 5)
          (skg-id-push)
          (should (equal (length skg-id-stack) (1+ len-before)))
          (should (equal (car skg-id-stack) '("6" "just a title"))) ))
      (let (( len-before (length skg-id-stack) )) ;; Line 3: fake metadata — should NOT push
        (dotimes (_ 3)
          (goto-char (+ line3-start
                        (random (- line3-end line3-start))))
          (skg-id-push)
          (should (equal (length skg-id-stack) len-before)) )) )))

(ert-deftest test-skg-id-push-and-view-stack ()
  "Test pushing IDs from a buffer and viewing the stack."
  (setq skg-id-stack nil)
  (with-temp-buffer
    (org-mode)
    (insert "* (skg (node (id a))) a\n")
    (insert "** (skg (node (id b))) b has a [[id:a][link to a]]\n")
    (insert "* (skg (node (id c))) c\n")
    (progn ;; Push from start of buffer (line 1, id=a)
      (goto-char (point-min))
      (skg-id-push))
    (progn ;; Push from last char of line 2 (id=b)
      (goto-char (point-min))
      (forward-line 1)
      (end-of-line)
      (skg-id-push))
    (should (equal (length skg-id-stack) 2))
    (should (equal (car skg-id-stack) '("b" "b has a [[id:a][link to a]]")))
    (should (equal (cadr skg-id-stack) '("a" "a"))) ))

(ert-deftest test-skg-validate-id-stack-buffer_valid-input ()
  "Test skg-validate-id-stack-buffer with valid inputs."
  (with-temp-buffer ;; Single headline
    (insert "* the label\n")
    (insert "  the-id\n")
    (should (equal (skg-validate-id-stack-buffer)
                   '(success (("the-id" "the label"))) )))
  (with-temp-buffer ;; Two headlines. First headline = head of stack.
    (insert "* the label\n")
    (insert "  the-id\n")
    (insert "* label 2\n")
    (insert "  id-2\n")
    (should (equal (skg-validate-id-stack-buffer)
                   '(success ( ("the-id" "the label")
                               ("id-2" "label 2") )))) ))

(ert-deftest test-skg-validate-id-stack-buffer_invalid-input ()
  "Test skg-validate-id-stack-buffer rejects invalid inputs."
  (with-temp-buffer ;; Text before first headline
    (insert "Hello!\n")
    (insert "* okay\n")
    (insert "  okay-id\n")
    (should (eq (car (skg-validate-id-stack-buffer)) 'error)) )
  (with-temp-buffer ;; looks like a headline but has no title
    (insert "* \n")
    (insert "  okay-id\n")
    (should (eq (car (skg-validate-id-stack-buffer)) 'error)) )
  (with-temp-buffer ;; Headline without body
    (insert "* okay\n")
    (insert "  okay-id\n")
    (insert "* label without ID\n")
    (insert "* okay\n")
    (insert "  okay-id\n")
    (should (eq (car (skg-validate-id-stack-buffer)) 'error)) )
  (with-temp-buffer ;; Headline with multi-line body
    (insert "* okay\n")
    (insert "  okay-id\n")
    (insert "* label with too much ID stuff\n")
    (insert "  an-id\n")
    (insert "  invalid-extra-stuff\n")
    (insert "* okay\n")
    (insert "  okay-id\n")
    (should (eq (car (skg-validate-id-stack-buffer)) 'error)) ))

(ert-deftest test-skg-replace-id-stack-from-buffer_empty-inputs ()
  "Test that empty/whitespace buffers result in empty stack."
  (dolist (content '( ""
                      "\n"
                      "\n\n"
                      " \n"
                      " \n \n" ))
    (setq skg-id-stack '(("old" "stuff"))) ;; Start with non-empty stack
    (with-temp-buffer
      (insert content)
      (skg-replace-id-stack-from-buffer) )
    (should (equal skg-id-stack nil)) ))

(ert-deftest test-skg-replace-id-stack-from-buffer_single-headline ()
  "Test single headline with body."
  (dolist (content
           '( "*   label\n  id\n" ;; leading space is ignored
              "* label\nid\n" )) ;; leading space is not needed, except the bit after the bullet
    (setq skg-id-stack nil)
    (with-temp-buffer
      (insert content)
      (skg-replace-id-stack-from-buffer) )
    (should (equal skg-id-stack '(("id" "label")) )) ))

(ert-deftest test-skg-replace-id-stack-from-buffer_two-headlines ()
  "Test two headlines - first headline = head of stack."
  (setq skg-id-stack nil)
  (with-temp-buffer
    (insert "* label\nid\n* label-2\n  id-2\n")
    (skg-replace-id-stack-from-buffer) )
  (should (equal skg-id-stack
                 '( ("id" "label")
                    ("id-2" "label-2") )) ))

(ert-deftest test-skg--format-id-stack-as-org ()
  "Test that skg--format-id-stack-as-org produces correct org format."
  (let (( skg-id-stack nil )) ;; Empty stack
    (should (equal (skg--format-id-stack-as-org) "")) )
  (let (( skg-id-stack '(("id-1" "label one")) )) ;; Single item
    (should (equal (skg--format-id-stack-as-org) "* label one\nid-1")) )
  (let (( skg-id-stack '(("id-2" "second") ("id-1" "first")) )) ;; Two items
    ;; Head of stack (most recent) appears first in buffer
    (should (equal (skg--format-id-stack-as-org)
                   "* second\nid-2\n* first\nid-1")) ))

(ert-deftest test-skg-id-stack ()
  "Test that skg-id-stack creates buffer with correct content."
  (let (( skg-id-stack '(("uuid-123" "My Node")) ))
    (skg-id-stack)
    (should (equal (buffer-name) "*skg-id-stack*"))
    (should (equal (buffer-string) "* My Node\nuuid-123"))
    (should skg-id-stack-mode)
    (kill-buffer "*skg-id-stack*") ))

(ert-deftest test-skg--save-id-stack-buffer ()
  "Test that saving the id-stack buffer updates skg-id-stack."
  (let (( skg-id-stack '(("old-id" "old label")) ))
    (skg-id-stack)
    (erase-buffer)
    (insert "* new label\nnew-id\n* another\nanother-id\n")
    (skg--save-id-stack-buffer)
    ;; First headline = head of stack
    (should (equal skg-id-stack
                   '( ("new-id" "new label")
                      ("another-id" "another") )))
    (kill-buffer "*skg-id-stack*") ))

(provide 'test-skg-id-search)
