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
  "Test buffer for skg-next-id and skg-previous-id tests.")

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

(ert-deftest test-skg-next-id ()
  "Test forward navigation between ID occurrences."
  (with-temp-buffer
    (insert test-skg-id-navigation-buffer)
    (goto-char (point-min))
    (skg-next-id)
    (should (equal (point) test-skg-id-navigation-line2-paren))
    (skg-next-id)
    (should (equal (point) test-skg-id-navigation-line2-bracket))
    (skg-next-id)
    (should (equal (point) test-skg-id-navigation-line3-paren))
    (let (( pos-before (point) ))
      (skg-next-id)
      (should (equal (point) pos-before)) )))

(ert-deftest test-skg-previous-id ()
  "Test backward navigation between ID occurrences."
  (with-temp-buffer
    (insert test-skg-id-navigation-buffer)
    (goto-char (point-max))
    (skg-previous-id)
    (should (equal (point) test-skg-id-navigation-line3-paren))
    (skg-previous-id)
    (should (equal (point) test-skg-id-navigation-line2-bracket))
    (skg-previous-id)
    (should (equal (point) test-skg-id-navigation-line2-paren))
    (let (( pos-before (point) ))
      (skg-previous-id)
      (should (equal (point) pos-before)) )))

(ert-deftest test-skg-push-id-to-stack ()
  "Test that skg-push-id-to-stack extracts id and label from links and metadata."
  (setq skg-id-stack nil)
  (with-temp-buffer
    (insert "* (skg (node (id 1))) [[id:2][link to 2]]\n")
    (insert "* (skg (node (id 3))) [[id:2][link to 4]] hello [[id:2][link to 5]]\n")
    (insert "* (skg (fake metadata]] [[id:fake-link)(]]\n")
    (insert "* (skg (node (id 6))) just a title\n")
    (insert "* (skg (node (id 7))) [[id:8][link to 8]]\n")
    (let* ( ( link-start
              (progn (goto-char (point-min))
                     (search-forward "[[id:2][link to 2]]")
                     (- (point) (length "[[id:2][link to 2]]")) ))
            ( link-end (point) )
            ( metadata-start
              (progn (goto-char (point-min))
                     (search-forward "(skg (node (id 6)))")
                     (- (point) (length "(skg (node (id 6)))")) ))
            ( metadata-end (point) )
            ( line3-start
              (progn (goto-char (point-min))
                     (forward-line 2)
                     (point) ))
            ( line3-end (line-end-position) ))
      (dotimes (_ 3) ;; Test 3 random positions in link
        (let (( len-before (length skg-id-stack) ))
          (goto-char (+ link-start (random (- link-end link-start))))
          (skg-push-id-to-stack)
          (should (equal (length skg-id-stack) (1+ len-before)))
          (should (equal (car skg-id-stack) '("2" "link to 2"))) ))
      (dotimes (_ 3) ;; Test 3 random positions in metadata
        (let (( len-before (length skg-id-stack) ))
          (goto-char (+ metadata-start (random (- metadata-end metadata-start))))
          (skg-push-id-to-stack)
          (should (equal (length skg-id-stack) (1+ len-before)))
          (should (equal (car skg-id-stack) '("6" "just a title"))) ))
      (let (( len-before (length skg-id-stack) )) ;; Test positions that should NOT push
        (dolist (id '("1" "3" "6" "7"))
          ;; First whitespace after )) on each line
          (goto-char (point-min))
          (search-forward (format "(skg (node (id %s)))" id))
          (skg-push-id-to-stack)
          (should (equal (length skg-id-stack) len-before)) )
        (progn (goto-char (point-min)) ;; The 'h' in "hello"
               (search-forward "hello")
               (backward-char 5)
               (should (equal (char-after) ?h))
               (skg-push-id-to-stack)
               (should (equal (length skg-id-stack) len-before)))
        (dotimes (_ 3) ;; 3 random positions on line 3
          (goto-char (+ line3-start
                        (random (- line3-end line3-start))))
          (skg-push-id-to-stack)
          (should (equal (length skg-id-stack) len-before)) )) )))

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

(ert-deftest test-skg-edit-id-stack ()
  "Test that skg-edit-id-stack creates buffer with correct content."
  (let (( skg-id-stack '(("uuid-123" "My Node")) ))
    (skg-edit-id-stack)
    (should (equal (buffer-name) "*skg-id-stack*"))
    (should (equal (buffer-string) "* My Node\nuuid-123"))
    (should skg-id-stack-mode)
    (kill-buffer "*skg-id-stack*") ))

(ert-deftest test-skg--save-id-stack-buffer ()
  "Test that saving the id-stack buffer updates skg-id-stack."
  (let (( skg-id-stack '(("old-id" "old label")) ))
    (skg-edit-id-stack)
    (erase-buffer)
    (insert "* new label\nnew-id\n* another\nanother-id\n")
    (skg--save-id-stack-buffer)
    ;; First headline = head of stack
    (should (equal skg-id-stack
                   '( ("new-id" "new label")
                      ("another-id" "another") )))
    (kill-buffer "*skg-id-stack*") ))

(provide 'test-skg-id-search)
