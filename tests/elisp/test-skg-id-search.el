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

(ert-deftest test-skg-push-id-to-stack ()
  "Test that skg-push-id-to-stack extracts id and label from links and metadata."
  (setq skg-id-stack nil)
  (with-temp-buffer
    (insert "* (skg (id 1)) [[id:2][link to 2]]\n")
    (insert "* (skg (id 3)) [[id:2][link to 4]] hello [[id:2][link to 5]]\n")
    (insert "* (skg (fake metadata]] [[id:fake-link)(]]\n")
    (insert "* (skg (id 6)) just a title\n")
    (insert "* (skg (id 7)) [[id:8][link to 8]]\n")
    (let* ( ( link-start
              (progn (goto-char (point-min))
                     (search-forward "[[id:2][link to 2]]")
                     (- (point) (length "[[id:2][link to 2]]")) ))
            ( link-end (point) )
            ( metadata-start
              (progn (goto-char (point-min))
                     (search-forward "(skg (id 6))")
                     (- (point) (length "(skg (id 6))")) ))
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
          (search-forward (format "(skg (id %s))" id))
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
