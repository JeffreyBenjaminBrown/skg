;;; test-skg-metadata.el --- Tests for skg metadata parsing

(add-to-list
 'load-path
 (expand-file-name "../../elisp"
                   (file-name-directory load-file-name)))

(require 'ert)
(require 'org)
(require 'skg-metadata)

(ert-deftest test-skg-parse-headline-metadata ()
  "Test skg-parse-headline-metadata with various inputs."
  (let ;; Test title only - should return nil
      ((result (skg-parse-headline-metadata "title")))
    (should (null result)))
  (let ;; Test id and value only - should parse correctly with empty title
      ((result (skg-parse-headline-metadata "<skg<id:1,value>>")))
    (should result)
    (let ((alist (car result))
          (set (cadr result))
          (title (caddr result)))
      (should (equal alist '(("id" . "1"))))
      (should (equal set '("value")))
      (should (equal title ""))))
  (let ;; Test complex metadata with title
      ((result (skg-parse-headline-metadata "<skg<a,b,c:d,e:f>> title")))
    (should result)
    (let ((alist (car result))
          (set (cadr result))
          (title (caddr result)))
      (should (equal (sort alist (lambda (a b) (string< (car a) (car b))))
                     '(("c" . "d") ("e" . "f"))))
      (should (equal (sort set #'string<) '("a" "b")))
      (should (equal title "title")))))

(ert-deftest test-skg-parse-metadata-inner ()
  "Test skg-parse-metadata-inner with various inputs."

  (let ;; Test id and value
      ((result (skg-parse-metadata-inner "id:1,value")))
    (should result)
    (let ((alist (car result))
          (set (cadr result)))
      (should (equal alist '(("id" . "1"))))
      (should (equal set '("value")))))
  (let ;; Test complex metadata
      ((result (skg-parse-metadata-inner "a,b,c:d,e:f")))
    (should result)
    (let ((alist (car result))
          (set (cadr result)))
      (should (equal (sort alist (lambda (a b) (string< (car a) (car b))))
                     '(("c" . "d") ("e" . "f"))))
      (should (equal (sort set #'string<) '("a" "b"))))))

(ert-deftest test-skg-get-current-headline-metadata ()
  "Test skg-get-current-headline-metadata with various buffer contents."

  (with-temp-buffer ;; metadata and title
    (org-mode)
    (insert "** <skg<id:test123,val>> title")
    (goto-char (point-min))
    (let ((result (skg-get-current-headline-metadata)))
      (should result)
      (should (equal (car result) "test123"))  ; id
      (should (equal (cadr result) 2))         ; level
      (should (equal (caddr result) "title")))) ; title
  (with-temp-buffer ;; metadata but empty title
    (org-mode)
    (insert "** <skg<id:test456,val>>")
    (goto-char (point-min))
    (let ((result (skg-get-current-headline-metadata)))
      (should result)
      (should (equal (car result) "test456"))  ; id
      (should (equal (cadr result) 2))         ; level
      (should (equal (caddr result) ""))))     ; empty titl
  (with-temp-buffer ;; should error (not a headline)
    (org-mode)
    (insert "title")
    (goto-char (point-min))
    (should-error (skg-get-current-headline-metadata)
                  :type 'error))
  (with-temp-buffer ;; should error (not a headline)
    (org-mode)
    (insert "<skg<id:test,val>>")
    (goto-char (point-min))
    (should-error (skg-get-current-headline-metadata)
                  :type 'error))
  (with-temp-buffer ;; should error (not a headline)
    (org-mode)
    (insert " <skg<id:test,val>>")
    (goto-char (point-min))
    (should-error (skg-get-current-headline-metadata)
                  :type 'error)))

(provide 'test-skg-metadata)
