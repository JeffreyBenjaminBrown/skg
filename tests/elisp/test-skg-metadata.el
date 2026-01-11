;;; test-skg-metadata.el --- Tests for skg metadata parsing

(add-to-list
 'load-path
 (expand-file-name "../../elisp"
                   (file-name-directory load-file-name)))

(require 'ert)
(require 'org)
(require 'skg-metadata)
(require 'skg-compare-sexpr)

(ert-deftest test-skg-parse-headline-metadata ()
  "Test skg-parse-headline-metadata with various inputs."
  (let ;; Test title only - should return nil
      ((result (skg-parse-headline-metadata "title")))
    (should (null result)))
  (let ;; Test id and value only - should parse correctly with empty title
      ((result (skg-parse-headline-metadata "(skg (id 1) value)")))
    (should result)
    (let ((alist (car result))
          (set (cadr result))
          (title (caddr result)))
      (should (equal alist '(("id" . "1"))))
      (should (equal set '("value")))
      (should (equal title ""))))
  (let ;; Test complex metadata with title
      ((result (skg-parse-headline-metadata "(skg a b (c d) (e f)) title")))
    (should result)
    (let ((alist (car result))
          (set (cadr result))
          (title (caddr result)))
      (should (equal (sort alist (lambda (a b) (string< (car a) (car b))))
                     '(("c" . "d") ("e" . "f"))))
      (should (equal (sort set #'string<) '("a" "b")))
      (should (equal title "title")))))

(ert-deftest test-skg-parse-metadata-sexp ()
  "Test skg-parse-metadata-sexp with various inputs."

  (let ;; Test id and value
      ((result (skg-parse-metadata-sexp "(skg (id 1) value)")))
    (should result)
    (let ((alist (car result))
          (set (cadr result)))
      (should (equal alist '(("id" . "1"))))
      (should (equal set '("value")))))
  (let ;; Test complex metadata
      ((result (skg-parse-metadata-sexp "(skg a b (c d) (e f))")))
    (should result)
    (let ((alist (car result))
          (set (cadr result)))
      (should (equal (sort alist (lambda (a b) (string< (car a) (car b))))
                     '(("c" . "d") ("e" . "f"))))
      (should (equal (sort set #'string<) '("a" "b"))))))

(defun test-skg--extract-metadata-sexp ()
  "Extract and parse the (skg ...) metadata from current buffer's first line.
Returns the parsed s-expression or nil if not found."
  (goto-char (point-min))
  (when (re-search-forward "(skg[^)]*)" nil t)
    (goto-char (point-min))
    (when (search-forward "(skg" nil t)
      (let* ((start (- (point) 4))
             (text (buffer-substring-no-properties start (point-max)))
             (end-pos (skg-find-sexp-end text)))
        (when end-pos
          (read (substring text 0 end-pos)))))))

(ert-deftest test-skg-set-metadata-indefinitive ()
  "Test skg-set-metadata-indefinitive adds indefinitive to code section."
  ;; Test adding indefinitive to headline with no code section
  (with-temp-buffer
    (org-mode)
    (insert "* (skg (id 1)) title")
    (goto-char (point-min))
    (skg-set-metadata-indefinitive)
    (let ((result (test-skg--extract-metadata-sexp)))
      ;; Verify indefinitive is in code section
      (should (skg-sexp-subtree-p result '(skg (code indefinitive))))
      ;; Verify id is preserved
      (should (skg-sexp-subtree-p result '(skg (id 1))))))

  ;; Test adding indefinitive to headline with existing code section
  (with-temp-buffer
    (org-mode)
    (insert "* (skg (id 2)) title")
    (goto-char (point-min))
    (skg-set-metadata-indefinitive)
    (let ((result (test-skg--extract-metadata-sexp)))
      ;; Verify indefinitive is in code section
      (should (skg-sexp-subtree-p result '(skg (code indefinitive))))
      ;; Verify existing code content is preserved
      (should (skg-sexp-subtree-p result '(skg)))
      ;; Verify id is preserved
      (should (skg-sexp-subtree-p result '(skg (id 2))))))

  ;; Test adding indefinitive to headline with no metadata
  (with-temp-buffer
    (org-mode)
    (insert "* plain title")
    (goto-char (point-min))
    (skg-set-metadata-indefinitive)
    (let ((result (test-skg--extract-metadata-sexp)))
      ;; Verify indefinitive is in code section
      (should (skg-sexp-subtree-p result '(skg (code indefinitive)))))))

(provide 'test-skg-metadata)
