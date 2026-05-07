;;; test-skg-metadata.el --- Tests for skg metadata parsing

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'cl-lib)
(require 'ert)
(require 'heralds-minor-mode)
(require 'org)
(require 'skg-metadata)
(require 'skg-modify-graph)
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

(defun test-skg--all-metadata-sexps ()
  "Return every parsed (skg ...) metadata sexp in the current buffer."
  (let (result)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "(skg" nil t)
        (let* ((start (- (point) 4))
               (text (buffer-substring-no-properties start
                                                      (line-end-position)))
               (end-pos (skg-find-sexp-end text)))
          (when end-pos
            (push (read (substring text 0 end-pos))
                  result)))))
    (nreverse result)))

(defun test-skg--metadata-sexp-by-id (id)
  "Return the first metadata sexp whose node id is ID."
  (cl-find-if
   (lambda (sexp)
     (equal (skg-sexp-cdr-at-path sexp '(skg node id))
            (list (intern id))))
   (test-skg--all-metadata-sexps)))

(ert-deftest test-skg-set-indefinitive ()
  "Test skg-set-indefinitive adds indefinitive to node section."
  ;; Test adding indefinitive to headline with id
  (with-temp-buffer
    (org-mode)
    (insert "* (skg (node (id 1))) title")
    (goto-char (point-min))
    (skg-set-indefinitive)
    (let ((result (test-skg--extract-metadata-sexp)))
      ;; Verify indefinitive is in node section
      (should (skg-sexp-subtree-p result '(skg (node indef))))
      ;; Verify id is preserved
      (should (skg-sexp-subtree-p result '(skg (node (id 1)))))))

  ;; Test adding indefinitive to headline with existing node section
  (with-temp-buffer
    (org-mode)
    (insert "* (skg (node (id 2))) title")
    (goto-char (point-min))
    (skg-set-indefinitive)
    (let ((result (test-skg--extract-metadata-sexp)))
      ;; Verify indefinitive is in node section
      (should (skg-sexp-subtree-p result '(skg (node indef))))
      ;; Verify id is preserved
      (should (skg-sexp-subtree-p result '(skg (node (id 2)))))))

  ;; Test adding indefinitive to headline with no metadata
  (with-temp-buffer
    (org-mode)
    (insert "* plain title")
    (goto-char (point-min))
    (skg-set-indefinitive)
    (let ((result (test-skg--extract-metadata-sexp)))
      ;; Verify indefinitive is in node section
      (should (skg-sexp-subtree-p result '(skg (node indef)))))))

(ert-deftest test-skg-strip-metadata-from-org-text ()
  "Test stripping skg metadata from every headline in org text."
  (should
   (equal
    (skg-strip-metadata-from-org-text
     (concat
      "* (skg (node (id 1) (source public))) root\n"
      "body line\n"
      "** (skg alias (staged newM)) alias title\n"
      "** plain child\n"))
    (concat
     "* root\n"
     "body line\n"
     "** alias title\n"
     "** plain child\n"))))

(ert-deftest test-skg-view-without-metadata-does-nothing-without-region ()
  "Test skg-view-without-metadata does not open a buffer without a region."
  (with-temp-buffer
    (let ((before (buffer-list)))
      (insert "* (skg (node (id 1))) root")
      (goto-char (point-min))
      (skg-view-without-metadata)
      (should (equal before (buffer-list))))))

(ert-deftest test-skg-view-without-metadata-opens-stripped-region ()
  "Test skg-view-without-metadata opens a new buffer with stripped text."
  (let ((source-buffer (generate-new-buffer " *skg-test-source*"))
        (projection-buffer nil))
    (unwind-protect
        (with-current-buffer source-buffer
          (switch-to-buffer source-buffer)
          (insert
           (concat
            "* (skg (node (id 1))) root\n"
            "** (skg (node (id 2))) child\n"))
          (goto-char (point-min))
          (push-mark (point-max) nil t)
          (activate-mark)
          (skg-view-without-metadata)
          (setq projection-buffer (current-buffer))
          (should (equal (buffer-string)
                         "* root\n** child\n"))
          (should (derived-mode-p 'org-mode)))
      (when (buffer-live-p projection-buffer)
        (kill-buffer projection-buffer))
      (when (buffer-live-p source-buffer)
        (kill-buffer source-buffer)))))

(ert-deftest test-skg-set-source ()
  "Test skg-set-source replaces the node source field."
  (with-temp-buffer
    (org-mode)
    (insert "* (skg (node (id 1) (source public))) title")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'skg--prompt-for-source-change)
               (lambda (current-source)
                 (should (equal current-source "public"))
                 "private")))
      (skg-set-source))
    (let ((result (test-skg--extract-metadata-sexp)))
      (should (skg-sexp-subtree-p
               result
               '(skg (node (id 1) (source private)))))
      (should (skg-sexp-subtree-p
               result
               '(skg (node (viewStats (sourceHerald ⌂:private))))))
      (should-not (skg-sexp-subtree-p
                   result
                   '(skg (node (source public))))))))

(ert-deftest test-skg-set-source-updates-displayed-source-herald ()
  "Test skg-set-source changes the source herald for the current node."
  (with-temp-buffer
    (org-mode)
    (insert "* (skg (node (id 1) (source public) (viewStats (sourceHerald ⌂:public)))) title")
    (goto-char (point-min))
    (heralds-minor-mode 1)
    (cl-letf (((symbol-function 'skg--prompt-for-source-change)
               (lambda (_current-source)
                 "private")))
      (skg-set-source))
    (let* ((metadata-start (save-excursion
                             (goto-char (point-min))
                             (search-forward "(skg")
                             (match-beginning 0)))
           (display-overlay
            (cl-find-if (lambda (ov) (overlay-get ov 'display))
                        (overlays-at metadata-start))))
      (should display-overlay)
      (let ((display-text (overlay-get display-overlay 'display)))
        (should (string-match-p "⌂private" display-text))
        (should-not (string-match-p "⌂public" display-text))))))

(ert-deftest test-skg-set-source-recursive-prunes-non-content-births ()
  "Test recursive source change follows only contentOf org relationships."
  (with-temp-buffer
    (org-mode)
    (insert
     (concat
      "* (skg (node (id root) (source public) (birth independent))) root\n"
      "** (skg (node (id content-child) (source public))) content child\n"
      "*** (skg (node (id content-grandchild) (source public))) content grandchild\n"
      "** (skg (node (id mismatched-content) (source foreign))) mismatched content\n"
      "*** (skg (node (id public-under-mismatch) (source public))) public under mismatch\n"
      "** (skg (node (id link-child) (source public) (birth linksTo))) link child\n"
      "*** (skg (node (id under-link) (source public))) under link\n"
      "** (skg aliasCol) aliases\n"
      "*** (skg (node (id under-scaffold) (source public))) under scaffold\n"))
    (goto-char (point-min))
    (cl-letf (((symbol-function 'skg--prompt-for-source-change)
               (lambda (current-source)
                 (should (equal current-source "public"))
                 "private")))
      (skg-set-source-recursive))
    (dolist (id '("root"
                  "content-child"
                  "content-grandchild"
                  "public-under-mismatch"))
      (should (skg-sexp-subtree-p
               (test-skg--metadata-sexp-by-id id)
               '(skg (node (source private)))))
      (should (skg-sexp-subtree-p
               (test-skg--metadata-sexp-by-id id)
               '(skg (node (viewStats (sourceHerald ⌂:private)))))))
    (should (skg-sexp-subtree-p
             (test-skg--metadata-sexp-by-id "mismatched-content")
             '(skg (node (source foreign)))))
    (should-not (skg-sexp-subtree-p
                 (test-skg--metadata-sexp-by-id "mismatched-content")
                 '(skg (node (source private)))))
    (dolist (id '("link-child"
                  "under-link"
                  "under-scaffold"))
      (should (skg-sexp-subtree-p
               (test-skg--metadata-sexp-by-id id)
               '(skg (node (source public)))))
      (should-not (skg-sexp-subtree-p
                   (test-skg--metadata-sexp-by-id id)
                   '(skg (node (source private))))))))

(ert-deftest test-skg-replace-content-with-link-from-body ()
  "Test replacing the current branch from point in the node body."
  (with-temp-buffer
    (org-mode)
    (insert
     (concat
      "* (skg (node (id r) (source public))) R\n"
      "** (skg (node (id p) (source public))) P\n"
      "body point starts here\n"
      "*** (skg aliasCol) aliases\n"
      "*** (skg (node (id c) (source public) indef)) child\n"
      "** (skg (node (id p) (source public) indef)) P elsewhere\n"))
    (goto-char (point-min))
    (search-forward "body point")
    (let ((save-count 0))
      (cl-letf (((symbol-function 'skg--owned-sources)
                 (lambda () '("public")))
                ((symbol-function 'skg-request-save-buffer)
                 (lambda () (setq save-count (1+ save-count)))))
        (skg-replace-content-with-link))
      (should (= save-count 1))
      (should
       (equal
        (buffer-string)
        (concat
         "* (skg (node (id r) (source public))) R\n"
         "** [[id:p][P]]\n"
         "** (skg (node (id p) (source public) indef)) P elsewhere\n"))))))

(ert-deftest test-skg-replace-content-with-link-confirms-linked-headline ()
  "Test link-bearing headlines ask for confirmation and simplify labels."
  (with-temp-buffer
    (org-mode)
    (insert
     (concat
      "* (skg (node (id r) (source public))) R\n"
      "** (skg (node (id p) (source public))) P has [[https://x][X]] and [[id:y]]\n"))
    (goto-char (point-min))
    (forward-line 1)
    (let ((save-count 0))
      (cl-letf (((symbol-function 'skg--owned-sources)
                 (lambda () '("public")))
                ((symbol-function 'skg-request-save-buffer)
                 (lambda () (setq save-count (1+ save-count))))
                ((symbol-function 'y-or-n-p)
                 (lambda (prompt)
                   (should (equal prompt "Are you sure? "))
                   t)))
        (skg-replace-content-with-link))
      (should (= save-count 1))
      (should
       (equal
        (buffer-string)
        (concat
         "* (skg (node (id r) (source public))) R\n"
         "** [[id:p][P has X and id:y]]\n"))))))

(ert-deftest test-skg-replace-content-with-link-cancel-linked-headline ()
  "Test declining the confirmation leaves the buffer untouched."
  (with-temp-buffer
    (org-mode)
    (let ((original
           (concat
            "* (skg (node (id r) (source public))) R\n"
            "** (skg (node (id p) (source public))) P has [[id:x][X]]\n")))
      (insert original)
      (goto-char (point-min))
      (forward-line 1)
      (let ((save-count 0))
        (cl-letf (((symbol-function 'skg--owned-sources)
                   (lambda () '("public")))
                  ((symbol-function 'skg-request-save-buffer)
                   (lambda () (setq save-count (1+ save-count))))
                  ((symbol-function 'y-or-n-p)
                   (lambda (_prompt) nil)))
          (should-error (skg-replace-content-with-link)
                        :type 'user-error))
        (should (= save-count 0))
        (should (equal (buffer-string) original))))))

(ert-deftest test-skg-replace-content-with-link-rejects-missing-id ()
  "Test replacing a new node fails because it has no link target ID."
  (with-temp-buffer
    (org-mode)
    (insert
     (concat
      "* (skg (node (id r) (source public))) R\n"
      "** (skg (node (source public))) P\n"))
    (goto-char (point-min))
    (forward-line 1)
    (let ((save-count 0))
      (cl-letf (((symbol-function 'skg--owned-sources)
                 (lambda () '("public")))
                ((symbol-function 'skg-request-save-buffer)
                 (lambda () (setq save-count (1+ save-count)))))
        (should-error (skg-replace-content-with-link)
                      :type 'user-error))
      (should (= save-count 0)))))

(ert-deftest test-skg-replace-content-with-link-rejects-foreign-container ()
  "Test replacement fails under a container source not owned by the user."
  (with-temp-buffer
    (org-mode)
    (insert
     (concat
      "* (skg (node (id r) (source foreign))) R\n"
      "** (skg (node (id p) (source public))) P\n"))
    (goto-char (point-min))
    (forward-line 1)
    (let ((save-count 0))
      (cl-letf (((symbol-function 'skg--owned-sources)
                 (lambda () '("public")))
                ((symbol-function 'skg-request-save-buffer)
                 (lambda () (setq save-count (1+ save-count)))))
        (should-error (skg-replace-content-with-link)
                      :type 'user-error))
      (should (= save-count 0)))))

(ert-deftest test-skg-replace-content-with-link-rejects-indef-container ()
  "Test replacement fails under an indefinitive container."
  (with-temp-buffer
    (org-mode)
    (insert
     (concat
      "* (skg (node (id r) (source public) indef)) R\n"
      "** (skg (node (id p) (source public))) P\n"))
    (goto-char (point-min))
    (forward-line 1)
    (let ((save-count 0))
      (cl-letf (((symbol-function 'skg--owned-sources)
                 (lambda () '("public")))
                ((symbol-function 'skg-request-save-buffer)
                 (lambda () (setq save-count (1+ save-count)))))
        (should-error (skg-replace-content-with-link)
                      :type 'user-error))
      (should (= save-count 0)))))

(provide 'test-skg-metadata)
