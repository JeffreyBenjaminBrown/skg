;;; test-skg-metadata.el --- Tests for skg metadata parsing

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'cl-lib)
(require 'ert)
(require 'heralds-minor-mode)
(require 'org)
(require 'skg-keymaps-and-aliases)
(require 'skg-metadata)
(require 'skg-id-search)
(require 'skg-modify-graph)
(require 'skg-compare-sexpr)
(skg-test-install-herald-rules)

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

(ert-deftest test-skg-set-write-protected ()
  "Test skg-set-write-protected adds writeProtected to node section."
  ;; Test adding write-protected to headline with id
  (with-temp-buffer
    (org-mode)
    (insert "* (skg (node (id 1))) title")
    (goto-char (point-min))
    (skg-set-write-protected)
    (let ((result (test-skg--extract-metadata-sexp)))
      ;; Verify write-protected is in node section
      (should (skg-sexp-subtree-p result '(skg (node writeProtected))))
      ;; Verify id is preserved
      (should (skg-sexp-subtree-p result '(skg (node (id 1)))))))

  ;; Test adding write-protected to headline with existing node section
  (with-temp-buffer
    (org-mode)
    (insert "* (skg (node (id 2))) title")
    (goto-char (point-min))
    (skg-set-write-protected)
    (let ((result (test-skg--extract-metadata-sexp)))
      ;; Verify write-protected is in node section
      (should (skg-sexp-subtree-p result '(skg (node writeProtected))))
      ;; Verify id is preserved
      (should (skg-sexp-subtree-p result '(skg (node (id 2)))))))

  ;; Test adding write-protected to headline with no metadata
  (with-temp-buffer
    (org-mode)
    (insert "* plain title")
    (goto-char (point-min))
    (skg-set-write-protected)
    (let ((result (test-skg--extract-metadata-sexp)))
      ;; Verify write-protected is in node section
      (should (skg-sexp-subtree-p result '(skg (node writeProtected)))))))

(ert-deftest test-skg-strip-metadata-from-org-text ()
  "Test stripping skg metadata from every headline in org text."
  (should
   (equal
    (skg-strip-metadata-from-org-text
     (concat
      "* (skg (node (id 1) (repo public))) root\n"
      "body line\n"
      "** (skg alias (staged addedR)) alias title\n"
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
  (let ((source-buffer (generate-new-buffer " *skg-test-repo*"))
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

(ert-deftest test-skg-set-merge-request-keybinding ()
  "Test C-c s m is bound to skg-set-merge-request."
  (should (eq (lookup-key skg-content-view-mode-map (kbd "C-c s m"))
              'skg-set-merge-request)))

(ert-deftest test-skg-folder-and-path-keybindings ()
  "A sample of the C-c l (folders) and C-c p (paths) bindings.
The UPPER/lower path letters select opposite roles, so C-c p O and
C-c p o must bind to distinct commands."
  (dolist (pair '(("C-c l a" . skg-show-folderOf-aliases)
                  ("C-c l o" . skg-show-folderOf-overridesViewOf)
                  ("C-c l s" . skg-show-folderOf-subscribesTo)
                  ("C-c p C" . skg-show-containerward-tree)
                  ("C-c p L" . skg-show-mentionerward-tree)
                  ("C-c p l" . skg-show-mentionedward-tree)
                  ("C-c p O" . skg-show-overriderward-tree)
                  ("C-c p o" . skg-show-overriddenward-tree)
                  ("C-c p s" . skg-show-subscribeeward-tree)))
    (should (eq (lookup-key skg-content-view-mode-map (kbd (car pair)))
                (cdr pair)))))

(ert-deftest test-skg-set-repo ()
  "Test skg-set-repo replaces the node repo field."
  (with-temp-buffer
    (org-mode)
    (insert "* (skg (node (id 1) (repo public))) title")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'skg--prompt-for-repo-change)
               (lambda (current-repo)
                 (should (equal current-repo "public"))
                 "private")))
      (skg-set-repo))
    (let ((result (test-skg--extract-metadata-sexp)))
      (should (skg-sexp-subtree-p
               result
               '(skg (node (id 1) (repo private)))))
      (should (skg-sexp-subtree-p
               result
               '(skg (node (viewStats (homeRepoHerald ⌂:private))))))
      (should-not (skg-sexp-subtree-p
                   result
                   '(skg (node (repo public))))))))

(ert-deftest test-skg-set-repo-updates-displayed-repo-herald ()
  "Test skg-set-repo changes the repo herald for the current node."
  (with-temp-buffer
    (org-mode)
    (insert "* (skg (node (id 1) (repo public) (viewStats (homeRepoHerald ⌂:public)))) title")
    (goto-char (point-min))
    (heralds-minor-mode 1)
    (cl-letf (((symbol-function 'skg--prompt-for-repo-change)
               (lambda (_current-repo)
                 "private")))
      (skg-set-repo))
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

(ert-deftest test-skg-set-merge-request-with-bare-id ()
  "Test skg-set-merge-request adds a merge editRequest."
  (with-temp-buffer
    (org-mode)
    (insert "* (skg (node (id acquirer) (repo public))) title")
    (goto-char (point-min))
    (skg-set-merge-request "acquiree")
    (let ((result (test-skg--extract-metadata-sexp)))
      (should (skg-sexp-subtree-p
               result
               '(skg (node (id acquirer) (repo public)
                           (editRequest (merge acquiree)))))))))

(ert-deftest test-skg-set-merge-request-with-link-replaces-editrequest ()
  "Test skg-set-merge-request accepts org links and replaces old editRequests."
  (with-temp-buffer
    (org-mode)
    (insert "* (skg (node (id acquirer) (repo public) (editRequest delete))) title")
    (goto-char (point-min))
    (skg-set-merge-request "[[id:acquiree][Acquiree title]]")
    (let ((result (test-skg--extract-metadata-sexp)))
      (should (skg-sexp-subtree-p
               result
               '(skg (node (editRequest (merge acquiree))))))
      (should-not (skg-sexp-subtree-p
                   result
                   '(skg (node (editRequest delete))))))))

(ert-deftest test-skg-install-id-stack-minibuffer-bindings ()
  "Test merge-request prompts install ID-stack bindings."
  (with-temp-buffer
    (use-local-map (make-sparse-keymap))
    (skg--install-id-stack-minibuffer-bindings)
    (should (eq (key-binding (kbd "C-c o i") t) 'skg-paste-id))
    (should (eq (key-binding (kbd "C-c o l") t) 'skg-paste-link))
    (should (eq (key-binding (kbd "C-c O i") t) 'skg-pop-id))
    (should (eq (key-binding (kbd "C-c O l") t) 'skg-pop-link))))

(ert-deftest test-skg-pop-link-in-minibuffer-uses-stack-title ()
  "Test popping a link in a minibuffer does not prompt for a label."
  (with-temp-buffer
    (let ((skg-id-stack '(("acquiree" "Acquiree title"))))
      (cl-letf (((symbol-function 'minibufferp)
                 (lambda (&optional _buffer) t))
                ((symbol-function 'read-string)
                 (lambda (&rest _args)
                   (error "Should not prompt for a label"))))
        (skg-pop-link))
      (should (equal (buffer-string)
                     "[[id:acquiree][Acquiree title]]"))
      (should (null skg-id-stack)))))

(ert-deftest test-skg-set-repo-recursive-prunes-non-content-affectsParent ()
  "Test recursive repo change follows only container org relationships."
  (with-temp-buffer
    (org-mode)
    (insert
     (concat
      "* (skg (node (id root) (repo public) (affectsParent na))) root\n"
      "** (skg (node (id content-child) (repo public))) content child\n"
      "*** (skg (node (id content-grandchild) (repo public))) content grandchild\n"
      "** (skg (node (id mismatched-content) (repo foreign))) mismatched content\n"
      "*** (skg (node (id public-under-mismatch) (repo public))) public under mismatch\n"
      "** (skg (node (id link-child) (repo public) (affectsParent false) (birth roleGraft mentioner))) link child\n"
      "*** (skg (node (id under-link) (repo public))) under link\n"
      "** (skg aliasFolder) aliases\n"
      "*** (skg (node (id under-non-vognode) (repo public))) under non-vognode\n"))
    (goto-char (point-min))
    (cl-letf (((symbol-function 'skg--prompt-for-repo-change)
               (lambda (current-repo)
                 (should (equal current-repo "public"))
                 "private")))
      (skg-set-repo-recursive))
    (dolist (id '("root"
                  "content-child"
                  "content-grandchild"
                  "public-under-mismatch"))
      (should (skg-sexp-subtree-p
               (test-skg--metadata-sexp-by-id id)
               '(skg (node (repo private)))))
      (should (skg-sexp-subtree-p
               (test-skg--metadata-sexp-by-id id)
               '(skg (node (viewStats (homeRepoHerald ⌂:private)))))))
    (should (skg-sexp-subtree-p
             (test-skg--metadata-sexp-by-id "mismatched-content")
             '(skg (node (repo foreign)))))
    (should-not (skg-sexp-subtree-p
                 (test-skg--metadata-sexp-by-id "mismatched-content")
                 '(skg (node (repo private)))))
    (dolist (id '("link-child"
                  "under-link"
                  "under-non-vognode"))
      (should (skg-sexp-subtree-p
               (test-skg--metadata-sexp-by-id id)
               '(skg (node (repo public)))))
      (should-not (skg-sexp-subtree-p
                   (test-skg--metadata-sexp-by-id id)
                   '(skg (node (repo private))))))))

(ert-deftest test-skg-replace-content-with-link-from-body ()
  "Test replacing the current branch from point in the node body."
  (with-temp-buffer
    (org-mode)
    (insert
     (concat
      "* (skg (node (id r) (repo public))) R\n"
      "** (skg (node (id p) (repo public))) P\n"
      "body point starts here\n"
      "*** (skg aliasFolder) aliases\n"
      "*** (skg (node (id c) (repo public) writeProtected)) child\n"
      "** (skg (node (id p) (repo public) writeProtected)) P elsewhere\n"))
    (goto-char (point-min))
    (search-forward "body point")
    (let ((save-count 0))
      (cl-letf (((symbol-function 'skg--owned-repos)
                 (lambda () '("public")))
                ((symbol-function 'skg-request-save-buffer)
                 (lambda () (setq save-count (1+ save-count)))))
        (skg-replace-content-with-link))
      (should (= save-count 1))
      (should
       (equal
        (buffer-string)
        (concat
         "* (skg (node (id r) (repo public))) R\n"
         "** [[id:p][P]]\n"
         "** (skg (node (id p) (repo public) writeProtected)) P elsewhere\n"))))))

(ert-deftest test-skg-replace-content-with-link-confirms-linked-headline ()
  "Test link-bearing headlines ask for confirmation and simplify labels."
  (with-temp-buffer
    (org-mode)
    (insert
     (concat
      "* (skg (node (id r) (repo public))) R\n"
      "** (skg (node (id p) (repo public))) P has [[https://x][X]] and [[id:y]]\n"))
    (goto-char (point-min))
    (forward-line 1)
    (let ((save-count 0))
      (cl-letf (((symbol-function 'skg--owned-repos)
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
         "* (skg (node (id r) (repo public))) R\n"
         "** [[id:p][P has X and id:y]]\n"))))))

(ert-deftest test-skg-replace-content-with-link-cancel-linked-headline ()
  "Test declining the confirmation leaves the buffer untouched."
  (with-temp-buffer
    (org-mode)
    (let ((original
           (concat
            "* (skg (node (id r) (repo public))) R\n"
            "** (skg (node (id p) (repo public))) P has [[id:x][X]]\n")))
      (insert original)
      (goto-char (point-min))
      (forward-line 1)
      (let ((save-count 0))
        (cl-letf (((symbol-function 'skg--owned-repos)
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
  "Test replacing a new node fails because it has no link dest ID."
  (with-temp-buffer
    (org-mode)
    (insert
     (concat
      "* (skg (node (id r) (repo public))) R\n"
      "** (skg (node (repo public))) P\n"))
    (goto-char (point-min))
    (forward-line 1)
    (let ((save-count 0))
      (cl-letf (((symbol-function 'skg--owned-repos)
                 (lambda () '("public")))
                ((symbol-function 'skg-request-save-buffer)
                 (lambda () (setq save-count (1+ save-count)))))
        (should-error (skg-replace-content-with-link)
                      :type 'user-error))
      (should (= save-count 0)))))

(ert-deftest test-skg-replace-content-with-link-rejects-foreign-container ()
  "Test replacement fails under a container repo not owned by the user."
  (with-temp-buffer
    (org-mode)
    (insert
     (concat
      "* (skg (node (id r) (repo foreign))) R\n"
      "** (skg (node (id p) (repo public))) P\n"))
    (goto-char (point-min))
    (forward-line 1)
    (let ((save-count 0))
      (cl-letf (((symbol-function 'skg--owned-repos)
                 (lambda () '("public")))
                ((symbol-function 'skg-request-save-buffer)
                 (lambda () (setq save-count (1+ save-count)))))
        (should-error (skg-replace-content-with-link)
                      :type 'user-error))
      (should (= save-count 0)))))

(ert-deftest test-skg-replace-content-with-link-rejects-write-protected-container ()
  "Test replacement fails under a write-protected container."
  (with-temp-buffer
    (org-mode)
    (insert
     (concat
      "* (skg (node (id r) (repo public) writeProtected)) R\n"
      "** (skg (node (id p) (repo public))) P\n"))
    (goto-char (point-min))
    (forward-line 1)
    (let ((save-count 0))
      (cl-letf (((symbol-function 'skg--owned-repos)
                 (lambda () '("public")))
                ((symbol-function 'skg-request-save-buffer)
                 (lambda () (setq save-count (1+ save-count)))))
        (should-error (skg-replace-content-with-link)
                      :type 'user-error))
      (should (= save-count 0)))))

(ert-deftest test-skg-replace-link-with-content-from-body ()
  "Test replacing a link leaf from point in the body."
  (with-temp-buffer
    (org-mode)
    (insert
     (concat
      "* (skg (node (id r) (repo public))) R\n"
      "** note\n"
      "see [[id:p][P]]\n"
      "** (skg (node (id s) (repo public))) sibling\n"))
    (goto-char (point-min))
    (search-forward "[[id:p][P]]")
    (let ((save-count 0))
      (cl-letf (((symbol-function 'skg--owned-repos)
                 (lambda () '("public")))
                ((symbol-function 'skg-request-save-buffer)
                 (lambda () (setq save-count (1+ save-count)))))
        (skg-replace-link-with-content))
      (should (= save-count 1))
      (should
       (equal
        (buffer-string)
        (concat
         "* (skg (node (id r) (repo public))) R\n"
         "** (skg (node (id p) writeProtected (viewRequests definitiveView))) P\n"
         "** (skg (node (id s) (repo public))) sibling\n"))))))

(ert-deftest test-skg-replace-link-with-content-warns-for-existing-node ()
  "Test replacing an existing node warns because it might orphan it."
  (with-temp-buffer
    (org-mode)
    (insert
     (concat
      "* (skg (node (id r) (repo public))) R\n"
      "** (skg (node (id old) (repo public))) see [[id:p][P]]\n"))
    (goto-char (point-min))
    (forward-line 1)
    (let ((save-count 0)
          (messages nil))
      (cl-letf (((symbol-function 'skg--owned-repos)
                 (lambda () '("public")))
                ((symbol-function 'skg-request-save-buffer)
                 (lambda () (setq save-count (1+ save-count))))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (skg-replace-link-with-content))
      (should (= save-count 1))
      (should
       (member
        "Warning: replacing existing node old may have created an orphan"
        messages)))))

(ert-deftest test-skg-replace-link-with-content-rejects-multiple-links ()
  "Test replacement requires exactly one link in title plus body."
  (with-temp-buffer
    (org-mode)
    (insert
     (concat
      "* (skg (node (id r) (repo public))) R\n"
      "** [[id:a][A]]\n"
      "[[id:b][B]]\n"))
    (goto-char (point-min))
    (forward-line 1)
    (let ((save-count 0))
      (cl-letf (((symbol-function 'skg--owned-repos)
                 (lambda () '("public")))
                ((symbol-function 'skg-request-save-buffer)
                 (lambda () (setq save-count (1+ save-count)))))
        (should-error (skg-replace-link-with-content)
                      :type 'user-error))
      (should (= save-count 0)))))

(ert-deftest test-skg-replace-link-with-content-rejects-non-id-link ()
  "Test replacement requires the single link to be an id link."
  (with-temp-buffer
    (org-mode)
    (insert
     (concat
      "* (skg (node (id r) (repo public))) R\n"
      "** [[https://example.com][web]]\n"))
    (goto-char (point-min))
    (forward-line 1)
    (let ((save-count 0))
      (cl-letf (((symbol-function 'skg--owned-repos)
                 (lambda () '("public")))
                ((symbol-function 'skg-request-save-buffer)
                 (lambda () (setq save-count (1+ save-count)))))
        (should-error (skg-replace-link-with-content)
                      :type 'user-error))
      (should (= save-count 0)))))

(ert-deftest test-skg-replace-link-with-content-rejects-descendents ()
  "Test replacement requires a leaf node."
  (with-temp-buffer
    (org-mode)
    (insert
     (concat
      "* (skg (node (id r) (repo public))) R\n"
      "** [[id:p][P]]\n"
      "*** child\n"))
    (goto-char (point-min))
    (forward-line 1)
    (let ((save-count 0))
      (cl-letf (((symbol-function 'skg--owned-repos)
                 (lambda () '("public")))
                ((symbol-function 'skg-request-save-buffer)
                 (lambda () (setq save-count (1+ save-count)))))
        (should-error (skg-replace-link-with-content)
                      :type 'user-error))
      (should (= save-count 0)))))

(ert-deftest test-skg-replace-link-with-content-rejects-write-protected-container ()
  "Test replacement requires a definitive container."
  (with-temp-buffer
    (org-mode)
    (insert
     (concat
      "* (skg (node (id r) (repo public) writeProtected)) R\n"
      "** [[id:p][P]]\n"))
    (goto-char (point-min))
    (forward-line 1)
    (let ((save-count 0))
      (cl-letf (((symbol-function 'skg--owned-repos)
                 (lambda () '("public")))
                ((symbol-function 'skg-request-save-buffer)
                 (lambda () (setq save-count (1+ save-count)))))
        (should-error (skg-replace-link-with-content)
                      :type 'user-error))
      (should (= save-count 0)))))

(provide 'test-skg-metadata)
