;;; test-skg-metadata.el --- Tests for skg metadata parsing

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'cl-lib)
(require 'ert)
(require 'heralds-minor-mode)
(require 'org)
(require 'skg-keymaps-and-aliases)
(require 'skg-metadata)
(require 'skg-request-views)
(require 'skg-id-search)
(require 'skg-modify-graph)
(require 'skg-compare-sexpr)
(skg-test-install-herald-rules)

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

(defun test-skg--metadata-sexp-by-id (gnid)
  "Return the first metadata sexp whose node id is ID."
  (cl-find-if
   (lambda (sexp)
     (equal (skg-sexp-cdr-at-path sexp '(skg gnid))
            (list (intern gnid))))
   (test-skg--all-metadata-sexps)))

(ert-deftest test-skg-request-write-protected ()
  "The write-protect commands add their view request without saving,
and leave the headline's own metadata alone."
  (dolist (pair '((skg-request-write-protected . writeProtectedView)
                  (skg-request-write-protected-recursive
                   . writeProtectedSubtreeView)))
    (cl-letf (((symbol-function 'skg-request-save-buffer)
               (lambda () (error "The command must not save"))))
      (with-temp-buffer
        (org-mode)
        (insert "* (skg node (gnid 1)) title")
        (goto-char (point-min))
        (funcall (car pair))
        (let ((result (test-skg--extract-metadata-sexp)))
          (should (skg-sexp-subtree-p
                   result `(skg node (viewRequests ,(cdr pair)))))
          (should (skg-sexp-subtree-p result '(skg node (gnid 1))))
          (should-not (skg-sexp-subtree-p
                       result '(skg node writeProtected))))))))

(ert-deftest test-skg-write-protect-keybindings ()
  "C-c s w and C-c s W request write-protection."
  (should (eq (lookup-key skg-content-view-mode-map (kbd "C-c s w"))
              'skg-request-write-protected))
  (should (eq (lookup-key skg-content-view-mode-map (kbd "C-c s W"))
              'skg-request-write-protected-recursive)))

(ert-deftest test-skg-strip-metadata-from-org-text ()
  "Test stripping skg metadata from every headline in org text."
  (should
   (equal
    (skg-strip-metadata-from-org-text
     (concat
      "* (skg node (gnid 1) (skgRepos (nodeRepo public))) root\n"
      "body line\n"
      "** (skg alias (diff (rel added))) alias title\n"
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
      (insert "* (skg node (gnid 1)) root")
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
            "* (skg node (gnid 1)) root\n"
            "** (skg node (gnid 2)) child\n"))
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

(ert-deftest test-skg-folder-keybindings ()
  "A sample of the C-c l (folders) bindings. No C-c p prefix is bound:
the folder commands are the way to show a node's partners."
  (dolist (pair '(("C-c l a" . skg-show-folderOf-aliases)
                  ("C-c l o" . skg-show-folderOf-overrides)
                  ("C-c l s" . skg-show-folderOf-subscribesTo)
                  ("C-c l c" . skg-show-folderOf-containers)
                  ("C-c l m" . skg-show-folderOf-mentioners)))
    (should (eq (lookup-key skg-content-view-mode-map (kbd (car pair)))
                (cdr pair))))
  (should-not (lookup-key skg-content-view-mode-map (kbd "C-c p"))))

(ert-deftest test-skg-folder-change-readiness ()
  "A folder is changed when a member carries a change of its
relationship to the folder's viewparent, and ready when every such
change is. The cases the server pinned when it computed this."
  (let ((ready-member '(skg node (gnid m) (diff (rel added) (leaves 2 node rel) (ready rel))))
        (unready-member '(skg node (gnid m) (diff (rel added) (leaves 2 node rel) (ready node))))
        (ready-unknown '(skg unknown (gnid u) (diff (rel added) (leaves 1) (ready))))
        (unready-alias '(skg alias noClaim (view ghostWarrant) (diff (rel removed))))
        (unchanged '(skg node (gnid n) (diff (node added) (ready node)))))
    (cl-flet ((readiness (&rest member-sexps)
                (skg--folder-change-readiness
                 (mapcar #'list member-sexps))))
      (should (eq (readiness ready-member) 'ready))
      (should (eq (readiness unready-member) 'unready))
      (should (eq (readiness ready-unknown) 'ready))
      (should (eq (readiness unready-alias) 'unready))
      (should (null (readiness unchanged)))
      (should (eq (readiness ready-member ready-unknown unchanged) 'ready))
      (should (eq (readiness ready-member unready-member ready-unknown
                             unready-alias)
                  'unready))))
  ;; A '(relRepoMove ...)' child changes its member's relationship too.
  (let ((member '(skg node (gnid m))))
    (should (eq (skg--folder-change-readiness
                 (list (list member '(skg (relRepoMove (old a) (new b))
                                          (diff (leaves 1) (ready))))))
                'ready))
    (should (eq (skg--folder-change-readiness
                 (list (list member '(skg (relRepoMove (old a) (new b))
                                          (diff (leaves 1))))))
                'unready))
    ;; Both must be ready.
    (should (eq (skg--folder-change-readiness
                 (list (list '(skg node (gnid m) (diff (rel added) (leaves 1 rel) (ready rel)))
                             '(skg (relRepoMove (old a) (new b))
                                   (diff (leaves 1))))))
                'unready))))

(ert-deftest test-skg-set-repo ()
  "Test skg-set-repo replaces the node repo field, and writes no
derived fact: the save recomputes '(view repoBoundary)'."
  (with-temp-buffer
    (org-mode)
    (insert "* (skg node (gnid 1) (skgRepos (nodeRepo public))) title")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'skg--prompt-for-repo-change)
               (lambda (current-skgrepo)
                 (should (equal current-skgrepo "public"))
                 "private")))
      (skg-set-repo))
    (let ((result (test-skg--extract-metadata-sexp)))
      (should (skg-sexp-subtree-p
               result
               '(skg node (gnid 1) (skgRepos (nodeRepo private)))))
      (should-not (assq 'view (cdr result)))
      (should-not (skg-sexp-subtree-p
                   result
                   '(skg node (skgRepos (nodeRepo public))))))))

(ert-deftest test-skg-set-repo-leaves-the-repo-boundary-to-the-save ()
  "Test skg-set-repo leaves the node's '(view repoBoundary)', and so
its herald, as they were: the save recomputes them."
  (with-temp-buffer
    (org-mode)
    (insert "* (skg node (gnid 1) (skgRepos (nodeRepo public)) (view repoBoundary)) title")
    (goto-char (point-min))
    (heralds-minor-mode 1)
    (cl-letf (((symbol-function 'skg--prompt-for-repo-change)
               (lambda (_current-skgrepo)
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
      (should (equal (test-skg--extract-metadata-sexp)
                     '(skg node (gnid 1) (skgRepos (nodeRepo private))
                           (view repoBoundary))))
      (should (string-match-p "⌂" (overlay-get display-overlay 'display))))))

(ert-deftest test-skg-set-merge-request-with-bare-id ()
  "Test skg-set-merge-request adds a merge editRequest."
  (with-temp-buffer
    (org-mode)
    (insert "* (skg node (gnid acquirer) (skgRepos (nodeRepo public))) title")
    (goto-char (point-min))
    (skg-set-merge-request "acquiree")
    (let ((result (test-skg--extract-metadata-sexp)))
      (should (skg-sexp-subtree-p
               result
               '(skg node (gnid acquirer) (skgRepos (nodeRepo public))
                     (editRequest (merge acquiree))))))))

(ert-deftest test-skg-set-merge-request-with-link-replaces-editrequest ()
  "Test skg-set-merge-request accepts org links and replaces old editRequests."
  (with-temp-buffer
    (org-mode)
    (insert "* (skg node (gnid acquirer) (skgRepos (nodeRepo public)) (editRequest delete)) title")
    (goto-char (point-min))
    (skg-set-merge-request "[[id:acquiree][Acquiree title]]")
    (let ((result (test-skg--extract-metadata-sexp)))
      (should (skg-sexp-subtree-p
               result
               '(skg node (editRequest (merge acquiree)))))
      (should-not (skg-sexp-subtree-p
                   result
                   '(skg node (editRequest delete)))))))

(ert-deftest test-skg-install-linkstack-minibuffer-bindings ()
  "Test merge-request prompts install linkstack bindings."
  (with-temp-buffer
    (use-local-map (make-sparse-keymap))
    (skg--install-linkstack-minibuffer-bindings)
    (should (eq (key-binding (kbd "C-c o i") t) 'skg-paste-id))
    (should (eq (key-binding (kbd "C-c o l") t) 'skg-paste-link))
    (should (eq (key-binding (kbd "C-c O i") t) 'skg-pop-id))
    (should (eq (key-binding (kbd "C-c O l") t) 'skg-pop-link))))

(ert-deftest test-skg-pop-link-in-minibuffer-uses-stack-title ()
  "Test popping a link in a minibuffer does not prompt for a label."
  (with-temp-buffer
    (let ((skg-linkstack '(("acquiree" "Acquiree title"))))
      (cl-letf (((symbol-function 'minibufferp)
                 (lambda (&optional _buffer) t))
                ((symbol-function 'read-string)
                 (lambda (&rest _args)
                   (error "Should not prompt for a label"))))
        (skg-pop-link))
      (should (equal (buffer-string)
                     "[[id:acquiree][Acquiree title]]"))
      (should (null skg-linkstack)))))

(ert-deftest test-skg-set-repo-recursive-prunes-noClaim-children ()
  "Test recursive repo change follows only container org relationships."
  (with-temp-buffer
    (org-mode)
    (insert
     (concat
      "* (skg node (gnid root) (skgRepos (nodeRepo public))) root\n"
      "** (skg node (gnid content-child) (skgRepos (nodeRepo public))) content child\n"
      "*** (skg node (gnid content-grandchild) (skgRepos (nodeRepo public))) content grandchild\n"
      "** (skg node (gnid mismatched-content) (skgRepos (nodeRepo foreign))) mismatched content\n"
      "*** (skg node (gnid public-under-mismatch) (skgRepos (nodeRepo public))) public under mismatch\n"
      "** (skg node (gnid link-child) (skgRepos (nodeRepo public)) noClaim) link child\n"
      "*** (skg node (gnid under-link) (skgRepos (nodeRepo public))) under link\n"
      "** (skg aliasFolder) aliases\n"
      "*** (skg node (gnid under-non-vognode) (skgRepos (nodeRepo public))) under non-vognode\n"))
    (goto-char (point-min))
    (cl-letf (((symbol-function 'skg--prompt-for-repo-change)
               (lambda (current-skgrepo)
                 (should (equal current-skgrepo "public"))
                 "private")))
      (skg-set-repo-recursive))
    (dolist (gnid '("root"
                  "content-child"
                  "content-grandchild"
                  "public-under-mismatch"))
      (should (skg-sexp-subtree-p
               (test-skg--metadata-sexp-by-id gnid)
               '(skg node (skgRepos (nodeRepo private)))))
      (should-not (assq 'view (cdr (test-skg--metadata-sexp-by-id gnid)))))
    (should (skg-sexp-subtree-p
             (test-skg--metadata-sexp-by-id "mismatched-content")
             '(skg node (skgRepos (nodeRepo foreign)))))
    (should-not (skg-sexp-subtree-p
                 (test-skg--metadata-sexp-by-id "mismatched-content")
                 '(skg node (skgRepos (nodeRepo private)))))
    (dolist (gnid '("link-child"
                  "under-link"
                  "under-non-vognode"))
      (should (skg-sexp-subtree-p
               (test-skg--metadata-sexp-by-id gnid)
               '(skg node (skgRepos (nodeRepo public)))))
      (should-not (skg-sexp-subtree-p
                   (test-skg--metadata-sexp-by-id gnid)
                   '(skg node (skgRepos (nodeRepo private))))))))

(ert-deftest test-skg-replace-content-with-link-from-body ()
  "Test replacing the current branch from point in the node body."
  (with-temp-buffer
    (org-mode)
    (insert
     (concat
      "* (skg node (gnid r) (skgRepos (nodeRepo public))) R\n"
      "** (skg node (gnid p) (skgRepos (nodeRepo public))) P\n"
      "body point starts here\n"
      "*** (skg aliasFolder) aliases\n"
      "*** (skg node (gnid c) (skgRepos (nodeRepo public)) writeProtected) child\n"
      "** (skg node (gnid p) (skgRepos (nodeRepo public)) writeProtected) P elsewhere\n"))
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
         "* (skg node (gnid r) (skgRepos (nodeRepo public))) R\n"
         "** [[id:p][P]]\n"
         "** (skg node (gnid p) (skgRepos (nodeRepo public)) writeProtected) P elsewhere\n"))))))

(ert-deftest test-skg-replace-content-with-link-confirms-linked-headline ()
  "Test link-bearing headlines ask for confirmation and simplify labels."
  (with-temp-buffer
    (org-mode)
    (insert
     (concat
      "* (skg node (gnid r) (skgRepos (nodeRepo public))) R\n"
      "** (skg node (gnid p) (skgRepos (nodeRepo public))) P has [[https://x][X]] and [[id:y]]\n"))
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
         "* (skg node (gnid r) (skgRepos (nodeRepo public))) R\n"
         "** [[id:p][P has X and id:y]]\n"))))))

(ert-deftest test-skg-replace-content-with-link-cancel-linked-headline ()
  "Test declining the confirmation leaves the buffer untouched."
  (with-temp-buffer
    (org-mode)
    (let ((original
           (concat
            "* (skg node (gnid r) (skgRepos (nodeRepo public))) R\n"
            "** (skg node (gnid p) (skgRepos (nodeRepo public))) P has [[id:x][X]]\n")))
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
      "* (skg node (gnid r) (skgRepos (nodeRepo public))) R\n"
      "** (skg node (skgRepos (nodeRepo public))) P\n"))
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
      "* (skg node (gnid r) (skgRepos (nodeRepo foreign))) R\n"
      "** (skg node (gnid p) (skgRepos (nodeRepo public))) P\n"))
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
      "* (skg node (gnid r) (skgRepos (nodeRepo public)) writeProtected) R\n"
      "** (skg node (gnid p) (skgRepos (nodeRepo public))) P\n"))
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
      "* (skg node (gnid r) (skgRepos (nodeRepo public))) R\n"
      "** note\n"
      "see [[id:p][P]]\n"
      "** (skg node (gnid s) (skgRepos (nodeRepo public))) sibling\n"))
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
         "* (skg node (gnid r) (skgRepos (nodeRepo public))) R\n"
         "** (skg node (gnid p) writeProtected (viewRequests editableView)) P\n"
         "** (skg node (gnid s) (skgRepos (nodeRepo public))) sibling\n"))))))

(ert-deftest test-skg-replace-link-with-content-warns-for-existing-node ()
  "Test replacing an existing node warns because it might orphan it."
  (with-temp-buffer
    (org-mode)
    (insert
     (concat
      "* (skg node (gnid r) (skgRepos (nodeRepo public))) R\n"
      "** (skg node (gnid old) (skgRepos (nodeRepo public))) see [[id:p][P]]\n"))
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
      "* (skg node (gnid r) (skgRepos (nodeRepo public))) R\n"
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
      "* (skg node (gnid r) (skgRepos (nodeRepo public))) R\n"
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
      "* (skg node (gnid r) (skgRepos (nodeRepo public))) R\n"
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
  "Test replacement requires a editable container."
  (with-temp-buffer
    (org-mode)
    (insert
     (concat
      "* (skg node (gnid r) (skgRepos (nodeRepo public)) writeProtected) R\n"
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

;;; The diff view's restore requests

(ert-deftest test-skg-restore-keybindings ()
  "C-c s n and C-c s l are bound to the two restore commands."
  (should (eq (lookup-key skg-content-view-mode-map (kbd "C-c s n"))
              'skg-restore-deleted-node))
  (should (eq (lookup-key skg-content-view-mode-map (kbd "C-c s l"))
              'skg-restore-removed-membership)))

(defun test-skg--metadata-after (headlines command)
  "The metadata of the last of HEADLINES after COMMAND runs on it;
the command must not save, so the buffer is all there is. The
headlines before the last give it the place it stands in."
  (with-temp-buffer
    (org-mode)
    (insert headlines)
    (goto-char (point-max))
    (beginning-of-line)
    (funcall command)
    (skg--metadata-sexp-at-point-or-nil)))

(defun test-skg--refusal-of (headlines command)
  "The message with which COMMAND refuses the last of HEADLINES,
leaving the buffer as it was."
  (with-temp-buffer
    (org-mode)
    (insert headlines)
    (goto-char (point-max))
    (beginning-of-line)
    (let ((refusal (condition-case err (progn (funcall command) nil)
                     (user-error (cadr err)))))
      (should (equal (buffer-string) headlines))
      refusal)))

(ert-deftest test-skg-restore-deleted-node-marks-a-ghost ()
  "The request goes inside the ghost's form."
  (should (skg-sexp-subtree-p
           (test-skg--metadata-after
            "* (skg (vnid 0d) ghost (gnid g) (skgRepos (nodeRepo main)) (diff (node removed) (rel removed))) g"
            #'skg-restore-deleted-node)
           '(skg ghost (gnid g) (editRequest restoreNode)))))

(ert-deftest test-skg-restore-removed-membership-marks-a-removed-member ()
  "An editable removed member, under a node or a ghost,
and a write-protected folder member carrying '(rel removed)' all take
the request."
  (dolist (headline
           '("* (skg node (gnid r) (skgRepos (nodeRepo main))) r
** (skg (vnid 0c) node (gnid x) (skgRepos (nodeRepo main)) noClaim (diff (rel removed) (leaves 1 rel))) x"
             "* (skg ghost (gnid g) (skgRepos (nodeRepo main)) (diff (node removed))) g
** (skg (vnid 0c) node (gnid x) (skgRepos (nodeRepo main)) noClaim (diff (rel removed) (leaves 1 rel))) x"
             "* (skg node (gnid r) (skgRepos (nodeRepo main))) r
** (skg node (gnid c) (skgRepos (nodeRepo main))) c
*** (skg containerFolder) containers
**** (skg (vnid 0o) node (gnid d) (skgRepos (nodeRepo main)) noClaim writeProtected (diff (rel removed) (properties changed) (leaves 1 rel all))) d"))
    (should (skg-sexp-subtree-p
             (test-skg--metadata-after
              headline #'skg-restore-removed-membership)
             '(skg node (editRequest restoreRel))))))

(ert-deftest test-skg-edit-request-commands-keep-the-readiness-request ()
  "The merge and restore commands replace the edit request in
'(editRequest ...)' and keep the readiness request pending there."
  (should (equal (test-skg--metadata-after
                  "* (skg node (gnid a) (skgRepos (nodeRepo main)) (editRequest delete (ready node)) (diff (node added) (leaves 1 node))) a"
                  (lambda () (skg-set-merge-request "b")))
                 '(skg node (gnid a) (skgRepos (nodeRepo main))
                       (editRequest (merge b) (ready node))
                       (diff (node added) (leaves 1 node)))))
  (should (equal (test-skg--metadata-after
                  "* (skg ghost (gnid g) (skgRepos (nodeRepo main)) (editRequest (unready node)) (diff (node removed) (rel removed) (leaves 1 node) (ready node))) g"
                  #'skg-restore-deleted-node)
                 '(skg ghost (gnid g) (skgRepos (nodeRepo main))
                       (editRequest restoreNode (unready node))
                       (diff (node removed) (rel removed) (leaves 1 node) (ready node)))))
  (should (equal (test-skg--metadata-after
                  "* (skg node (gnid r) (skgRepos (nodeRepo main))) r
** (skg node (gnid x) (skgRepos (nodeRepo main)) noClaim (editRequest (ready rel)) (view noWarrant) (diff (rel removed) (leaves 1 rel))) x"
                  #'skg-restore-removed-membership)
                 '(skg node (gnid x) (skgRepos (nodeRepo main)) noClaim
                       (editRequest restoreRel (ready rel))
                       (view noWarrant) (diff (rel removed) (leaves 1 rel))))))

(ert-deftest test-skg-restore-commands-refuse-the-wrong-kind ()
  "Each command refuses a headline of the wrong kind at once."
  (should (equal (test-skg--refusal-of
                  "* (skg ghost (gnid g) (skgRepos (nodeRepo main)) (diff (node removed) (rel removed))) g"
                  #'skg-restore-removed-membership)
                 "This node is deleted; restore it first (skg-restore-deleted-node)"))
  (dolist (headline
           '("* (skg node (gnid x) (skgRepos (nodeRepo main)) (diff (rel added))) x"
             "* (skg node (gnid x) (skgRepos (nodeRepo main))) x"
             ;; a root stands for no relationship
             "* (skg node (gnid d) (skgRepos (nodeRepo main)) noClaim writeProtected (diff (rel removed))) d"
             ;; a member is no removed member
             "* (skg node (gnid r) (skgRepos (nodeRepo main))) r
** (skg node (gnid x) (skgRepos (nodeRepo main)) (diff (rel removed))) x"
             ;; a node of the containerward chain
             "* (skg node (gnid r) (skgRepos (nodeRepo main))) r
** (skg containerFolder) containers
*** (skg node (gnid c) (skgRepos (nodeRepo main)) writeProtected) c
**** (skg node (gnid d) (skgRepos (nodeRepo main)) noClaim writeProtected (diff (rel removed))) d"
             ;; a node of the chain under another folder's member
             "* (skg node (gnid r) (skgRepos (nodeRepo main))) r
** (skg subscriberFolder) subscribers
*** (skg node (gnid s) (skgRepos (nodeRepo main)) writeProtected) s
**** (skg node (gnid d) (skgRepos (nodeRepo main)) noClaim writeProtected (diff (rel removed))) d"))
    (should (equal (test-skg--refusal-of
                    headline #'skg-restore-removed-membership)
                   "Only a removed membership can be restored")))
  (dolist (headline
           '("* (skg node (gnid x) (skgRepos (nodeRepo main)) (diff (rel removed))) x"
             "* (skg unknown (gnid u))"
             "* (skg tombstone)"))
    (should (equal (test-skg--refusal-of headline #'skg-restore-deleted-node)
                   "Only a node deleted since HEAD can be restored; restoreNode belongs on a ghost (skg-restore-deleted-node)"))))

(defun test-skg--with-metadata-editor-on (headline body-fn)
  "Open the metadata editor on HEADLINE, the only line of an org
buffer, and call BODY-FN in the edit buffer with the source buffer.
Kills both buffers after."
  (let ((source (generate-new-buffer " *source*")))
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert headline "\n")
            (org-mode)
            (goto-char (point-min))
            (skg-view-metadata))
          (with-current-buffer
              (cl-find-if (lambda (buffer)
                            (eq (buffer-local-value
                                 'skg-sexp-edit--source-buffer buffer)
                                source))
                          (buffer-list))
            (funcall body-fn source)))
      (dolist (buffer (buffer-list))
        (when (eq (buffer-local-value 'skg-sexp-edit--source-buffer buffer)
                  source)
          (kill-buffer buffer)))
      (kill-buffer source))))

(defun test-skg--cycle-values-of-shown-field (field)
  "The cycle values of FIELD in the current edit buffer, which must
show FIELD with its value on the next line."
  (goto-char (point-min))
  (should (re-search-forward (concat "^\\*+ " field "$") nil t))
  (forward-line 1)
  (skg-sexp-edit--cycle-values-for-field
   field (string-trim (org-get-heading t t t t))))

(ert-deftest test-skg-metadata-editor-offers-each-kind-its-fields ()
  "The metadata editor, opened on each kind of headline, shows the
fields 'shared/metadata-fields.json' gives that kind, each at its
default when absent; cycles each through the values the headline may
take (a write-protected node is offered no delete, merge or flag, a
removed member restoreRel, a ghost restoreNode); shows
no field on a kind that has none; and saves an untouched view
unchanged."
  (require 'skg-sexpr-edit)
  (let ((view-requests '("none (default)" "editableView"
                         "writeProtectedView" "writeProtectedSubtreeView"
                         "fork" "flags" "folder"))
        (atom-values '("false (default)" "true")))
    (dolist (case
             `(("* (skg node (gnid x) (skgRepos (nodeRepo main))) x"
                ("writeProtected" . ,atom-values)
                ("noClaim" . ,atom-values)
                ("editRequest" . ("none (default)" "delete" "merge"
                                  "relRepo" "flag"))
                ("viewRequests" . ,view-requests))
               ("* (skg node (gnid x) (skgRepos (nodeRepo main)) writeProtected) x"
                ("editRequest" . ("none (default)" "relRepo")))
               ("* (skg node (gnid x) (skgRepos (nodeRepo main)) noClaim (diff (rel removed))) x"
                ("editRequest" . ("none (default)" "delete" "merge"
                                  "relRepo" "flag" "restoreRel")))
               ("* (skg ghost (gnid g) (skgRepos (nodeRepo main)) (diff (node removed))) g"
                ("editRequest" . ("none (default)" "restoreNode")))
               ("* (skg unknown (gnid u))"
                ("editRequest" . ("none (default)" "relRepo")))
               ("* (skg alias) an alias"
                ("editRequest" . ("none (default)" "relRepo")))
               ("* (skg tombstone)")
               ("* (skg subscribeeFolder)")
               ("* (skg (flag noSearchMatching)) no search matching")))
      (test-skg--with-metadata-editor-on
       (car case)
       (lambda (source)
         (dolist (field (cdr case))
           (should (equal (test-skg--cycle-values-of-shown-field (car field))
                          (cdr field))))
         (unless (cdr case)
           (goto-char (point-min))
           (should-not (re-search-forward
                        "^\\*+ \\(editRequest\\|viewRequests\\|writeProtected\\|noClaim\\)$"
                        nil t)))
         (skg-sexp-edit--apply)
         (should (equal (with-current-buffer source
                          (string-trim (buffer-string)))
                        (car case))))))))

(ert-deftest test-skg-metadata-editor-cycles-and-saves-values-with-arguments ()
  "Cycling to a value that takes arguments prompts for each and saves
the value as a list; an untouched default is stripped on every kind."
  (require 'skg-sexpr-edit)
  (dolist (case
           '(("* (skg alias) an alias" ("editRequest") 1
              "* (skg alias (editRequest (relRepo r))) an alias")
             ("* (skg unknown (gnid u))" ("editRequest") 1
              "* (skg unknown (gnid u) (editRequest (relRepo r)))")
             ("* (skg ghost (gnid g) (skgRepos (nodeRepo main))) g" ("editRequest") 1
              "* (skg ghost (gnid g) (skgRepos (nodeRepo main)) (editRequest restoreNode)) g")
             ("* (skg node (gnid x) (skgRepos (nodeRepo main))) x" ("viewRequests") 6
              "* (skg node (gnid x) (skgRepos (nodeRepo main)) (viewRequests (folder containers))) x")
             ("* (skg node (gnid x) (skgRepos (nodeRepo main))) x" ("editRequest") 4
              "* (skg node (gnid x) (skgRepos (nodeRepo main)) (editRequest (flag noSearchMatching true))) x")))
    (test-skg--with-metadata-editor-on
     (car case)
     (lambda (source)
       (goto-char (point-min))
       (re-search-forward (concat "^\\*+ " (car (nth 1 case)) "$"))
       (forward-line 1)
       (let ((answers '("containers" "noSearchMatching" "true")))
         (cl-letf (((symbol-function 'skg--repo-names)
                    (lambda () '("r" "main")))
                   ((symbol-function 'read-string)
                    (lambda (&rest _) "[[id:m][a target]]"))
                   ((symbol-function 'completing-read)
                    (lambda (_prompt collection &rest _)
                      (if (member "r" collection) "r"
                        (let ((answer (cl-find-if
                                       (lambda (a) (member a collection))
                                       answers)))
                          (setq answers (remove answer answers))
                          answer)))))
           (dotimes (_ (nth 2 case))
             (skg-sexp-edit-cycle-right))))
       (skg-sexp-edit--apply)
       (should (equal (with-current-buffer source
                        (string-trim (buffer-string)))
                      (nth 3 case)))))))

(ert-deftest test-skg-write-edit-request-places-it-as-the-server-does ()
  "An edit request goes first in the headline's '(editRequest ...)',
before its readiness request, and a new '(editRequest ...)' stands
where the server writes one: before the view requests and the derived
facts."
  (cl-flet ((write (line request &optional kind)
              (with-temp-buffer
                (org-mode)
                (insert line "\n")
                (goto-char (point-min))
                (skg--write-edit-request request kind)
                (car (split-string (buffer-string) "\n" t)))))
    (should (equal (write "* (skg node (gnid k) (viewRequests editableView) (view cycle) (diff (node added))) k"
                          'delete)
                   "* (skg node (gnid k) (editRequest delete) (viewRequests editableView) (view cycle) (diff (node added))) k"))
    (should (equal (write "* (skg node (gnid k) (editRequest (ready node)) (view cycle)) k"
                          '(merge j))
                   "* (skg node (gnid k) (editRequest (merge j) (ready node)) (view cycle)) k"))
    (should (equal (write "* (skg node (gnid k) (editRequest (merge j) (ready node))) k"
                          nil)
                   "* (skg node (gnid k) (editRequest (ready node))) k"))
    (should (equal (write "* k" 'delete 'node)
                   "* (skg node (editRequest delete)) k"))))
