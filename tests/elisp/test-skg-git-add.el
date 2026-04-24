;;; test-skg-git-add.el --- Tests for skg-git-add-if-new.

;;; Sets up a throwaway git repo and skgconfig.toml in /tmp, runs the
;;; two user commands, then asserts against the repo's state.

(defconst test-skg-git-add--this-dir
  (file-name-directory load-file-name)
  "Captured at load time; `load-file-name' is nil inside ERT bodies.")

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             test-skg-git-add--this-dir))
(require 'ert)
(load-file (expand-file-name "../../elisp/skg-init.el"
                             test-skg-git-add--this-dir))

(defvar test-skg-git-add--data-root nil
  "Populated per-test, set back to nil after.")

(defun test-skg-git-add--sh (dir &rest args)
  "Run git with ARGS in DIR. Asserts success."
  (let ((default-directory dir))
    (unless (zerop (apply #'call-process "git" nil nil nil args))
      (error "git %s failed in %s" args dir))))

(defun test-skg-git-add--setup ()
  "Create a fresh data-root with a source dir + skgconfig.toml.

Initial state:
- old.skg      : committed (present in HEAD).
- new.skg      : on disk, untracked.
- child.skg    : on disk, untracked.
- staged.skg   : on disk, staged but never committed (`(staged newX)` shape).

Returns the absolute data-root path."
  (let* ((root (make-temp-file "skg-git-add-" t))
         (src  (expand-file-name "main" root)))
    (make-directory src)
    (with-temp-file (expand-file-name "old.skg"    src) (insert "title: old\npid: old\n"))
    (with-temp-file (expand-file-name "new.skg"    src) (insert "title: new\npid: new\n"))
    (with-temp-file (expand-file-name "child.skg"  src) (insert "title: child\npid: child\n"))
    (with-temp-file (expand-file-name "staged.skg" src) (insert "title: staged\npid: staged\n"))
    (test-skg-git-add--sh src "init" "-q")
    (test-skg-git-add--sh src "config" "user.email" "t@t")
    (test-skg-git-add--sh src "config" "user.name"  "Test")
    (test-skg-git-add--sh src "add" "old.skg")
    (test-skg-git-add--sh src "commit" "-q" "-m" "initial")
    (test-skg-git-add--sh src "add" "staged.skg") ;; staged-new: in index, not in HEAD
    (with-temp-file (expand-file-name "skgconfig.toml" root)
      (insert "db_name = \"t\"\n"
              "tantivy_folder = \".idx\"\n"
              "port = 9999\n\n"
              "[[sources]]\n"
              "name = \"main\"\n"
              "path = \"main\"\n"
              "user_owns_it = true\n"))
    (setq test-skg-git-add--data-root root)
    (setq skg-config-dir (file-name-as-directory root))
    root))

(defun test-skg-git-add--in-index-p (source-dir file)
  "Return t if FILE is in the git index of SOURCE-DIR."
  (let ((default-directory source-dir))
    (zerop (call-process "git" nil nil nil
                         "ls-files" "--error-unmatch" file))))

(defun test-skg-git-add--in-head-p (source-dir file)
  "Return t if FILE is present in HEAD of SOURCE-DIR."
  (let ((default-directory source-dir))
    (zerop (call-process "git" nil nil nil
                         "cat-file" "-e" (concat "HEAD:" file)))))

(defun test-skg-git-add--head-count (source-dir)
  "Return the number of commits reachable from HEAD in SOURCE-DIR."
  (with-temp-buffer
    (let ((default-directory source-dir))
      (call-process "git" nil t nil "rev-list" "--count" "HEAD"))
    (string-to-number (string-trim (buffer-string)))))

(defun test-skg-git-add--make-view (ids-and-children)
  "Create a skg content-view buffer shaped like IDS-AND-CHILDREN.
IDS-AND-CHILDREN is a list of (id . children-ids)."
  (let ((buf (get-buffer-create "*test-skg-git-add-view*")))
    (with-current-buffer buf
      (erase-buffer)
      (dolist (pair ids-and-children)
        (let ((id       (car pair))
              (children (cdr pair)))
          (insert (format "* (skg (node (id %s) (source main))) %s\n" id id))
          (dolist (cid children)
            (insert (format "** (skg (node (id %s) (source main))) %s\n" cid cid)))))
      (skg-content-view-mode)
      (goto-char (point-min)))
    buf))

(defun test-skg-git-add--teardown ()
  (when (and test-skg-git-add--data-root
             (file-directory-p test-skg-git-add--data-root))
    (delete-directory test-skg-git-add--data-root t))
  (setq test-skg-git-add--data-root nil)
  (setq skg-config-dir nil)
  (when (get-buffer "*test-skg-git-add-view*")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*test-skg-git-add-view*"))))

(ert-deftest test-skg-git-add-if-new-adds-untracked ()
  "On an untracked node, the command stages just that file."
  (unwind-protect
      (let* ((root (test-skg-git-add--setup))
             (src  (expand-file-name "main" root))
             (buf  (test-skg-git-add--make-view '(("new")))))
        (should-not (test-skg-git-add--in-index-p src "new.skg"))
        (with-current-buffer buf
          (goto-char (point-min))
          (skg-git-add-if-new))
        (should (test-skg-git-add--in-index-p src "new.skg"))
        (should-not (test-skg-git-add--in-head-p src "new.skg")))
    (test-skg-git-add--teardown)))

(ert-deftest test-skg-git-add-if-new-skips-in-head ()
  "A file already in HEAD is left alone; no change to the repo."
  (unwind-protect
      (let* ((root (test-skg-git-add--setup))
             (src  (expand-file-name "main" root))
             (buf  (test-skg-git-add--make-view '(("old"))))
             (commits-before (test-skg-git-add--head-count src)))
        (with-current-buffer buf
          (goto-char (point-min))
          (skg-git-add-if-new))
        (should (= commits-before (test-skg-git-add--head-count src))))
    (test-skg-git-add--teardown)))

(defun test-skg-git-add--staged-blob-hash (source-dir file)
  "Return the git hash of FILE's current staged blob in SOURCE-DIR."
  (with-temp-buffer
    (let ((default-directory source-dir))
      (call-process "git" nil t nil "ls-files" "-s" file))
    (string-trim (buffer-string))))

(ert-deftest test-skg-git-add-if-new-skips-already-staged ()
  "A file already in the index must not be re-staged, even when
the worktree copy has been modified since the original `git add'.
This is the contract that keeps `C-c t n' from silently promoting
the user's unrelated edits."
  (unwind-protect
      (let* ((root (test-skg-git-add--setup))
             (src  (expand-file-name "main" root))
             (buf  (test-skg-git-add--make-view '(("staged"))))
             (staged-hash-before
              (test-skg-git-add--staged-blob-hash src "staged.skg")))
        ;; Modify the worktree copy after staging: the staged blob and
        ;; the worktree now differ. A naive `git add' would silently
        ;; stage these modifications; the command must not.
        (with-temp-file (expand-file-name "staged.skg" src)
          (insert "title: staged-MODIFIED\npid: staged\n"))
        (with-current-buffer buf
          (goto-char (point-min))
          (skg-git-add-if-new))
        (let ((staged-hash-after
               (test-skg-git-add--staged-blob-hash src "staged.skg")))
          (should (string= staged-hash-before staged-hash-after))))
    (test-skg-git-add--teardown)))

(ert-deftest test-skg-git-add-if-new-recursive-covers-descendants ()
  "The recursive form adds the node at point AND each descendant."
  (unwind-protect
      (let* ((root (test-skg-git-add--setup))
             (src  (expand-file-name "main" root))
             (buf  (test-skg-git-add--make-view '(("new" "child")))))
        (should-not (test-skg-git-add--in-index-p src "new.skg"))
        (should-not (test-skg-git-add--in-index-p src "child.skg"))
        (with-current-buffer buf
          (goto-char (point-min))
          (skg-git-add-if-new-recursive))
        (should (test-skg-git-add--in-index-p src "new.skg"))
        (should (test-skg-git-add--in-index-p src "child.skg")))
    (test-skg-git-add--teardown)))

(provide 'test-skg-git-add)
