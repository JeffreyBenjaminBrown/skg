;;; test-skg-git-add.el --- Tests for recursive skg git-add commands.

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
              "port = 9999\n"
              "beep_when_server_becomes_available = false\n\n"
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

(defun test-skg-git-add--make-diff-view ()
  "Create a skg content-view buffer with mixed git-diff metadata."
  (let ((buf (get-buffer-create "*test-skg-git-add-view*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert
       "* (skg (node (id new) (source main) (unstaged newX newM))) new\n"
       "body\n"
       "** (skg (node (id child) (source main) (unstaged newX newM))) child\n"
       "** (skg (node (id old) (source main) (unstaged newM))) old\n"
       "* (skg (node (id staged) (source main) (staged newX))) staged\n")
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

(defun test-skg-git-add--staged-blob-hash (source-dir file)
  "Return the git hash of FILE's current staged blob in SOURCE-DIR."
  (with-temp-buffer
    (let ((default-directory source-dir))
      (call-process "git" nil t nil "ls-files" "-s" file))
    (string-trim (buffer-string))))

(ert-deftest test-skg-git-add-if-new-recursive-runs-previewed-command ()
  "The recursive executor stages the same newX subtree files as the preview."
  (unwind-protect
      (let* ((root (test-skg-git-add--setup))
             (src  (expand-file-name "main" root))
             (buf  (test-skg-git-add--make-diff-view)))
        (should-not (test-skg-git-add--in-index-p src "new.skg"))
        (should-not (test-skg-git-add--in-index-p src "child.skg"))
        (with-current-buffer buf
          (goto-char (point-min))
          (skg-git-add-if-new-recursive))
        (should (test-skg-git-add--in-index-p src "new.skg"))
        (should (test-skg-git-add--in-index-p src "child.skg"))
        (let ((staged-hash
               (test-skg-git-add--staged-blob-hash src "staged.skg")))
          (with-temp-file (expand-file-name "staged.skg" src)
            (insert "title: staged-MODIFIED\npid: staged\n"))
          (with-current-buffer buf
            (goto-char (point-min))
            (re-search-forward "^\\* .* staged$")
            (skg-git-add-if-new-recursive))
          (should
           (string=
            staged-hash
            (test-skg-git-add--staged-blob-hash src "staged.skg")))))
    (test-skg-git-add--teardown)))

(ert-deftest test-skg-git-add-preview-collects-only-unstaged-new-files ()
  "The preview collector uses newX, not membership-only newM or staged newX."
  (unwind-protect
      (progn
        (test-skg-git-add--setup)
        (let ((buf (test-skg-git-add--make-diff-view)))
          (with-current-buffer buf
            (goto-char (point-min))
            (should
             (equal
              (skg--subtree-unstaged-new-file-id-and-source-pairs)
              '(("new" . "main")
                ("child" . "main")))))))
    (test-skg-git-add--teardown)))

(ert-deftest test-skg-git-add-preview-works-from-body ()
  "Calling from body text still uses that body's owning subtree."
  (unwind-protect
      (progn
        (test-skg-git-add--setup)
        (let ((buf (test-skg-git-add--make-diff-view)))
          (with-current-buffer buf
            (goto-char (point-min))
            (search-forward "body")
            (should
             (equal
              (skg--subtree-unstaged-new-file-id-and-source-pairs)
              '(("new" . "main")
                ("child" . "main")))))))
    (test-skg-git-add--teardown)))

(ert-deftest test-skg-git-add-preview-command-does-not_touch_git ()
  "The preview command creates a buffer but does not stage anything."
  (unwind-protect
      (let* ((root (test-skg-git-add--setup))
             (src  (expand-file-name "main" root))
             (buf  (test-skg-git-add--make-diff-view)))
        (should-not (test-skg-git-add--in-index-p src "new.skg"))
        (should-not (test-skg-git-add--in-index-p src "child.skg"))
        (cl-letf (((symbol-function 'skg-readable-ids-mode)
                   (lambda (&optional _arg) nil)))
          (with-current-buffer buf
            (goto-char (point-min))
            (skg-git-add-if-new-recursive-preview)))
        (should-not (test-skg-git-add--in-index-p src "new.skg"))
        (should-not (test-skg-git-add--in-index-p src "child.skg"))
        (with-current-buffer "*skg git add new files*"
          (let ((text (buffer-string)))
            (should (string-match-p "ls-files" text))
            (should (string-match-p "git\" nil \"\\*skg-git-add-output\\*\" t" text))
            (should
             ;; `pp-to-string' line-wrapping is Emacs-version-sensitive. This regex is robust to those variations.
             (string-match-p "\"add\"[[:space:]\n]+\"--\"[[:space:]\n]+file" text))
            (should (string-match-p "\"new.skg\"" text))
            (should (string-match-p "\"child.skg\"" text))
            (should-not (string-match-p "\"old.skg\"" text))
            (should-not (string-match-p "\"staged.skg\"" text)))))
    (when (get-buffer "*skg git add new files*")
      (let ((kill-buffer-query-functions nil))
        (kill-buffer "*skg git add new files*")))
    (test-skg-git-add--teardown)))

(ert-deftest test-skg-git-add-preview-ignores-print-length ()
  "The preview must print a complete executable form even when
`print-length' is non-nil. Otherwise Emacs prints `...' into the
quoted filename list, and evaluating the preview later passes that
ellipsis object to `call-process' instead of a string."
  (unwind-protect
      (let* ((root (test-skg-git-add--setup))
             (src  (expand-file-name "main" root))
             (buf  (get-buffer-create "*test-skg-git-add-view*"))
             (ids  nil))
        (dotimes (i 11)
          (let ((id (format "new-%02d" i)))
            (push id ids)
            (with-temp-file (expand-file-name (concat id ".skg") src)
              (insert "title: " id "\nids:\n  - " id "\n"))))
        (setq ids (nreverse ids))
        (with-current-buffer buf
          (erase-buffer)
          (insert "* (skg (node (id old) (source main) (unstaged newM))) old\n")
          (dolist (id ids)
            (insert (format "** (skg (node (id %s) (source main) (unstaged newX newM))) %s\n"
                            id id)))
          (skg-content-view-mode)
          (goto-char (point-min)))
        (cl-letf (((symbol-function 'skg-readable-ids-mode)
                   (lambda (&optional _arg) nil)))
          (let ((print-length 10))
            (with-current-buffer buf
              (skg-git-add-if-new-recursive-preview))))
        (with-current-buffer "*skg git add new files*"
          (let ((text (buffer-string)))
            (should-not (string-match-p "\\.\\.\\." text))
            (should (string-match-p "\"new-10.skg\"" text))
            (eval (read text) t)))
        (dolist (id ids)
          (should (test-skg-git-add--in-index-p
                   src (concat id ".skg")))))
    (when (get-buffer "*skg git add new files*")
      (let ((kill-buffer-query-functions nil))
        (kill-buffer "*skg git add new files*")))
    (test-skg-git-add--teardown)))

(provide 'test-skg-git-add)
