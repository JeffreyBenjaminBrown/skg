;;; test-skg-flag.el --- Tests for flag staging -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'org)
(require 'skg-compare-sexpr)
(require 'skg-metadata)
(require 'skg-request-flag-state)
(require 'skg-keymaps-and-aliases)

(defun test-skg-flag--metadata-at-line (line)
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (skg--metadata-sexp-at-point-or-nil)))

(defun test-skg-flag--has-request-p (line value)
  (skg-sexp-subtree-p
   (test-skg-flag--metadata-at-line line)
   `(skg (node (editRequest (flag noSearchMatching ,value))))))

(ert-deftest test-skg-flag-stamps-exact-set-and-clear-requests ()
  (dolist (case '((t true) (nil false)))
    (with-temp-buffer
      (insert "* (skg (node (id root) (repo main))) root")
      (org-mode)
      (goto-char (point-min))
      (skg--stamp-search-matching-request (car case))
      (should (test-skg-flag--has-request-p 1 (cadr case))))))

(ert-deftest test-skg-flag-recursion-prunes-and-skips ()
  (with-temp-buffer
    (insert
     "* (skg (node (id root) (repo main))) root\n"
     "** (skg (node (id child) (repo main))) child\n"
     "** (skg (node (id child) (repo main))) duplicate child\n"
     "** (skg (node (id protected) (repo main) writeProtected)) protected\n"
     "** (skg (node (id foreign) (repo elsewhere))) foreign\n"
     "** (skg (node (id conflict) (repo main) (editRequest delete))) conflict\n"
     "** (skg (node (id link) (repo main) (affectsParent false))) link\n"
     "*** (skg (node (id under-link) (repo main))) under link\n"
     "** (skg aliasFolder) aliases\n"
     "*** (skg (node (id under-folder) (repo main))) under folder\n"
     "* (skg (node (id sibling) (repo main))) sibling")
    (org-mode)
    (goto-char (point-min))
    (cl-letf (((symbol-function 'skg--owned-repos)
               (lambda () '("main"))))
      (skg--stage-flag-search-matching-recursive t))
    (should (test-skg-flag--has-request-p 1 'true))
    (should (test-skg-flag--has-request-p 2 'true))
    (should-not (test-skg-flag--has-request-p 3 'true))
    (dolist (line '(4 5 6 7 8 9 10 11))
      (should-not (test-skg-flag--has-request-p line 'true)))))

(ert-deftest test-skg-flag-state-response-seeds-prompt-and-never-saves ()
  (dolist (case '(("false" "search matching" "no search matching" true)
                  ("true" "no search matching" "search matching" false)))
    (with-temp-buffer
      (insert "* (skg (node (id root) (repo main))) root")
      (org-mode)
      (goto-char (point-min))
      (let ((buffer (current-buffer))
            (marker (point-marker))
            initial
            (save-count 0))
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_secs _repeat function &rest args)
                     (apply function args)))
                  ((symbol-function 'skg--completing-read-with-cycle)
                   (lambda (&rest args)
                     (setq initial (nth 4 args))
                     (nth 2 case)))
                  ((symbol-function 'skg-request-save-buffer)
                   (lambda () (setq save-count (1+ save-count)))))
          (skg--set-flag-search-matching-from-state
           buffer marker "root" nil
           (format "((response-type flag-state) (id \"root\") (flag \"noSearchMatching\") (value \"%s\") (repo \"main\") (owned \"true\"))"
                   (car case))))
        (should (equal initial (cadr case)))
        (should (test-skg-flag--has-request-p 1 (nth 3 case)))
        (should (= save-count 0))))))

(ert-deftest test-skg-flag-state-response-refuses-a-vanished-buffer ()
  (let ((buffer (generate-new-buffer " *skg-flag-stale*")) marker)
    (with-current-buffer buffer
      (insert "* (skg (node (id root) (repo main))) root")
      (org-mode)
      (goto-char (point-min))
      (setq marker (point-marker)))
    (kill-buffer buffer)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_secs _repeat function &rest args)
                 (apply function args))))
      (should-error
       (skg--set-flag-search-matching-from-state
        buffer marker "root" nil
        "((id \"root\") (value \"false\") (owned \"true\"))")
       :type 'user-error))))

(ert-deftest test-skg-flag-root-refusals-and-key-bindings ()
  (dolist (metadata
           '((skg (node (repo main)))
             (skg (node (id root) (repo main) writeProtected))
             (skg (node (id root) (repo main) (editRequest delete)))))
    (should-error (skg--flag-eligible-root-id metadata)
                  :type 'user-error))
  (should (eq (lookup-key skg-content-view-mode-map (kbd "C-c l p"))
              #'skg-show-folderOf-flags))
  (should-not (lookup-key skg-content-view-mode-map (kbd "C-c l b")))
  (should (eq (lookup-key skg-content-view-mode-map (kbd "C-c s x"))
              #'skg-set-flag-search-matching)))

(provide 'test-skg-flag)
