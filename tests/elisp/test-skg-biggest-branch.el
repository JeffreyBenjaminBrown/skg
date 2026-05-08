;;; test-skg-goto-biggest-branch.el --- Tests for skg-goto-biggest-branch

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'org)
(require 'skg-modify-graph)

(ert-deftest test-skg-goto-biggest-branch-chooses-largest-sibling-branch ()
  "The current node participates in sibling comparisons."
  (let ((buf (generate-new-buffer "*test-skg-biggest-sibling*")))
    (with-current-buffer buf
      (org-mode)
      (insert "* root
** small
*** small child
** big
*** big child 1
**** big grandchild
*** big child 2
** medium
*** medium child
")
      (goto-char (point-min))
      (search-forward "small")
      (skg-goto-biggest-branch)
      (should (equal (org-get-heading t t t t)
                     "big")))
    (kill-buffer buf)))

(ert-deftest test-skg-goto-biggest-branch-falls-back-to-children-without-siblings ()
  "A node with no siblings chooses among its immediate children."
  (let ((buf (generate-new-buffer "*test-skg-biggest-child*")))
    (with-current-buffer buf
      (org-mode)
      (insert "* only root
** leaf
** heavy
*** heavy child 1
**** heavy grandchild
*** heavy child 2
")
      (goto-char (point-min))
      (skg-goto-biggest-branch)
      (should (equal (org-get-heading t t t t)
                     "heavy")))
    (kill-buffer buf)))

(provide 'test-skg-goto-biggest-branch)
