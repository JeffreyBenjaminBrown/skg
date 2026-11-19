;;; test-skg-org-fold.el --- Tests for skg org folding

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'skg-org-fold)
(require 'skg-request-save)

(ert-deftest test-skg-fold-round-trip ()
  "A whole redraw folds by the atoms and strips them; a save writes the
same atoms back."
  (let* ((imaginary-from-rust "* 1
1 body
** 11
*** (skg folded node (gnid 1)) 111
*** (skg folded node (gnid 2) writeProtected) 112
** 12
12 body
*** (skg folded node (gnid 3)) 121
121 body
*** 122
** 13")
         (buf (generate-new-buffer "*test-fold-round-trip*")))
    (with-current-buffer buf
      (org-mode)
      (skg-replace-buffer-with-new-content nil imaginary-from-rust)
      (should ;; There should be 5 visible lines.
       (= (count-visible-nonempty-lines)
          5))
      (goto-char (point-min))
      (let*
          ((expected
            ;; Every sibling of a folded node should be folded.
            ;; 'folded' is a top-level bare atom, written where the
            ;; server writes it: before the kind atom. The headline
            ;; holding point is 'focused'.
            "* (skg focused) 1
1 body
** 11
*** (skg folded node (gnid 1)) 111
*** (skg folded node (gnid 2) writeProtected) 112
** 12
12 body
*** (skg folded node (gnid 3)) 121
121 body
*** (skg folded) 122
** 13")
           (actual (skg--buffer-snapshot-with-save-metadata)))
        (should (equal actual
                       expected))))
    (kill-buffer buf)))

(ert-deftest test-save-snapshot-leaves-the-buffer-untouched ()
  "The text a save sends carries 'folded' on each hidden headline,
'bodyFolded' on one whose entry alone is hidden, and 'focused' on the
headline holding point; the buffer keeps its text, its folds and point,
and no change to it runs a modification hook."
  (with-temp-buffer
    (org-mode)
    (insert "* (skg node (gnid 1)) 1\n1 body\n** 11\n*** 111\n** 12\n12 body\n*** 121\n")
    (goto-char (point-min))
    (search-forward "** 11")
    (org-fold-hide-subtree)
    (search-forward "** 12")
    (org-fold-hide-entry)
    (let ((text (buffer-substring-no-properties (point-min) (point-max)))
          (start (point))
          (changes 0))
      (add-hook 'after-change-functions
                (lambda (&rest _) (setq changes (1+ changes))) nil t)
      (should
       (equal (skg--buffer-snapshot-with-save-metadata)
              "* (skg node (gnid 1)) 1\n1 body\n** 11\n*** (skg folded) 111\n** (skg focused bodyFolded) 12\n12 body\n*** 121\n"))
      (should (= changes 0))
      (should (= (point) start))
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     text))
      (goto-char (point-min))
      (search-forward "*** 111")
      (should (invisible-p (line-beginning-position)))
      (search-forward "12 body")
      (should (invisible-p (line-beginning-position)))
      (search-forward "*** 121")
      (should-not (invisible-p (line-beginning-position))))))

(ert-deftest test-whole-redraw-leaves-a-top-level-folded-headline-visible ()
  "A top-level headline marked `folded' has no parent to fold, so it
stays visible; the other `folded' headlines still fold their parents."
  (with-temp-buffer
    (org-mode)
    (skg-replace-buffer-with-new-content nil "* (skg folded) 1
** (skg folded) 11
* 2
** (skg folded) 21")
    (should (= (count-visible-nonempty-lines)
               2))
    (goto-char (point-min))
    (should-not (invisible-p (point)))
    (search-forward "* 2")
    (should-not (invisible-p (line-beginning-position)))))

(ert-deftest test-whole-redraw-strips-fold-atoms ()
  "A whole redraw removes each 'folded' and 'bodyFolded' atom, with the
space before it, and nothing else: a headline with no title keeps no
space after its metadata."
  (with-temp-buffer
    (org-mode)
    (skg-replace-buffer-with-new-content nil "* (skg folded) only folded
* (skg folded other) folded first
Body text.
* (skg folded (key value)) folded last
* (skg folded (k v) other) folded middle
** (skg folded forksFolder)
** (skg bodyFolded node) body folded
* (skg other) no folded")
    (should (equal (buffer-substring-no-properties (point-min)
                                                   (point-max))
                   "* (skg) only folded
* (skg other) folded first
Body text.
* (skg (key value)) folded last
* (skg (k v) other) folded middle
** (skg forksFolder)
** (skg node) body folded
* (skg other) no folded"))))

(ert-deftest test-whole-redraw-edits-the-buffer-once ()
  "A whole redraw of a mostly folded view runs the modification hooks
once to erase the old text and once to insert the new, however many
atoms it strips."
  (with-temp-buffer
    (org-mode)
    (insert "* old\nold body\n")
    (let ((changes 0))
      (add-hook 'after-change-functions
                (lambda (&rest _) (setq changes (1+ changes))) nil t)
      (skg-replace-buffer-with-new-content
       nil (concat "* (skg focused) root\n"
                   (mapconcat
                    (lambda (n)
                      (format "** (skg folded) %d\n*** (skg folded) %d.1\n"
                              n n))
                    (number-sequence 1 20) "")))
      (should (= changes 2))
      (should (= (count-visible-nonempty-lines) 1)))))

(defun count-visible-nonempty-lines ()
  "Count the number of visible non-empty lines in the current buffer."
  (let
      ((visible-count 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (beginning-of-line)
        (unless
            (invisible-p (point))
          (let
              ((line-text
                (buffer-substring
                 (line-beginning-position)
                 (line-end-position))))
            (when
                (not (string-empty-p line-text))
              (setq visible-count (1+ visible-count)))))
        (forward-line 1)))
    visible-count))

(provide 'test-skg-org-fold)

(ert-deftest test-skg-org-body-toggle ()
  "Toggling hides then shows a body; from inside the body point
moves to the headline start; a bodyless headline is left alone."
  (with-temp-buffer
    (org-mode)
    (insert "* first\nbody line one\nbody line two\n* second\n")
    (progn ;; from the headline: hide, then show
      (goto-char (point-min))
      (skg-org-body-toggle)
      (should (invisible-p (save-excursion (forward-line 1) (point))))
      (skg-org-body-toggle)
      (should-not (invisible-p (save-excursion (forward-line 1)
                                               (point)))))
    (progn ;; from inside the body: point moves to the headline start
      (goto-char (point-min))
      (search-forward "body line two")
      (skg-org-body-toggle)
      (should (= (point) (point-min)))
      (should (org-at-heading-p))
      (should (invisible-p (save-excursion (forward-line 1) (point)))))
    (progn ;; a bodyless headline: no error, nothing hidden
      (goto-char (point-min))
      (search-forward "* second")
      (beginning-of-line)
      (skg-org-body-toggle)
      (should (org-at-heading-p)))))

(ert-deftest test-skg-org-body-toggle-opens-a-folded-headline-one-level ()
  "A fully folded headline opens its body and immediate children only."
  (with-temp-buffer
    (org-mode)
    (insert "* parent\nparent body\n** first child\nfirst child body\n*** grandchild\ngrandchild body\n** second child\nsecond child body\n* sibling\n")
    (goto-char (point-min))
    (org-fold-hide-subtree)

    (skg-org-body-toggle)
    (should-not (invisible-p
                 (save-excursion (forward-line 1) (point))))
    (should-not (invisible-p
                 (save-excursion
                   (search-forward "** first child")
                   (line-beginning-position))))
    (should (invisible-p
             (save-excursion
               (search-forward "first child body")
               (line-beginning-position))))
    (should (invisible-p
             (save-excursion
               (search-forward "*** grandchild")
               (line-beginning-position))))
    (should-not (invisible-p
                 (save-excursion
                   (search-forward "** second child")
                   (line-beginning-position))))

    (skg-org-body-toggle)
    (should (invisible-p
             (save-excursion (forward-line 1) (point))))
    (should-not (invisible-p
                 (save-excursion
                   (search-forward "** first child")
                   (line-beginning-position))))

    (skg-org-body-toggle)
    (should-not (invisible-p
                 (save-excursion (forward-line 1) (point))))
    (should-not (invisible-p
                 (save-excursion
                   (search-forward "** first child")
                   (line-beginning-position))))))

(ert-deftest test-skg-org-body-toggle-opens-a-folded-bodyless-headline ()
  "A folded bodyless headline still reveals its immediate children."
  (with-temp-buffer
    (org-mode)
    (insert "* parent\n** child\nchild body\n*** grandchild\n* sibling\n")
    (goto-char (point-min))
    (org-fold-hide-subtree)

    (skg-org-body-toggle)
    (should-not (invisible-p
                 (save-excursion
                   (search-forward "** child")
                   (line-beginning-position))))
    (should (invisible-p
             (save-excursion
               (search-forward "child body")
               (line-beginning-position))))
    (should (invisible-p
             (save-excursion
               (search-forward "*** grandchild")
               (line-beginning-position))))))
