;;; test-skg-folds-across-redraw.el --- A redraw the user did not ask for keeps their folds -*- lexical-binding: t; -*-

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'skg-buffer)
(require 'skg-request-save)

(defconst test-skg-folds-across-redraw--last-save
  (concat "* (skg (vnid a0) node (gnid ga0)) A\nbody of A\n"
          "** (skg (vnid a1) node (gnid ga1)) A1\nbody of A1\n"
          "* (skg (vnid b0) node (gnid gb0)) B\nbody of B\n"
          "** (skg (vnid b1) folded node (gnid gb1)) B1\nbody of B1\n"
          "* (skg (vnid c0) node (gnid gc0)) C\nbody of C\nmore of C\n")
  "A view as its last save left it: B folded, so B1 hidden.")

(defun test-skg-folds-across-redraw--goto-headline (title)
  "Move point to the headline whose title is TITLE."
  (goto-char (point-min))
  (re-search-forward (concat ") " (regexp-quote title) "$"))
  (beginning-of-line))

(defun test-skg-folds-across-redraw--invisible-p (title)
  "Whether the headline whose title is TITLE is hidden."
  (save-excursion
    (test-skg-folds-across-redraw--goto-headline title)
    (invisible-p (point))))

(defun test-skg-folds-across-redraw--body-hidden-p (title)
  "Whether the first body line of the headline whose title is TITLE is hidden."
  (save-excursion
    (test-skg-folds-across-redraw--goto-headline title)
    (forward-line 1)
    (invisible-p (point))))

(ert-deftest test-a-redraw-not-asked-for-keeps-the-folds-changed-since-the-last-save ()
  "After the last save folded B, the user unfolded B, folded A and hid
C's body. A rerender carrying the last save's folds, and a new headline
D whose body the server hides, keeps the user's folds on A, B and C and
draws D as the server says."
  (save-window-excursion
    (with-temp-buffer
      (skg-content-view-mode)
      (setq-local skg-view-id "view-folds")
      (set-window-buffer (selected-window) (current-buffer))
      (skg-replace-buffer-with-new-content
       nil test-skg-folds-across-redraw--last-save)
      (should (test-skg-folds-across-redraw--invisible-p "B1"))
      (test-skg-folds-across-redraw--goto-headline "B")
      (org-fold-show-subtree)
      (test-skg-folds-across-redraw--goto-headline "A")
      (org-fold-hide-subtree)
      (test-skg-folds-across-redraw--goto-headline "C")
      (org-fold-hide-entry)
      (skg--apply-streamed-view-update
       (prin1-to-string
        `((view-id "view-folds")
          (content
           ,(concat test-skg-folds-across-redraw--last-save
                    "* (skg (vnid d0) bodyFolded node (gnid gd0)) D\n"
                    "body of D\nmore of D\n"))))
       'rerender "rerender-view")
      (should (test-skg-folds-across-redraw--invisible-p "A1"))
      (should-not (test-skg-folds-across-redraw--invisible-p "B1"))
      (should-not (test-skg-folds-across-redraw--body-hidden-p "B1"))
      (should (test-skg-folds-across-redraw--body-hidden-p "C"))
      (should (test-skg-folds-across-redraw--body-hidden-p "D"))
      (should-not (test-skg-folds-across-redraw--invisible-p "D"))
      (goto-char (point-min))
      (should-not (re-search-forward "\\_<\\(folded\\|bodyFolded\\)\\_>"
                                     nil t)))))

(ert-deftest test-a-redraw-not-asked-for-keeps-a-collapse-whose-children-left ()
  "The user collapsed B and left C open. A rerender in which B's only
child moved under C, and a new child of A arrives marked folded, keeps
B collapsed (its body hidden), and C, its moved child and A's new
child shown, A being open."
  (save-window-excursion
    (with-temp-buffer
      (skg-content-view-mode)
      (setq-local skg-view-id "view-folds")
      (set-window-buffer (selected-window) (current-buffer))
      (skg-replace-buffer-with-new-content
       nil test-skg-folds-across-redraw--last-save)
      (test-skg-folds-across-redraw--goto-headline "B")
      (should (invisible-p (save-excursion (forward-line 2) (point))))
      (skg--apply-streamed-view-update
       (prin1-to-string
        `((view-id "view-folds")
          (content
           ,(concat "* (skg (vnid a0) node (gnid ga0)) A\nbody of A\n"
                    "** (skg (vnid a1) node (gnid ga1)) A1\nbody of A1\n"
                    "** (skg (vnid n2) folded node (gnid gn2)) N2\n"
                    "* (skg (vnid b0) node (gnid gb0)) B\nbody of B\n"
                    "* (skg (vnid c0) node (gnid gc0)) C\nbody of C\nmore of C\n"
                    "** (skg (vnid b1) folded node (gnid gb1)) B1\nbody of B1\n"))))
       'rerender "rerender-view")
      (should (test-skg-folds-across-redraw--body-hidden-p "B"))
      (should-not (test-skg-folds-across-redraw--body-hidden-p "C"))
      (should-not (test-skg-folds-across-redraw--invisible-p "B1"))
      (should-not (test-skg-folds-across-redraw--invisible-p "N2")))))

(ert-deftest test-content-with-fold-atoms-decides-folded-by-the-parent-and-bodyFolded-by-the-headline ()
  "Each headline's 'folded' follows its parent's recorded state and its
'bodyFolded' its own, placed after the viewnode ID; the focus atom and
the title are kept, and what only a new headline decides is the
server's."
  (let ((fold-states (make-hash-table :test 'equal)))
    (puthash "k1" 'bodyHidden fold-states)
    (puthash "k2" 'open fold-states)
    (puthash "k5" 'collapsed fold-states)
    (puthash "k6" 'collapsed fold-states)
    (should
     (equal
      (skg--content-with-fold-atoms
       (concat "* (skg (vnid k1) focused folded node (gnid g1)) folded title\n"
               "body\n"
               "** (skg (vnid k2) folded bodyFolded node (gnid g2)) two\n"
               "** (skg (vnid n3) node (gnid g3)) three\n"
               "*** (skg (vnid n4) folded node (gnid g4)) four\n"
               "* (skg (vnid k5) node (gnid g5)) five\nbody\n"
               "* (skg (vnid k6) bodyFolded node (gnid g6)) six\n"
               "** (skg (vnid n7) node (gnid g7)) seven\n")
       fold-states)
      (concat "* (skg (vnid k1) bodyFolded focused folded node (gnid g1)) folded title\n"
              "body\n"
              "** (skg (vnid k2) node (gnid g2)) two\n"
              "** (skg (vnid n3) node (gnid g3)) three\n"
              "*** (skg (vnid n4) folded node (gnid g4)) four\n"
              "* (skg (vnid k5) bodyFolded node (gnid g5)) five\nbody\n"
              "* (skg (vnid k6) node (gnid g6)) six\n"
              "** (skg (vnid n7) folded node (gnid g7)) seven\n")))))

(provide 'test-skg-folds-across-redraw)
