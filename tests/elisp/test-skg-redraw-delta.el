;;; test-skg-redraw-delta.el --- Redrawing a saved buffer by its delta -*- lexical-binding: t; -*-

;; The stripped form against the cases the server and both clients
;; share, and the patch `skg-handle-save-sexp' tries when a save-result
;; names the headlines it changed ('elisp/skg-redraw-delta.el').

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'skg-request-save)
(require 'skg-redraw-delta)

(defun test-skg-redraw-delta--strip-cases ()
  "The cases of 'tests/shared/fold-and-focus-atom-strip-cases.txt', each
a list (LINE STRIPPED-LINE)."
  (let ((cases nil)
        (case-lines nil))
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "tests/shared/fold-and-focus-atom-strip-cases.txt"
                         skg-test--project-root))
      (dolist (line (append (split-string (buffer-string) "\n") '("")))
        (cond ((string-prefix-p "#" line))
              ((string-empty-p line)
               (when case-lines
                 (push (nreverse case-lines) cases)
                 (setq case-lines nil)))
              (t (push line case-lines)))))
    (nreverse cases)))

(ert-deftest test-skg-redraw-delta-strips-the-shared-cases ()
  "Each line of the shared cases file strips to the line after it."
  (let ((cases (test-skg-redraw-delta--strip-cases)))
    (should (> (length cases) 20))
    (dolist (case cases)
      (should (= (length case) 2))
      (should (equal (list (car case) (skg--stripped-line (car case)))
                     case)))))

(ert-deftest test-skg-redraw-delta-strips-a-text-line-by-line ()
  "The stripped form of a text strips headline lines only."
  (should (equal (skg--stripped-text
                  (concat "preamble folded\n"
                          "* (skg (vnid a) focused node (gnid a)) a\n"
                          "body (skg folded)\n"
                          "** (skg (vnid b) folded node (gnid b)) b\n"))
                 (concat "preamble folded\n"
                         "* (skg (vnid a) node (gnid a)) a\n"
                         "body (skg folded)\n"
                         "** (skg (vnid b) node (gnid b)) b\n"))))

(defconst test-skg-redraw-delta--drawn
  (concat "* (skg (vnid a) node (gnid a)) a\n"
          "a body\n"
          "** (skg (vnid b) node (gnid b)) bee title\n"
          "b body\n"
          "*** (skg (vnid c) node (gnid c)) c\n"
          "** (skg (vnid d) node (gnid d)) d\n")
  "A view as the buffer shows it, without fold and focus atoms.")

(defconst test-skg-redraw-delta--b-marked
  (concat "* (skg (vnid a) node (gnid a)) a\n"
          "a body\n"
          "** (skg (vnid b) folded focused node (gnid b) (diff (ready all))) bee title\n"
          "b body\n"
          "*** (skg (vnid c) node (gnid c)) c\n"
          "** (skg (vnid d) node (gnid d)) d\n")
  "The server's new text for `test-skg-redraw-delta--drawn': b's line
changed, b's subtree folded and focused.")

(defun test-skg-redraw-delta--save-result (content changed-headlines)
  "A save-result carrying CONTENT and, unless CHANGED-HEADLINES is
`none', the delta CHANGED-HEADLINES."
  (prin1-to-string
   `((content ,content) (errors ()) (warnings ())
     ,@(unless (eq changed-headlines 'none)
         `((changed-headlines ,changed-headlines))))))

(defmacro test-skg-redraw-delta--in-view (text &rest body)
  "Run BODY in an Org buffer showing TEXT, counting in the variable
`whole-redraws' the calls to `skg-replace-buffer-with-new-content'."
  (declare (indent 1))
  `(let ((whole-redraws 0))
     (with-temp-buffer
       (org-mode)
       (insert ,text)
       (set-buffer-modified-p nil)
       (advice-add 'skg-replace-buffer-with-new-content :before
                   (lambda (&rest _) (cl-incf whole-redraws))
                   '((name . test-skg-redraw-delta-count)))
       (unwind-protect
           (progn ,@body)
         (advice-remove 'skg-replace-buffer-with-new-content
                        'test-skg-redraw-delta-count)))))

(defun test-skg-redraw-delta--goto-line-containing (string)
  "Move point to the start of the first line containing STRING."
  (goto-char (point-min))
  (search-forward string)
  (beginning-of-line))

(defun test-skg-redraw-delta--whole-redraw-of (content)
  "The text a whole redraw of CONTENT leaves in a buffer."
  (with-temp-buffer
    (org-mode)
    (skg-replace-buffer-with-new-content nil content)
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest test-skg-redraw-delta-a-patch-keeps-folds-and-point ()
  "A save-result naming b rewrites b's line alone: the buffer equals
the stripped new text, b's folded subtree stays folded while b's
headline stays visible, point keeps its place in b's title, and the
buffer is unmodified with the new text as its baseline."
  (should (eq org-fold-core-style 'overlays))
  (test-skg-redraw-delta--in-view test-skg-redraw-delta--drawn
    (test-skg-redraw-delta--goto-line-containing "(vnid b)")
    (org-fold-subtree t)
    (search-forward "title")
    (let ((point-before-title (- (point) (length "title"))))
      (goto-char point-before-title)
      (skg-handle-save-sexp
       (test-skg-redraw-delta--save-result
        test-skg-redraw-delta--b-marked '(b)))
      (should (= whole-redraws 0))
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     (skg--stripped-text test-skg-redraw-delta--b-marked)))
      (should (looking-at "title$"))
      (should (= (point) (+ point-before-title (length " (diff (ready all))")))))
    (test-skg-redraw-delta--goto-line-containing "(vnid b)")
    (should-not (invisible-p (point)))
    (should (invisible-p (save-excursion (search-forward "b body"))))
    (should (invisible-p (save-excursion (search-forward "(vnid c)"))))
    (should-not (invisible-p (save-excursion (search-forward "(vnid d)"))))
    (should-not (buffer-modified-p))
    (should (equal skg-clean-baseline
                   (skg--stripped-text test-skg-redraw-delta--b-marked)))))

(ert-deftest test-skg-redraw-delta-an-empty-delta-edits-nothing ()
  "'(changed-headlines ())' reads as nil, yet names a delta: no headline
changed, so the buffer is not redrawn."
  (test-skg-redraw-delta--in-view test-skg-redraw-delta--drawn
    (let ((tick (buffer-modified-tick)))
      (skg-handle-save-sexp
       (test-skg-redraw-delta--save-result
        test-skg-redraw-delta--drawn ()))
      (should (= whole-redraws 0))
      (should (= tick (buffer-modified-tick))))))

(ert-deftest test-skg-redraw-delta-no-delta-redraws-the-whole-buffer ()
  "A save-result without a delta redraws the whole buffer."
  (test-skg-redraw-delta--in-view test-skg-redraw-delta--drawn
    (skg-handle-save-sexp
     (test-skg-redraw-delta--save-result
      test-skg-redraw-delta--b-marked 'none))
    (should (= whole-redraws 1))))

(ert-deftest test-skg-redraw-delta-a-failed-check-redraws-the-whole-buffer ()
  "When the new text differs from the buffer in a line the delta does
not name, the patch's check fails and the whole buffer is redrawn."
  (let ((content (replace-regexp-in-string
                  "(gnid d)) d" "(gnid d)) d retitled"
                  test-skg-redraw-delta--b-marked)))
    (test-skg-redraw-delta--in-view test-skg-redraw-delta--drawn
      (should-not (skg--patch-buffer-with-delta content '(b)))
      (erase-buffer)
      (insert test-skg-redraw-delta--drawn)
      (setq whole-redraws 0)
      (skg-handle-save-sexp
       (test-skg-redraw-delta--save-result content '(b)))
      (should (= whole-redraws 1))
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     (test-skg-redraw-delta--whole-redraw-of content))))))

(ert-deftest test-skg-redraw-delta-an-unlocatable-viewnode-id-edits-nothing ()
  "A delta naming a viewnode ID the buffer or the new text lacks makes
the patch decline before any edit, and the whole buffer is redrawn."
  (test-skg-redraw-delta--in-view test-skg-redraw-delta--drawn
    (let ((tick (buffer-modified-tick)))
      (should-not (skg--patch-buffer-with-delta
                   test-skg-redraw-delta--b-marked '(b zz)))
      (should (= tick (buffer-modified-tick))))
    (skg-handle-save-sexp
     (test-skg-redraw-delta--save-result
      test-skg-redraw-delta--b-marked '(b zz)))
    (should (= whole-redraws 1))))

(ert-deftest test-skg-redraw-delta-declines-under-text-property-folds ()
  "With 'text-properties' folds a rewritten headline could hide itself,
so the patch declines before any edit."
  (test-skg-redraw-delta--in-view test-skg-redraw-delta--drawn
    (let ((tick (buffer-modified-tick))
          (org-fold-core-style 'text-properties))
      (should-not (skg--patch-buffer-with-delta
                   test-skg-redraw-delta--b-marked '(b)))
      (should (= tick (buffer-modified-tick))))))

(defun test-skg-redraw-delta--herald-display-on-line-containing (string)
  "The display text of the herald on the first line containing STRING."
  (save-excursion
    (test-skg-redraw-delta--goto-line-containing string)
    (search-forward "(skg")
    (let ((overlay (cl-find-if (lambda (overlay) (overlay-get overlay 'display))
                               (overlays-at (match-beginning 0)))))
      (and overlay (substring-no-properties (overlay-get overlay 'display))))))

(ert-deftest test-skg-redraw-delta-re-heralds-a-noClaim-child ()
  "A 'noClaim' child shows a pending detachment only under an editable
parent. When the patch makes its parent editable, the child, whose line
no change hook touches, is re-heralded."
  (skg-test-install-herald-rules)
  (let ((child "** (skg (vnid c) node (gnid c) (skgRepos (nodeRepo r)) noClaim) c\n"))
    (test-skg-redraw-delta--in-view
        (concat "* (skg (vnid p) node (gnid p) (skgRepos (nodeRepo r)) writeProtected) p\n"
                child)
      (heralds-minor-mode 1)
      (should-not (test-skg-redraw-delta--herald-display-on-line-containing
                   "(vnid c)"))
      (should (skg--patch-buffer-with-delta
               (concat "* (skg (vnid p) node (gnid p) (skgRepos (nodeRepo r))) p\n"
                       child)
               '(p)))
      (should (equal (test-skg-redraw-delta--herald-display-on-line-containing
                      "(vnid c)")
                     "detach")))))

(ert-deftest test-skg-redraw-delta-re-heralds-the-folder-above ()
  "A folder's herald reads its members. When the patch makes its one
changed member ready, the folder's herald shows it ready too."
  (skg-test-install-herald-rules)
  (let ((root-and-folder
         (concat "* (skg (vnid r) node (gnid r) (skgRepos (nodeRepo r))) r\n"
                 "** (skg (vnid f) containerFolder)\n"))
        (member "*** (skg (vnid d) node (gnid d) (skgRepos (nodeRepo r)) noClaim writeProtected (diff (rel removed) (leaves 1 rel))) d\n")
        (changed "☮ its containers changed (other files)"))
    (test-skg-redraw-delta--in-view (concat root-and-folder member)
      (heralds-minor-mode 1)
      (should (equal (test-skg-redraw-delta--herald-display-on-line-containing
                      "(vnid f)")
                     changed))
      (should (skg--patch-buffer-with-delta
               (concat root-and-folder
                       (replace-regexp-in-string
                        "(leaves 1 rel)" "(leaves 1 rel) (ready rel)" member))
               '(d)))
      (should (equal (test-skg-redraw-delta--herald-display-on-line-containing
                      "(vnid f)")
                     (concat changed "✓"))))))

(provide 'test-skg-redraw-delta)
