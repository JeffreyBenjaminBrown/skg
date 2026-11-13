;;; test-skg-point-across-redraw.el --- A redraw leaves point and the window where they were -*- lexical-binding: t; -*-

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'cl-lib)
(require 'skg-buffer)
(require 'skg-request-save)

(defun test-skg-point-across-redraw--headlines (count)
  "COUNT headlines, each with a two-line body, numbered from 0, the
headline numbered N carrying viewnode ID vN."
  (mapconcat (lambda (n)
               (format "* (skg (vnid v%d) node (gnid g%d)) title %d\nbody %d one\nbody %d two\n"
                       n n n n n))
             (number-sequence 0 (1- count)) ""))

(defun test-skg-point-across-redraw--goto (string)
  "Move point to the start of the first occurrence of STRING."
  (goto-char (point-min))
  (search-forward string)
  (goto-char (match-beginning 0)))

(ert-deftest test-a-saves-screen-line-measure-recenters-to-the-same-window-start ()
  "Recentering on the screen line a save measures for point leaves the
window's start where it was, with point at a line's start or inside it."
  (save-window-excursion
    (with-temp-buffer
      (insert (test-skg-point-across-redraw--headlines 30))
      (org-mode)
      (set-window-buffer (selected-window) (current-buffer))
      (dolist (column '(0 3))
        (test-skg-point-across-redraw--goto "body 12 one")
        (forward-char column)
        (recenter 4)
        (let ((window-start-before (window-start))
              (measured (skg--current-save-point-position)))
          (should (= 4 (plist-get measured
                                  :point-screen-lines-below-window-start)))
          (skg--recenter-current-buffer-window
           (plist-get measured :point-screen-lines-below-window-start))
          (should (= window-start-before (window-start))))))))

(defun test-skg-point-across-redraw--rerender (content)
  "Apply CONTENT as the rerender of view \"view-point\"."
  (skg--apply-streamed-view-update
   (prin1-to-string `((view-id "view-point") (content ,content)))
   'rerender "rerender-view"))

(defmacro test-skg-point-across-redraw--in-view (content &rest body)
  "Run BODY in a content-view buffer of view \"view-point\" holding CONTENT."
  (declare (indent 1))
  `(with-temp-buffer
     (skg-content-view-mode)
     (setq-local skg-view-id "view-point")
     (insert ,content)
     (skg--capture-clean-baseline)
     ,@body))

(defun test-skg-point-across-redraw--changed-elsewhere (count)
  "The view of COUNT headlines as another writer's change redraws it: a
new headline above the rest, headline 0 still focused by the last
save, and headline 1's body grown by a line."
  (concat "* (skg (vnid w0) node (gnid new)) a new first headline\n"
          (replace-regexp-in-string
           "body 1 two\n" "body 1 two\nbody 1 three\n"
           (replace-regexp-in-string
            "(vnid v0) " "(vnid v0) focused "
            (test-skg-point-across-redraw--headlines count)))))

(ert-deftest test-a-redraw-not-asked-for-keeps-point-on-its-viewnode ()
  "A rerender that adds lines above point, and carries the last save's
focus elsewhere, leaves point on the same line and column of its
headline's entry."
  (test-skg-point-across-redraw--in-view
      (test-skg-point-across-redraw--headlines 6)
    (test-skg-point-across-redraw--goto "body 4 two")
    (forward-char 3)
    (test-skg-point-across-redraw--rerender
     (test-skg-point-across-redraw--changed-elsewhere 6))
    (should (looking-at "y 4 two"))))

(ert-deftest test-a-redraw-not-asked-for-moves-point-to-the-nearest-remaining-headline ()
  "When point's viewnode is gone, point goes to the start of the
headline nearest it that remains, not to the top."
  (test-skg-point-across-redraw--in-view
      (test-skg-point-across-redraw--headlines 6)
    (test-skg-point-across-redraw--goto "body 3 one")
    (test-skg-point-across-redraw--rerender
     (replace-regexp-in-string
      "\\* (skg (vnid v3) .*\nbody 3 one\nbody 3 two\n" ""
      (test-skg-point-across-redraw--headlines 6)))
    (should (looking-at "\\* (skg (vnid v4) "))))

(ert-deftest test-a-redraw-not-asked-for-keeps-each-windows-point-and-scroll ()
  "Two windows showing the view each keep point on its viewnode, on the
same screen line, though the rerender adds lines above both."
  (save-window-excursion
    (delete-other-windows)
    (test-skg-point-across-redraw--in-view
        (test-skg-point-across-redraw--headlines 40)
      (let* ((selected (selected-window))
             (other (split-window)))
        (set-window-buffer selected (current-buffer))
        (set-window-buffer other (current-buffer))
        (test-skg-point-across-redraw--goto "body 20 one")
        (forward-char 2)
        (recenter 3)
        (with-selected-window other
          (test-skg-point-across-redraw--goto "body 30 two")
          (recenter 5))
        (test-skg-point-across-redraw--rerender
         (test-skg-point-across-redraw--changed-elsewhere 40))
        (should (looking-at "dy 20 one"))
        (should (equal 3 (plist-get (skg--current-save-point-position selected)
                                    :point-screen-lines-below-window-start)))
        (with-selected-window other
          (should (looking-at "body 30 two"))
          (should (equal 5 (plist-get (skg--current-save-point-position other)
                                      :point-screen-lines-below-window-start))))))))
