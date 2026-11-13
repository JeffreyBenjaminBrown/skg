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
