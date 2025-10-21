;;; test-skg-org-fold.el --- Tests for skg org folding

(add-to-list
 'load-path
 (expand-file-name
  "../../elisp"
  (file-name-directory load-file-name)))

(require 'ert)
(require 'org)
(require 'skg-org-fold)

(ert-deftest test-skg-fold-round-trip ()
  "Test folding and unfolding with metadata markers."
  (let* ((imaginary-from-rust "* 1
1 body
** 11
*** (skg (id 1) folded) 111
*** (skg (id 2) folded indefinitive) 112
** 12
12 body
*** (skg (id 3) folded) 121
121 body
*** 122
** 13")
         (buf (generate-new-buffer "*test-fold-round-trip*")))
    (with-current-buffer buf
      (org-mode)
      (insert imaginary-from-rust)
      (skg-fold-marked-headlines)
      (should ;; There should be 5 visible lines.
       (= (count-visible-nonempty-lines)
          5))

      (skg-remove-folded-markers)
      (skg-add-folded-markers)
      (let*
          ((expected
            ;; Every sibling of a folded node should be folded
            ;; and folded should be nested under view.
            "* 1
1 body
** 11
*** (skg (id 1) (view folded)) 111
*** (skg (id 2) indefinitive (view folded)) 112
** 12
12 body
*** (skg (id 3) (view folded)) 121
121 body
*** (skg (view folded)) 122
** 13")
           (actual (buffer-substring-no-properties (point-min)
                                                   (point-max))))
        (should (equal actual
                       expected))))
    (kill-buffer buf)))

(ert-deftest test-skg-remove-folded-markers ()
  "Test that skg-remove-folded-markers handles all cases correctly."
  (let ((buf (generate-new-buffer "*test-remove-folded*")))
    (with-current-buffer buf
      (org-mode)
      (insert "* (skg folded) only folded
* (skg folded other) folded first
Body text.
* (skg (key value) folded) folded last
* (skg (k v) folded other) folded middle
* (skg other) no folded")
      (skg-remove-folded-markers)
      (should (equal (buffer-substring-no-properties (point-min)
                                                     (point-max))
                     "* (skg) only folded
* (skg other) folded first
Body text.
* (skg (key value)) folded last
* (skg (k v) other) folded middle
* (skg other) no folded")))
    (kill-buffer buf)))

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
