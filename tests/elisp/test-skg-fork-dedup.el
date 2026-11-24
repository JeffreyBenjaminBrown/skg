;;; test-skg-fork-dedup.el --- Covering what a forks-folder member restates -*- lexical-binding: t; -*-

;; 'elisp/skg-fork-dedup.el': an overlay covers a forks-folder member's
;; title or body where it equals its note's; the text stays as it is.

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'org)
(require 'skg-fork-dedup)
(require 'heralds-minor-mode)

(defconst test-skg-fork-dedup--buffer
  (concat
   "* (skg node (gnid F) (skgRepos (nodeRepo main))) Same words\n"
   "A long body\nthat its forks restate.\n"
   "** (skg forksFolder)\n"
   "*** (skg node (gnid G) (skgRepos (nodeRepo main)) contentInPart) Same words\n"
   "A long body\nthat its forks restate.\n\n"
   "*** (skg node (gnid K) (skgRepos (nodeRepo theirs)) contentInPart) Same words\n"
   "Their own body.\n"
   "*** (skg node (gnid L) (skgRepos (nodeRepo main)) contentInPart) Other words\n"
   "** (skg node (gnid C) (skgRepos (nodeRepo main))) Same words\n"
   "A long body\nthat its forks restate.\n")
  "F, whose forks folder holds G (its title and body), K (its title)
and L (neither), and whose content C restates it outside the folder.")

(defun test-skg-fork-dedup--covers ()
  "Each restatement overlay, in buffer order, as (COVERED-TEXT . SHOWN)."
  (mapcar (lambda (ov)
            (cons (buffer-substring-no-properties (overlay-start ov)
                                                  (overlay-end ov))
                  (substring-no-properties (overlay-get ov 'display))))
          (sort (seq-filter (lambda (ov) (overlay-get ov 'skg-fork-dedup))
                            (overlays-in (point-min) (point-max)))
                (lambda (a b) (< (overlay-start a) (overlay-start b))))))

(defmacro test-skg-fork-dedup--in-buffer (&rest body)
  `(with-temp-buffer
     (insert test-skg-fork-dedup--buffer)
     (skg-fork-dedup-refresh-buffer)
     ,@body))

(ert-deftest test-skg-fork-dedup-covers-what-members-restate ()
  "G's title and body, and K's title, are covered, each independently;
L, F and C, outside a forks folder, are not; the text is unchanged."
  (test-skg-fork-dedup--in-buffer
   (should (equal (test-skg-fork-dedup--covers)
                  '(("Same words" . "<b's title, unchanged>")
                    ("A long body\nthat its forks restate." . "<b's body, unchanged>")
                    ("Same words" . "<b's title, unchanged>"))))
   (should (equal (buffer-string) test-skg-fork-dedup--buffer))
   (should (eq (get-text-property 0 'face
                                  (overlay-get (car (seq-filter
                                                     (lambda (ov) (overlay-get ov 'skg-fork-dedup))
                                                     (overlays-in (point-min) (point-max))))
                                               'display))
               'heralds-message-face))))

(ert-deftest test-skg-fork-dedup-toggles-per-buffer ()
  "The command takes the overlays away and puts them back."
  (test-skg-fork-dedup--in-buffer
   (skg-toggle-fork-dedup-overlay)
   (should-not skg-fork-dedup-overlay)
   (should (null (test-skg-fork-dedup--covers)))
   (skg-toggle-fork-dedup-overlay)
   (should (= 3 (length (test-skg-fork-dedup--covers))))))

(ert-deftest test-skg-fork-dedup-an-edit-that-breaks-equality-uncovers ()
  "Edit G's title, then F's body: each overlay that no longer covers
equal text goes, the others stay."
  (test-skg-fork-dedup--in-buffer
   (goto-char (point-min))
   (search-forward "(gnid G)")
   (end-of-line)
   (insert "!")
   (skg-fork-dedup-refresh-lines (list (point)))
   (should (equal (mapcar #'cdr (test-skg-fork-dedup--covers))
                  '("<b's body, unchanged>" "<b's title, unchanged>")))
   (goto-char (point-min))
   (forward-line 1)
   (end-of-line)
   (insert " More.")
   (skg-fork-dedup-refresh-lines (list (point)))
   (should (equal (mapcar #'cdr (test-skg-fork-dedup--covers))
                  '("<b's title, unchanged>")))))

(ert-deftest test-skg-fork-dedup-a-whole-view-inserted-at-once-is-covered ()
  "A redraw of the whole view inserts it in one edit, whose change hook
reports its first and last lines. A forks folder far from both, here
under a note that is not the view's root, is drawn too."
  (skg-test-install-herald-rules)
  (with-temp-buffer
    (org-mode)
    (heralds-minor-mode 1)
    (insert (concat
             "* (skg node (gnid R)) Root\n"
             (replace-regexp-in-string
              "^\\*" "**" test-skg-fork-dedup--buffer)
             "* (skg node (gnid Z)) Last\n"))
    (should (equal (mapcar #'cdr (test-skg-fork-dedup--covers))
                   '("<b's title, unchanged>" "<b's body, unchanged>"
                     "<b's title, unchanged>")))))
