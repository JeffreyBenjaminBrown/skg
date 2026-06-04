;;; test-skg-save-response-folded-root.el --- Repro for "root vanishes" bug

;; Reproduces the bug where a single-root content view, saved while
;; fully folded (org-startup-folded behavior), comes back with the
;; root headline missing from the buffer text. See Dear_Claude.org
;; and the companion Rust test at tests/save/folded_single_root.rs.
;;
;; The Rust test proves the server round-trip preserves the root, so
;; the suspect is `skg-replace-buffer-with-new-content' and the
;; client-side fold/unfold + focus processing it runs.
;;
;; ROOT CAUSE (exposed by the failing test below):
;; After `skg-fold-marked-headlines' + `skg-remove-folded-markers'
;; run, the root headline is folded (its subtree hidden by an
;; org-fold overlay). Then `skg-remove-focused-marker' finds the
;; root, and `skg-edit-metadata-at-point' calls
;; `skg-replace-current-line', which issues a
;; `delete-region' on (line-beginning-position . line-end-position).
;; Because that region starts at the beginning of a folded heading,
;; org-fold's `org-fold-core--fix-folded-region' expands the delete
;; to cover the entire hidden subtree. The root line (and its
;; children's text) is clobbered, and only the reformatted line is
;; re-inserted at point. The test that has `focused' on the root
;; FAILS; the test without `focused' PASSES — confirming the trigger
;; is the focus-removal step operating on a folded heading.

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'org)
(require 'skg-org-fold)
(require 'skg-focus)
(require 'skg-request-save)

(defun skg-test--buffer-text ()
  (buffer-substring-no-properties (point-min) (point-max)))

(ert-deftest test-save-response-with-folded-children-preserves-root ()
  "After a folded single-root save, the server's response carries the
root with `focused' metadata and each child with `folded' metadata.
`skg-replace-buffer-with-new-content' must NOT drop the root line."
  (let* ((from-rust
          (concat
           "* (skg focused (node (id root))) root\n"
           "root body\n"
           "** (skg folded (node (id c1))) c1\n"
           "c1 body\n"
           "** (skg folded (node (id c2))) c2\n"
           "c2 body\n"))
         (buf (generate-new-buffer "*test-save-response-folded-root*")))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (setq-local skg-view-uri "test-uri")
          (skg-replace-buffer-with-new-content nil from-rust)
          (let ((actual (skg-test--buffer-text)))
            (should (string-match-p "(id root)" actual))
            (should (string-match-p "^\\* " actual))
            (should (string-match-p "(id c1)" actual))
            (should (string-match-p "(id c2)" actual))))
      (kill-buffer buf))))

(ert-deftest test-save-response-no-focused-preserves-root ()
  "Same as above but without a `focused' marker on the root, to
isolate the fold-processing path from the focus-processing path."
  (let* ((from-rust
          (concat
           "* (skg (node (id root))) root\n"
           "root body\n"
           "** (skg folded (node (id c1))) c1\n"
           "c1 body\n"
           "** (skg folded (node (id c2))) c2\n"
           "c2 body\n"))
         (buf (generate-new-buffer "*test-save-response-folded-root-nofocus*")))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (setq-local skg-view-uri "test-uri")
          (skg-replace-buffer-with-new-content nil from-rust)
          (let ((actual (skg-test--buffer-text)))
            (should (string-match-p "(id root)" actual))
            (should (string-match-p "^\\* " actual))))
      (kill-buffer buf))))

(ert-deftest test-save-response-restores-point-in-focused-body ()
  "Point returns to the same body line below the focused headline."
  (let* ((from-rust
          (concat
           "* (skg focused (node (id root))) root\n"
           "body 1\n"
           "body 2\n"
           "** (skg (node (id child))) child\n"))
         (buf (generate-new-buffer "*test-save-response-body-point*")))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (setq-local skg-view-uri "test-uri")
          (skg-replace-buffer-with-new-content
           nil
           from-rust
           '(:point-lines-below-focused-headline 2
             :point-screen-lines-below-window-start 0))
          (should (string= "body 2"
                           (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position)))))
      (kill-buffer buf))))

(ert-deftest test-save-response-leaves-point-on-focused-headline-when-body-line-vanishes ()
  "If the saved body line is gone, point stays on the focused headline."
  (let* ((from-rust
          (concat
           "* (skg focused (node (id root))) root\n"
           "** (skg (node (id child))) child\n"))
         (buf (generate-new-buffer "*test-save-response-vanished-body-point*")))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (setq-local skg-view-uri "test-uri")
          (skg-replace-buffer-with-new-content
           nil
           from-rust
           '(:point-lines-below-focused-headline 2
             :point-screen-lines-below-window-start 0))
          (should (string-match-p
                   "^\\* .*root$"
                   (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position)))))
      (kill-buffer buf))))

(ert-deftest test-save-response-success-with-warnings-shows_warning_channel ()
  (let ((buf (generate-new-buffer "*test-save-response-warning*"))
        (shown nil))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (cl-letf (((symbol-function 'skg-replace-buffer-with-new-content)
                     (lambda (_tcp-proc new-content &optional _position)
                       (erase-buffer)
                       (insert new-content)))
                    ((symbol-function 'skg-big-nonfatal-message)
                     (lambda (buffer-name message-text content)
                       (setq shown
                             (list buffer-name message-text content)))))
            (skg-handle-save-sexp
             (prin1-to-string
              '((content "* root\n")
                (errors ())
                (warnings ("audit warning")))))
            (should (string= (buffer-string) "* root\n"))
            (should (equal (car shown) "*SKG Save Warnings*"))
            (should (string-match-p "^\\* warnings\n\\*\\* audit warning"
                                    (nth 2 shown)))))
      (kill-buffer buf))))

(ert-deftest test-save-response-failure-with-errors-and-warnings-shows_both ()
  (let ((buf (generate-new-buffer "*test-save-response-errors-warnings*"))
        (shown nil)
        (replaced nil))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (cl-letf (((symbol-function 'skg-replace-buffer-with-new-content)
                     (lambda (&rest _args)
                       (setq replaced t)))
                    ((symbol-function 'skg-big-nonfatal-message)
                     (lambda (buffer-name message-text content)
                       (setq shown
                             (list buffer-name message-text content)))))
            (skg-handle-save-sexp
             (prin1-to-string
              '((content nil)
                (errors ("fatal save error"))
                (warnings ("audit warning")))))
            (should-not replaced)
            (should (equal (car shown) "*SKG Save Errors and Warnings*"))
            (should (string-match-p "^\\* errors\n\\*\\* fatal save error"
                                    (nth 2 shown)))
            (should (string-match-p "^\\* warnings\n\\*\\* audit warning"
                                    (nth 2 shown)))))
      (kill-buffer buf))))

(provide 'test-skg-save-response-folded-root)
