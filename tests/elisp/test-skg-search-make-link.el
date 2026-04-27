;;; test-skg-search-make-link.el --- Tests for skg-search-make-link

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'skg-id-search)
(require 'skg-search-make-link)

;; ---------- skg-replace-links-with-labels ----------

(ert-deftest test-skg-replace-links-with-labels-no-link ()
  "Plain text with no links returns unchanged."
  (should (equal (skg-replace-links-with-labels "just words")
                 "just words")))

(ert-deftest test-skg-replace-links-with-labels-single ()
  "A single textlink is reduced to its label."
  (should (equal (skg-replace-links-with-labels "[[id:abc][hello]]")
                 "hello")))

(ert-deftest test-skg-replace-links-with-labels-multiple ()
  "Multiple textlinks are each reduced to their label, leaving non-link text intact."
  (should (equal (skg-replace-links-with-labels
                  "[[id:a][one]] and [[id:b][two]] together")
                 "one and two together")))

(ert-deftest test-skg-replace-links-with-labels-mixed ()
  "Headline-like input with leading metadata-free title."
  (should (equal (skg-replace-links-with-labels
                  "before [[id:cf3d9e97][There is no time, only karma.]] after")
                 "before There is no time, only karma. after")))

;; ---------- skg-search-make-link-finish ----------

(ert-deftest test-skg-search-make-link-finish-link-at-point ()
  "Point on an inline result link inserts a link to that link's target."
  (let* ((dest-buf  (generate-new-buffer "*test-link-from-search-dest*"))
         (search-buf (generate-new-buffer "*test-link-from-search-search*"))
         (insert-pos nil))
    (unwind-protect
        (progn
          (with-current-buffer dest-buf
            (insert "before-AFTER")
            (goto-char (point-min))
            (search-forward "AFTER")
            (backward-char 5)
            (setq insert-pos (point)))
          (with-current-buffer search-buf
            (insert "* (skg (node (id outer-id))) outer headline with [[id:inner-id][inner label]] inside\n")
            (goto-char (point-min))
            (search-forward "inner label")
            (backward-char 4)
            ;; The target is the buffer object + position. Skg content
            ;; view buffers are not file-visiting, so we can't use a
            ;; file path here.
            (setq skg--link-from-search-target
                  (cons dest-buf insert-pos))
            (skg-search-make-link-finish))
          ;; The search buffer is killed; dest-buf has the link inserted.
          (should (not (buffer-live-p search-buf)))
          (with-current-buffer dest-buf
            (should (equal (buffer-string)
                           "before-[[id:inner-id][inner label]]AFTER"))))
      (when (buffer-live-p search-buf) (kill-buffer search-buf))
      (when (buffer-live-p dest-buf)   (kill-buffer dest-buf)))))

(ert-deftest test-skg-search-make-link-finish-headline-id ()
  "Point on the headline (not on an inline link) inserts a link to that headline's
ID, with any inline links in the title reduced to their labels."
  (let* ((dest-buf  (generate-new-buffer "*test-link-from-search-dest*"))
         (search-buf (generate-new-buffer "*test-link-from-search-search*"))
         (insert-pos nil))
    (unwind-protect
        (progn
          (with-current-buffer dest-buf
            (insert "X")
            (goto-char (point-max))
            (setq insert-pos (point)))
          (with-current-buffer search-buf
            (insert "* (skg (node (id outer-id))) outer with [[id:inner][inner]] mixed\n")
            (goto-char (point-min))
            ;; Move point onto the metadata sexp so we get the headline ID.
            (search-forward "(skg")
            (backward-char 2)
            (setq skg--link-from-search-target
                  (cons dest-buf insert-pos))
            (skg-search-make-link-finish))
          (with-current-buffer dest-buf
            ;; The headline's title is "outer with [[id:inner][inner]] mixed";
            ;; the new link's label has the inline link reduced.
            (should (equal (buffer-string)
                           "X[[id:outer-id][outer with inner mixed]]"))))
      (when (buffer-live-p search-buf) (kill-buffer search-buf))
      (when (buffer-live-p dest-buf)   (kill-buffer dest-buf)))))

(ert-deftest test-skg-search-make-link-finish-no-target ()
  "Without a buffer-local target, the finish handler raises a user-error."
  (with-temp-buffer
    (setq skg--link-from-search-target nil)
    (should-error (skg-search-make-link-finish) :type 'user-error)))

(provide 'test-skg-search-make-link)
