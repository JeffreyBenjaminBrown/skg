;;; test-skg-view-new-empty.el --- Tests for blank new content views

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'cl-lib)
(require 'skg-view-new-empty)

(ert-deftest test-skg-view-new-empty-opens-blank-content-view ()
  "The new-view command must not invent a node or ask for its source."
  (let (opened)
    (unwind-protect
        (cl-letf (((symbol-function 'skg--prompt-for-owned-source)
                   (lambda ()
                     (ert-fail "A blank view must not prompt for a source"))))
          (skg-view-new-empty)
          (setq opened (current-buffer))
          (should (equal (buffer-string) ""))
          (should (derived-mode-p 'skg-content-view-mode))
          (should (stringp skg-view-uri)))
      (when (buffer-live-p opened)
        (set-buffer-modified-p nil)
        (kill-buffer opened)))))

(provide 'test-skg-view-new-empty)
