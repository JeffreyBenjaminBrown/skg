;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'skg-request-save)

(ert-deftest test-skg-save-lock-broad-ack-retains-client-only-view ()
  (let ((saved (generate-new-buffer " *skg-lock-saved*"))
        (other (generate-new-buffer " *skg-lock-other*")))
    (unwind-protect
        (progn
          (with-current-buffer saved
            (setq-local skg-view-id "view-id-saved"))
          (with-current-buffer other
            (setq-local skg-view-id "view-id-client-only"))
          (skg--lock-all-skg-buffers)
          (skg--broad-save-lock-handler
           "((response-type save-lock) (lock-views (\"view-id-saved\")))")
          (should (buffer-local-value 'skg--save-lock-overlay saved))
          (should (buffer-local-value 'skg--save-lock-overlay other)))
      (skg--unlock-all-save-locked)
      (kill-buffer saved)
      (kill-buffer other))))

(ert-deftest test-skg-save-relax-lock-keeps-dirty-input-and-frees-clean-view ()
  (let ((saved (generate-new-buffer " *skg-relax-saved*"))
        (dirty (generate-new-buffer " *skg-relax-dirty*"))
        (clean (generate-new-buffer " *skg-relax-clean*")))
    (unwind-protect
        (progn
          (with-current-buffer saved
            (setq-local skg-view-id "view-id-saved"))
          (with-current-buffer dirty
            (setq-local skg-view-id "view-id-dirty"))
          (with-current-buffer clean
            (setq-local skg-view-id "view-id-clean"))
          (skg--lock-all-skg-buffers)
          (skg--save-relax-lock-handler
           "view-id-saved"
           "((response-type save-relax-lock) (lock-views (view-id-dirty)))")
          (should (buffer-local-value 'skg--save-lock-overlay saved))
          (should (buffer-local-value 'skg--save-lock-overlay dirty))
          (should-not (buffer-local-value 'skg--save-lock-overlay clean)))
      (skg--unlock-all-save-locked)
      (kill-buffer saved)
      (kill-buffer dirty)
      (kill-buffer clean))))

(ert-deftest test-skg-malformed-save-relax-lock-retains-all-locks ()
  (let ((saved (generate-new-buffer " *skg-malformed-saved*"))
        (other (generate-new-buffer " *skg-malformed-other*")))
    (unwind-protect
        (progn
          (with-current-buffer saved
            (setq-local skg-view-id "view-id-saved"))
          (with-current-buffer other
            (setq-local skg-view-id "view-id-other"))
          (skg--lock-all-skg-buffers)
          (skg--save-relax-lock-handler
           "view-id-saved"
           "((response-type save-relax-lock) (lock-views malformed))")
          (should (buffer-local-value 'skg--save-lock-overlay saved))
          (should (buffer-local-value 'skg--save-lock-overlay other)))
      (skg--unlock-all-save-locked)
      (kill-buffer saved)
      (kill-buffer other))))

;; A collateral-view for a buffer with unsaved edits leaves its text and
;; baseline as they are; one for a clean buffer replaces them.
(ert-deftest test-skg-collateral-view-leaves-a-modified-buffer-alone ()
  (let ((modified (generate-new-buffer " *skg-collateral-modified*"))
        (clean (generate-new-buffer " *skg-collateral-clean*")))
    (unwind-protect
        (progn
          (dolist (buffer-and-id `((,modified . "view-id-modified")
                                   (,clean . "view-id-clean")))
            (with-current-buffer (car buffer-and-id)
              (skg-content-view-mode)
              (setq-local skg-view-id (cdr buffer-and-id))
              (insert "* (skg node (gnid a)) a\n")
              (skg--capture-clean-baseline)
              (set-buffer-modified-p nil)))
          (with-current-buffer modified
            (goto-char (point-max))
            (insert "unsaved edit\n"))
          (should (buffer-modified-p modified))
          (skg--collateral-view-handler
           "((response-type collateral-view) (view-id \"view-id-modified\") (content \"* (skg node (gnid a)) a2\n\"))")
          (skg--collateral-view-handler
           "((response-type collateral-view) (view-id \"view-id-clean\") (content \"* (skg node (gnid a)) a2\n\"))")
          (with-current-buffer modified
            (should (equal (buffer-string)
                           "* (skg node (gnid a)) a\nunsaved edit\n"))
            (should (equal skg-clean-baseline "* (skg node (gnid a)) a\n"))
            (should (buffer-modified-p)))
          (with-current-buffer clean
            (should (equal skg-clean-baseline
                           "* (skg node (gnid a)) a2\n"))))
      (kill-buffer modified)
      (kill-buffer clean))))

(provide 'test-skg-save-locks)
