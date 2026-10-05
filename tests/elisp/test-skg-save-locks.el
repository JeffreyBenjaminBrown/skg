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

(provide 'test-skg-save-locks)
