;;; test-skg-undo-sidecar.el --- Native undo archive tests -*- lexical-binding: t; -*-

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'skg-undo-sidecar)

(defun skg-test--write-private-utf8 (path text)
  (with-temp-buffer
    (insert text)
    (let ((coding-system-for-write 'utf-8-unix)
          (write-region-inhibit-fsync nil))
      (with-file-modes #o600
        (write-region (point-min) (point-max) path nil 'silent)))))

(ert-deftest test-skg-undo-sidecar-refuses-active-undo-tree ()
  (with-temp-buffer
    ;; Do not load undo-tree merely to model its buffer-local mode flag.
    (setq-local undo-tree-mode t)
    (should (string-match-p "undo-tree-mode is active"
                            (skg-undo-sidecar-capability-error)))))

(ert-deftest test-skg-undo-sidecar-public-round-trip ()
  (skip-unless (equal (skg-undo-sidecar-package-version) "0.8"))
  (let* ((directory (make-temp-file "skg-undo-sidecar-test-" t))
         (pseudo-file (expand-file-name "unsaved-changes.org" directory))
         (sidecar (expand-file-name "undo.emacs.gz" directory))
         (buffer (generate-new-buffer " *skg-undo-sidecar-source*")))
    (unwind-protect
        (with-current-buffer buffer
          (org-mode)
          (buffer-disable-undo)
          (insert "* Root λ\nbase\n")
          (buffer-enable-undo)
          (setq buffer-undo-list nil pending-undo-list nil)
          (goto-char (point-max))
          (insert "unsaved café 🐙\n")
          (undo-boundary)
          (let ((text (skg-undo-sidecar--raw-text)))
            (skg-test--write-private-utf8 pseudo-file text)
            (narrow-to-region 2 8)
            (let ((result (skg-undo-sidecar-save
                           buffer pseudo-file sidecar directory)))
              (should (eq (plist-get result :status) 'archived))
              (should (memq (plist-get result :validation)
                            '(undo-redo-probed structurally-validated-apply)))
              (should (file-regular-p sidecar))
              (should (= (logand (file-modes sidecar) #o777) #o600))
              (should (= (point-min) 2))
              (should (= (point-max) 8))
              (should-not buffer-file-name))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest test-skg-undo-sidecar-empty-history-writes-no-fake-file ()
  (skip-unless (equal (skg-undo-sidecar-package-version) "0.8"))
  (let* ((directory (make-temp-file "skg-undo-sidecar-empty-" t))
         (pseudo-file (expand-file-name "unsaved-changes.org" directory))
         (sidecar (expand-file-name "undo.emacs.gz" directory))
         (buffer (generate-new-buffer " *skg-undo-sidecar-empty*")))
    (unwind-protect
        (with-current-buffer buffer
          (insert "unchanged")
          (setq buffer-undo-list nil pending-undo-list nil)
          (skg-test--write-private-utf8 pseudo-file "unchanged")
          (let ((result (skg-undo-sidecar-save
                         buffer pseudo-file sidecar directory)))
            (should (eq (plist-get result :status) 'empty))
            (should-not (file-exists-p sidecar))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest test-skg-undo-sidecar-restores-without-mutating-archive ()
  (skip-unless (equal (skg-undo-sidecar-package-version) "0.8"))
  (let* ((directory (make-temp-file "skg-undo-sidecar-restore-" t))
         (pseudo-file (expand-file-name "unsaved-changes.org" directory))
         (sidecar (expand-file-name "undo.emacs.gz" directory))
         (source (generate-new-buffer " *skg-undo-restore-source*"))
         (target (generate-new-buffer " *skg-undo-restore-target*")))
    (unwind-protect
        (let (text sidecar-before)
          (with-current-buffer source
            (org-mode)
            (buffer-disable-undo)
            (insert "* Root λ\nbase\n")
            (buffer-enable-undo)
            (setq buffer-undo-list nil pending-undo-list nil)
            (goto-char (point-max))
            (insert "unsaved café 🐙\n")
            (undo-boundary)
            (setq text (skg-undo-sidecar--raw-text))
            (skg-test--write-private-utf8 pseudo-file text)
            (should (eq (plist-get
                         (skg-undo-sidecar-save
                          source pseudo-file sidecar directory)
                         :status)
                        'archived)))
          (setq sidecar-before (skg-undo-sidecar--read-bytes sidecar))
          (with-current-buffer target
            (org-mode)
            (buffer-disable-undo)
            (insert text)
            (buffer-enable-undo)
            (setq buffer-undo-list nil pending-undo-list nil)
            (set-buffer-modified-p nil)
            (should (memq
                     (skg-undo-sidecar-restore
                      target pseudo-file sidecar "0.8")
                     '(undo-redo-probed structurally-validated-apply)))
            (should-not buffer-file-name)
            (should (equal text (skg-undo-sidecar--raw-text))))
          (should (equal sidecar-before
                         (skg-undo-sidecar--read-bytes sidecar))))
      (dolist (buffer (list source target))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (delete-directory directory t))))

(provide 'test-skg-undo-sidecar)
