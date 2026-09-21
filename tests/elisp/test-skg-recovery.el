;;; test-skg-recovery.el --- Clean baselines and detached recovery

(require 'ert)
(require 'json)
(require 'skg-client)

(defun test-skg-recovery--json-snapshot (document heading)
  "Decode the JSON snapshot below HEADING in DOCUMENT."
  (let ((pattern
         (concat "\\*\\* " (regexp-quote heading)
                 "\n#\\+begin_src json\n\\([^\n]+\\)"
                 "\n#\\+end_src")))
    (should (string-match pattern document))
    (json-read-from-string (match-string 1 document))))

(ert-deftest test-skg-recovery-baseline-and-current-round-trip-losslessly ()
  "JSON snapshots preserve delimiter-like text, Unicode, tabs, and EOF shape."
  (let* ((baseline "* old\n#+end_src\n\tλ\n")
         (current "* new\n#+begin_src json\n\t雪")
         (buffer (generate-new-buffer "*skg recovery source*")))
    (unwind-protect
        (with-current-buffer buffer
          (insert baseline)
          (skg-content-view-mode)
          (setq skg-view-uri "recovery-uri")
          (set-buffer-modified-p nil)
          (skg--capture-clean-baseline)
          (erase-buffer)
          (insert current)
          (let ((document
                 (skg--unsaved-changes-document
                  skg-clean-baseline (buffer-string))))
            (should (equal baseline
                           (test-skg-recovery--json-snapshot
                            document "Clean baseline")))
            (should (equal current
                           (test-skg-recovery--json-snapshot
                            document "Current text")))
            (should (string-match-p "^-\\* old" document))
            (should (buffer-modified-p))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest test-skg-show-unsaved-changes-is-detached-and-preserves-source ()
  "Writing and opening recovery neither cleans nor converts the source view."
  (let ((path (make-temp-file "skg-recovery-" nil ".org"))
        (source (generate-new-buffer "*skg recovery live*"))
        archive)
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert "* baseline\n")
            (skg-content-view-mode)
            (setq skg-view-uri "live-uri")
            (set-buffer-modified-p nil)
            (skg--capture-clean-baseline)
            (goto-char (point-max))
            (insert "unsaved\n")
            (skg-show-unsaved-changes path t))
          (setq archive (current-buffer))
          (should-not (buffer-local-value 'skg-view-uri archive))
          (should (with-current-buffer source (buffer-modified-p)))
          (should (equal "live-uri"
                         (buffer-local-value 'skg-view-uri source)))
          (kill-buffer source)
          (should (buffer-live-p archive))
          (should (file-exists-p path)))
      (when (buffer-live-p source)
        (with-current-buffer source (set-buffer-modified-p nil))
        (kill-buffer source))
      (when (buffer-live-p archive) (kill-buffer archive))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest test-skg-recovery-refused-overwrite-and-write-error-preserve-edits ()
  (let ((existing (make-temp-file "skg-recovery-existing-"))
        (directory (make-temp-file "skg-recovery-directory-" t))
        (source (generate-new-buffer "*skg recovery errors*")))
    (unwind-protect
        (with-current-buffer source
          (insert "* baseline\n")
          (skg-content-view-mode)
          (setq skg-view-uri "error-uri")
          (set-buffer-modified-p nil)
          (skg--capture-clean-baseline)
          (goto-char (point-max))
          (insert "unsaved")
          (with-temp-file existing (insert "do not replace"))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
            (should-error (skg-show-unsaved-changes existing)
                          :type 'user-error))
          (should (equal "do not replace"
                         (with-temp-buffer
                           (insert-file-contents existing)
                           (buffer-string))))
          (should-error (skg-show-unsaved-changes directory t) :type 'error)
          (should (buffer-modified-p))
          (should (equal "error-uri" skg-view-uri)))
      (when (buffer-live-p source)
        (with-current-buffer source (set-buffer-modified-p nil))
        (kill-buffer source))
      (when (file-exists-p existing) (delete-file existing))
      (when (file-directory-p directory) (delete-directory directory)))))

(provide 'test-skg-recovery)
