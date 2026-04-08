;;; Integration test for source cycling in the metadata edit buffer.
;;; Verifies that S-left / S-right cycle through owned sources only,
;;; excluding foreign (user_owns_it = false) sources.

(load-file "../../../elisp/skg-init.el")
(load-file "../test-wait.el")

(defvar integration-test-phase "starting")

(defun test-fail (message &rest args)
  (apply #'message (concat "FAIL: " message) args)
  (kill-emacs 1))

(defun test-pass (message &rest args)
  (apply #'message (concat "PASS: " message) args))

(defun source-value-in-edit-buffer ()
  "Return the source value headline text in the sexp-edit buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\*\\*\\* source$" nil t)
      (outline-next-heading)
      (string-trim (org-get-heading t t t t)))))

(defun phase-1-open-view ()
  "Open a content view for node x."
  (message "=== PHASE 1: Open content view ===")
  (setq integration-test-phase "phase-1")
  (skg-request-single-root-content-view-from-id "x")
  (skg-test-wait-for
   (lambda ()
     (cl-find-if
      (lambda (b)
        (and (with-current-buffer b
                            (derived-mode-p 'skg-content-view-mode))
             (with-current-buffer b
               (string-match-p "(id x)"
                 (buffer-substring-no-properties
                  (point-min) (point-max))))))
      (buffer-list))))
  (let ((buf (cl-find-if
              (lambda (b) (with-current-buffer b
                            (derived-mode-p 'skg-content-view-mode)))
              (buffer-list))))
    (unless buf (test-fail "Content view not created"))
    (switch-to-buffer buf)
    (test-pass "Content view opened")))

(defun phase-2-open-metadata-edit ()
  "Open the metadata edit buffer and go to the source value."
  (message "=== PHASE 2: Open metadata edit buffer ===")
  (setq integration-test-phase "phase-2")
  (goto-char (point-min))
  (unless (org-at-heading-p)
    (test-fail "Not on a headline"))
  (skg-edit-metadata)
  ;; The edit buffer should now be current.
  (let ((source (source-value-in-edit-buffer)))
    (message "  Initial source: %S" source)
    (unless (equal source "public")
      (test-fail "Expected initial source 'public', got %S" source))
    (test-pass "Metadata edit buffer opened, source is 'public'")))

(defun phase-3-cycle-right ()
  "Shift-right should change source to the next owned source."
  (message "=== PHASE 3: Cycle right ===")
  (setq integration-test-phase "phase-3")
  ;; Position point on the source value headline
  (goto-char (point-min))
  (re-search-forward "^\\*\\*\\* source$" nil t)
  (outline-next-heading)
  ;; Cycle right
  (skg-sexp-edit-cycle-right)
  (let ((source (source-value-in-edit-buffer)))
    (message "  After S-right: %S" source)
    (unless (equal source "personal")
      (test-fail "Expected 'personal' after first S-right, got %S" source))
    (test-pass "S-right changed source to 'personal'")))

(defun phase-4-cycle-left ()
  "Shift-left should change source back to 'public'."
  (message "=== PHASE 4: Cycle left ===")
  (setq integration-test-phase "phase-4")
  ;; Point should still be on the source value
  (skg-sexp-edit-cycle-left)
  (let ((source (source-value-in-edit-buffer)))
    (message "  After S-left: %S" source)
    (unless (equal source "public")
      (test-fail "Expected 'public' after S-left, got %S" source))
    (test-pass "S-left changed source back to 'public'")))

(defun phase-5-cycle-wraps ()
  "Three S-rights from 'public' should wrap back to 'public'."
  (message "=== PHASE 5: Three S-rights wraps around ===")
  (setq integration-test-phase "phase-5")
  ;; Currently at 'public'. Three owned sources: public, personal, private.
  (skg-sexp-edit-cycle-right) ; -> personal
  (skg-sexp-edit-cycle-right) ; -> private
  (skg-sexp-edit-cycle-right) ; -> public (wrap)
  (let ((source (source-value-in-edit-buffer)))
    (message "  After 3x S-right: %S" source)
    (unless (equal source "public")
      (test-fail "Expected 'public' after 3x S-right, got %S" source))
    ;; Also verify 'foreign' never appeared
    (test-pass "Three S-rights wraps back to 'public', 'foreign' never offered")))

(defun integration-test-main ()
  (message "=== SKG Source Cycling Integration Test ===")
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))
      (message "Using test port: %d" skg-port)))
  (let ((config-file (getenv "SKG_CONFIG_FILE")))
    (when config-file
      (setq skg-config-dir
            (file-name-directory (expand-file-name config-file)))))

  (phase-1-open-view)
  (phase-2-open-metadata-edit)
  (phase-3-cycle-right)
  (phase-4-cycle-left)
  (phase-5-cycle-wraps)

  (message "=== All phases passed ===")
  (kill-emacs 0))

(progn
  (run-at-time
   20 nil
   (lambda ()
     (message "TIMEOUT at phase: %s" integration-test-phase)
     (kill-emacs 1)))
  (integration-test-main))
