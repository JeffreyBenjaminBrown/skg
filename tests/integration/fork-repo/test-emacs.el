;;; Integration test for the fork-confirmation buffer's EDITABLE clone
;;; skgrepo. Open owned P (whose content is foreign N); make N editable
;;; and edit its title; save -> a fork-confirmation buffer. The clone's
;;; skgrepo is inferred as "owned"; rotate it to "owned2" in the
;;; confirmation buffer, then approve. The clone must land in "owned2"
;;; (the rotated skgrepo), not in "owned".

(load-file "../../../elisp/skg-init.el")
(load-file "../test-wait.el")

(defun test-fail (message &rest args)
  "Report test failure and exit."
  (apply #'message (concat "✗ FAIL: " message) args)
  (kill-emacs 1))

(defun fork-repo-test--buffer-showing (skgid)
  "Return a live skg view buffer whose text mentions (id ID)."
  (seq-find
   (lambda (b)
     (and (buffer-live-p b)
          (with-current-buffer b
            (and (boundp 'skg-view-uri) skg-view-uri
                 (string-match-p (regexp-quote (format "(id %s)" skgid))
                                 (buffer-substring-no-properties
                                  (point-min) (point-max)))))))
   (buffer-list)))

(defun fork-repo-test--skg-files (dir)
  "Names of .skg files in DIR (relative to the test working directory)."
  (and (file-directory-p dir)
       (directory-files dir nil "\\.skg\\'")))

(defun integration-test-fork-repo ()
  "Drive edit -> confirm -> rotate clone repo -> approve, then assert
the clone landed in the rotated repo."
  (message "Starting fork repo-rotation integration test...")
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port (setq skg-port (string-to-number test-port))))

  ;; 1. Open owned P; its content is the foreign node N.
  (skg-request-single-root-content-view-from-id "P")
  (let ((p-buf (skg-test-wait-for
                (lambda () (fork-repo-test--buffer-showing "P")) 10)))
    (unless p-buf (test-fail "P's view never appeared"))
    (with-current-buffer p-buf
      ;; 2. Make N editable and edit its title -- the fork gesture.
      (goto-char (point-min))
      (unless (re-search-forward "^.*(id N) (repo foreign).*$" nil t)
        (test-fail "could not find N's headline:\n%s" (buffer-string)))
      (let* ((line (match-string 0))
             (edited (replace-regexp-in-string
                      " writeProtected" ""
                      (replace-regexp-in-string
                       "N-original" "N-edited" line))))
        (replace-match edited t t))
      ;; 3. Save -> fork-confirmation (nothing committed).
      (skg-request-save-buffer)))

  ;; 4. The confirmation buffer appears. Rotate the clone-to-be's skgrepo
  ;;    from the inferred "owned" to "owned2", then approve.
  (let ((confirm-buf (skg-test-wait-for
                      (lambda () (get-buffer "*SKG Fork Confirmation*")) 10)))
    (unless confirm-buf (test-fail "no fork-confirmation buffer appeared"))
    (with-current-buffer confirm-buf
      ;; The server pre-fills the PICK-A-REPO placeholder and only
      ;; SUGGESTS the inferred skgrepo in a comment line (fork.rs;
      ;; documented in docs/COMMANDS.org and glossary.org). An earlier
      ;; version of this test asserted "(repo owned)" directly,
      ;; which predates the placeholder mechanism -- see the 2026-07-02
      ;; entry in TODO/problems.org.
      (unless (string-match-p "(repo PICK-A-REPO)" (buffer-string))
        (test-fail "clone-to-be should carry the PICK-A-REPO placeholder:\n%s"
                   (buffer-string)))
      (unless (string-match-p "Suggested repo for the clone below: owned"
                              (buffer-string))
        (test-fail "the inferred repo 'owned' should be suggested:\n%s"
                   (buffer-string)))
      ;; Move to the clone-to-be parent (the first, level-1 headline) and
      ;; rotate its skgrepo -- what C-c s s does interactively.
      (goto-char (point-min))
      (unless (re-search-forward "^\\* (skg" nil t)
        (test-fail "could not find the clone-to-be headline:\n%s"
                   (buffer-string)))
      (beginning-of-line)
      (skg--change-repo-at-point "owned2")
      (unless (string-match-p "(repo owned2)" (buffer-string))
        (test-fail "rotation did not set repo owned2:\n%s" (buffer-string)))
      (message "✓ rotated the clone's repo to owned2")
      ;; 5. Approve: re-save the source buffer with the chosen skgrepo.
      (skg-approve-fork)))

  ;; 6. The clone must land in owned2 (rotated), NOT owned (inferred).
  (let ((committed (skg-test-wait-for
                    (lambda ()
                      (let ((clones (fork-repo-test--skg-files "data/owned/owned2")))
                        (and clones (= (length clones) 1))))
                    10)))
    (unless committed
      (test-fail "no clone appeared in owned2; owned2=%S owned=%S"
                 (fork-repo-test--skg-files "data/owned/owned2")
                 (fork-repo-test--skg-files "data/owned/owned")))
    (let ((owned-files (fork-repo-test--skg-files "data/owned/owned")))
      (unless (equal owned-files '("P.skg"))
        (test-fail "owned should still hold only P.skg (the clone went to owned2), got %S"
                   owned-files)))
    (let* ((clone-file (car (directory-files "data/owned/owned2" t "\\.skg\\'")))
           (content (with-temp-buffer
                      (insert-file-contents clone-file)
                      (buffer-string))))
      (unless (string-match-p "overrides_view_of" content)
        (test-fail "the clone in owned2 should override N:\n%s" content)))
    (message "✓ the clone landed in the rotated repo owned2 and overrides N"))

  (message "PASS: Fork repo-rotation integration test successful!")
  (kill-emacs 0))

(run-at-time 40 nil (lambda ()
                      (message "TIMEOUT: fork repo-rotation test timed out!")
                      (kill-emacs 1)))

(integration-test-fork-repo)
