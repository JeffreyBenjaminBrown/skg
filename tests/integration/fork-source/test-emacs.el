;;; Integration test for the fork-confirmation buffer's EDITABLE clone
;;; source. Open owned P (whose content is foreign N); make N definitive
;;; and edit its title; save -> a fork-confirmation buffer. The clone's
;;; source is inferred as "owned"; rotate it to "owned2" in the
;;; confirmation buffer, then approve. The clone must land in "owned2"
;;; (the rotated source), not in "owned".

(load-file "../../../elisp/skg-init.el")
(load-file "../test-wait.el")

(defun test-fail (message &rest args)
  "Report test failure and exit."
  (apply #'message (concat "✗ FAIL: " message) args)
  (kill-emacs 1))

(defun fork-source-test--buffer-showing (id)
  "Return a live skg view buffer whose text mentions (id ID)."
  (seq-find
   (lambda (b)
     (and (buffer-live-p b)
          (with-current-buffer b
            (and (boundp 'skg-view-uri) skg-view-uri
                 (string-match-p (regexp-quote (format "(id %s)" id))
                                 (buffer-substring-no-properties
                                  (point-min) (point-max)))))))
   (buffer-list)))

(defun fork-source-test--skg-files (dir)
  "Names of .skg files in DIR (relative to the test working directory)."
  (and (file-directory-p dir)
       (directory-files dir nil "\\.skg\\'")))

(defun integration-test-fork-source ()
  "Drive edit -> confirm -> rotate clone source -> approve, then assert
the clone landed in the rotated source."
  (message "Starting fork source-rotation integration test...")
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port (setq skg-port (string-to-number test-port))))

  ;; 1. Open owned P; its content is the foreign node N.
  (skg-request-single-root-content-view-from-id "P")
  (let ((p-buf (skg-test-wait-for
                (lambda () (fork-source-test--buffer-showing "P")) 10)))
    (unless p-buf (test-fail "P's view never appeared"))
    (with-current-buffer p-buf
      ;; 2. Make N definitive and edit its title -- the fork gesture.
      (goto-char (point-min))
      (unless (re-search-forward "^.*(id N) (source foreign).*$" nil t)
        (test-fail "could not find N's headline:\n%s" (buffer-string)))
      (let* ((line (match-string 0))
             (edited (replace-regexp-in-string
                      " indef" ""
                      (replace-regexp-in-string
                       "N-original" "N-edited" line))))
        (replace-match edited t t))
      ;; 3. Save -> fork-confirmation (nothing committed).
      (skg-request-save-buffer)))

  ;; 4. The confirmation buffer appears. Rotate the clone-to-be's source
  ;;    from the inferred "owned" to "owned2", then approve.
  (let ((confirm-buf (skg-test-wait-for
                      (lambda () (get-buffer "*SKG Fork Confirmation*")) 10)))
    (unless confirm-buf (test-fail "no fork-confirmation buffer appeared"))
    (with-current-buffer confirm-buf
      (unless (string-match-p "(source owned)" (buffer-string))
        (test-fail "clone-to-be should default to source 'owned':\n%s"
                   (buffer-string)))
      ;; Move to the clone-to-be parent (the first, level-1 headline) and
      ;; rotate its source -- what C-c s s does interactively.
      (goto-char (point-min))
      (unless (re-search-forward "^\\* (skg" nil t)
        (test-fail "could not find the clone-to-be headline:\n%s"
                   (buffer-string)))
      (beginning-of-line)
      (skg--change-source-at-point "owned2")
      (unless (string-match-p "(source owned2)" (buffer-string))
        (test-fail "rotation did not set source owned2:\n%s" (buffer-string)))
      (message "✓ rotated the clone's source to owned2")
      ;; 5. Approve: re-save the origin with the chosen source.
      (skg-approve-fork)))

  ;; 6. The clone must land in owned2 (rotated), NOT owned (inferred).
  (let ((committed (skg-test-wait-for
                    (lambda ()
                      (let ((clones (fork-source-test--skg-files "data/owned2")))
                        (and clones (= (length clones) 1))))
                    10)))
    (unless committed
      (test-fail "no clone appeared in owned2; owned2=%S owned=%S"
                 (fork-source-test--skg-files "data/owned2")
                 (fork-source-test--skg-files "data/owned")))
    (let ((owned-files (fork-source-test--skg-files "data/owned")))
      (unless (equal owned-files '("P.skg"))
        (test-fail "owned should still hold only P.skg (the clone went to owned2), got %S"
                   owned-files)))
    (let* ((clone-file (car (directory-files "data/owned2" t "\\.skg\\'")))
           (content (with-temp-buffer
                      (insert-file-contents clone-file)
                      (buffer-string))))
      (unless (string-match-p "overrides_view_of" content)
        (test-fail "the clone in owned2 should override N:\n%s" content)))
    (message "✓ the clone landed in the rotated source owned2 and overrides N"))

  (message "PASS: Fork source-rotation integration test successful!")
  (kill-emacs 0))

(run-at-time 40 nil (lambda ()
                      (message "TIMEOUT: fork source-rotation test timed out!")
                      (kill-emacs 1)))

(integration-test-fork-source)
