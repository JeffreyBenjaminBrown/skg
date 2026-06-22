;;; Integration test for the fork gesture and its confirmation stage.
;;; Open owned P (whose content is foreign N); make N definitive and
;;; edit its title; save -> a fork-confirmation buffer appears and
;;; nothing commits; approve -> the clone is created (overriding N) and
;;; drawn in N's place when P re-renders.

(load-file "../../../elisp/skg-init.el")
(load-file "../test-wait.el")

(defun test-fail (message &rest args)
  "Report test failure and exit."
  (apply #'message (concat "✗ FAIL: " message) args)
  (kill-emacs 1))

(defun fork-test--buffer-showing (id)
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

(defun integration-test-fork ()
  "Drive the edit -> confirm -> approve -> fork flow end to end."
  (message "Starting fork integration test...")
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port (setq skg-port (string-to-number test-port))))

  ;; 1. Open the owned container P; its content is the foreign node N.
  (skg-request-single-root-content-view-from-id "P")
  (let ((p-buf (skg-test-wait-for (lambda () (fork-test--buffer-showing "P")) 10)))
    (unless p-buf (test-fail "P's view never appeared"))
    (with-current-buffer p-buf
      (unless (string-match-p "(id N)" (buffer-string))
        (test-fail "P's view does not show its foreign content N:\n%s"
                   (buffer-string)))

      ;; 2. Make N definitive (drop its 'indef' marker) and edit its
      ;;    title -- the fork gesture.
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

  ;; 4. The confirmation buffer appears and lists N.
  (let ((confirm-buf (skg-test-wait-for
                      (lambda () (get-buffer "*SKG Fork Confirmation*")) 10)))
    (unless confirm-buf (test-fail "no fork-confirmation buffer appeared"))
    (with-current-buffer confirm-buf
      (unless (string-match-p "(id N)" (buffer-string))
        (test-fail "confirmation buffer does not list N:\n%s" (buffer-string)))
      (unless (string-match-p "FORK CONFIRMATION" (buffer-string))
        (test-fail "confirmation buffer lacks its header:\n%s" (buffer-string)))
      (message "✓ fork-confirmation buffer lists N")

      ;; 5. Pick the clone's source (required), then approve.
      (goto-char (point-min))
      (re-search-forward "^\\* (skg (node (source ")
      (beginning-of-line)
      (skg--change-source-at-point "owned")
      (skg-approve-fork)))

  ;; 6. The fork commits: when P's saved view re-renders, N is now
  ;;    overridden and subscribed (its graphStats say so). (The saved
  ;;    view still draws N raw -- existing viewnodes are not rewritten;
  ;;    substitution shows on a fresh open, below.)
  (let ((committed (skg-test-wait-for
                    (lambda ()
                      (let ((p-buf (fork-test--buffer-showing "P")))
                        (and p-buf
                             (with-current-buffer p-buf
                               (string-match-p "(rels \"1S 1O\")"
                                               (buffer-string))))))
                    10)))
    (unless committed
      (let ((p-buf (fork-test--buffer-showing "P")))
        (test-fail "the fork did not commit after approval:\n%s"
                   (if p-buf (with-current-buffer p-buf (buffer-string)) "<no P buffer>"))))
    (message "✓ fork committed (N is now overridden and subscribed)"))

  ;; 7. Reopen P fresh: override substitution now draws the clone in N's
  ;;    place, carrying (overridesHere N).
  (let ((p-buf (fork-test--buffer-showing "P")))
    (when (buffer-live-p p-buf) (kill-buffer p-buf)))
  (skg-request-single-root-content-view-from-id "P")
  (let ((substituted (skg-test-wait-for
                      (lambda ()
                        (let ((p-buf (fork-test--buffer-showing "P")))
                          (and p-buf
                               (with-current-buffer p-buf
                                 (string-match-p "(overridesHere N)"
                                                 (buffer-string))))))
                      10)))
    (unless substituted
      (let ((p-buf (fork-test--buffer-showing "P")))
        (test-fail "the clone was not drawn in N's place on reopen:\n%s"
                   (if p-buf (with-current-buffer p-buf (buffer-string)) "<no P buffer>"))))
    (message "✓ on reopen, the clone is drawn in N's place with (overridesHere N)"))

  (message "PASS: Fork integration test successful!")
  (kill-emacs 0))

(run-at-time 40 nil (lambda ()
                      (message "TIMEOUT: fork integration test timed out!")
                      (kill-emacs 1)))

(integration-test-fork)
