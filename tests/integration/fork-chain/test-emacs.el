;;; Integration test for the MOTIVATING override-chain case.
;;; Fork a foreign N into a public clone C (implicit fork), then run
;;; skg-fork-node on the DRAWN SUBSTITUTE C and rotate its clone to a
;;; PRIVATE source, forming the chain D overrides C overrides N. Viewing
;;; N's container then draws the chain end D, marked (overridesHere N),
;;; and the save accepts that chain-end carrier.

(load-file "../../../elisp/skg-init.el")
(load-file "../test-wait.el")

(defun test-fail (message &rest args)
  "Report test failure and exit."
  (apply #'message (concat "✗ FAIL: " message) args)
  (kill-emacs 1))

(defun chain-test--buffer-showing (id)
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

(defun chain-test--reopen-P ()
  "Kill any open P buffer and request a fresh single-root view of P."
  (let ((p-buf (chain-test--buffer-showing "P")))
    (when (buffer-live-p p-buf) (kill-buffer p-buf)))
  (skg-request-single-root-content-view-from-id "P")
  (let ((p-buf (skg-test-wait-for (lambda () (chain-test--buffer-showing "P")) 10)))
    (unless p-buf (test-fail "P's view never appeared"))
    p-buf))

(defun integration-test-fork-chain ()
  "Drive the foreign-fork -> fork-the-substitute -> chain flow."
  (message "Starting override-chain integration test...")
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port (setq skg-port (string-to-number test-port))))

  ;; 1. Open P (public); its content is the foreign N.
  (let ((p-buf (chain-test--reopen-P)))
    (with-current-buffer p-buf
      (unless (string-match-p "(id N)" (buffer-string))
        (test-fail "P's view does not show foreign N:\n%s" (buffer-string)))
      ;; 2. Implicit fork: make N definitive and edit its title.
      (goto-char (point-min))
      (unless (re-search-forward "^.*(id N) (source foreign).*$" nil t)
        (test-fail "could not find N's headline:\n%s" (buffer-string)))
      (let* ((line (match-string 0))
             (edited (replace-regexp-in-string
                      " indef" ""
                      (replace-regexp-in-string "N-original" "N-edited" line))))
        (replace-match edited t t))
      (skg-request-save-buffer)))

  ;; 3. Approve the foreign fork -> public clone C overrides N.
  (let ((confirm-buf (skg-test-wait-for
                      (lambda () (get-buffer "*SKG Fork Confirmation*")) 10)))
    (unless confirm-buf (test-fail "no fork-confirmation for N appeared"))
    (with-current-buffer confirm-buf
      ;; Pick the clone's source (required), then approve.
      (goto-char (point-min))
      (re-search-forward "^\\* (skg (node (source ")
      (beginning-of-line)
      (skg--change-source-at-point "public")
      (skg-approve-fork)))
  (message "✓ foreign N forked into a public clone")

  ;; 4. Reopen P: the public clone C is drawn in N's place.
  (let ((p-buf (chain-test--reopen-P)))
    (let ((drawn (skg-test-wait-for
                  (lambda ()
                    (with-current-buffer p-buf
                      (string-match-p "(overridesHere N)" (buffer-string))))
                  10)))
      (unless drawn
        (test-fail "the public clone was not drawn in N's place:\n%s"
                   (with-current-buffer p-buf (buffer-string)))))
    ;; 5. Explicitly fork the DRAWN SUBSTITUTE C.
    (with-current-buffer p-buf
      (goto-char (point-min))
      (unless (re-search-forward "(overridesHere N)" nil t)
        (test-fail "could not find the drawn substitute:\n%s" (buffer-string)))
      (skg-fork-node)))

  ;; 6. In the confirmation buffer, rotate the clone-to-be's source to
  ;;    PRIVATE, then approve -> private D overrides C.
  (let ((confirm-buf (skg-test-wait-for
                      (lambda () (get-buffer "*SKG Fork Confirmation*")) 10)))
    (unless confirm-buf (test-fail "no fork-confirmation for the clone appeared"))
    (with-current-buffer confirm-buf
      (goto-char (point-min))
      (unless (re-search-forward "^\\* (skg (node (source " nil t)
        (test-fail "could not find the clone-to-be headline:\n%s" (buffer-string)))
      (beginning-of-line)
      (skg--change-source-at-point "private")
      (skg-approve-fork)))
  (message "✓ the public clone was forked into a private clone (source rotated)")

  ;; 7. Reopen P (all sources active): the chain end D -- a PRIVATE-source
  ;;    node -- is now drawn in N's place, still marked (overridesHere N).
  (let ((p-buf (chain-test--reopen-P)))
    (let ((end-drawn
           (skg-test-wait-for
            (lambda ()
              (with-current-buffer p-buf
                (save-excursion
                  (goto-char (point-min))
                  (catch 'found
                    (while (re-search-forward "(overridesHere N)" nil t)
                      (let ((line (buffer-substring-no-properties
                                   (line-beginning-position) (line-end-position))))
                        (when (string-match-p "(source private)" line)
                          (throw 'found t))))
                    nil))))
            10)))
      (unless end-drawn
        (test-fail "the chain end D (a private node) was not drawn for N:\n%s"
                   (with-current-buffer p-buf (buffer-string))))
      (message "✓ the chain end D (private) is drawn in N's place (overridesHere N)")
      ;; 8. Saving the chain-end view succeeds (tamper check accepts the
      ;;    chain-end carrier).
      (with-current-buffer p-buf
        (skg-request-save-buffer))
      (let ((saved (skg-test-wait-for
                    (lambda ()
                      (with-current-buffer p-buf
                        (not (buffer-modified-p))))
                    10)))
        (unless saved
          (test-fail "saving the chain-end view did not complete"))
        (message "✓ saving the chain-end view succeeded"))))

  (message "PASS: Override-chain integration test successful!")
  (kill-emacs 0))

(run-at-time 90 nil (lambda ()
                      (message "TIMEOUT: override-chain integration test timed out!")
                      (kill-emacs 1)))

(integration-test-fork-chain)
