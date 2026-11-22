;;; Integration test for the motivating chain-of-forks case.
;;; Fork a foreign N into a public clone C (implicit fork), then run
;;; skg-fork-node on the shown substitute C and rotate its clone to a
;;; private skgrepo, forming the chain D forks C forks N. Viewing
;;; N's container from its home, public, then shows C; switched to the
;;; perspective of private, it shows the chain end D, marked (inPlaceOf
;;; N), and the save accepts that chain-end carrier.

(load-file "../../../elisp/skg-init.el")
(load-file "../test-wait.el")

(defun test-fail (message &rest args)
  "Report test failure and exit."
  (apply #'message (concat "✗ FAIL: " message) args)
  (kill-emacs 1))

(defun chain-test--buffer-showing (gnid)
  "Return a live skg view buffer whose text mentions (gnid ID)."
  (seq-find
   (lambda (b)
     (and (buffer-live-p b)
          (with-current-buffer b
            (and (boundp 'skg-view-id) skg-view-id
                 (string-match-p (regexp-quote (format "(gnid %s)" gnid))
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
  (message "Starting chain-of-forks integration test...")
  (skg-test-use-test-port)

  ;; 1. Open P (public); its content is the foreign N.
  (let ((p-buf (chain-test--reopen-P)))
    (with-current-buffer p-buf
      (unless (string-match-p "(gnid N)" (buffer-string))
        (test-fail "P's view does not show foreign N:\n%s" (buffer-string)))
      ;; 2. Implicit fork: make N editable and edit its title.
      (goto-char (point-min))
      (unless (re-search-forward "^.*(gnid N) (skgRepos (nodeRepo foreign)).*$" nil t)
        (test-fail "could not find N's headline:\n%s" (buffer-string)))
      (let* ((line (match-string 0))
             (edited (replace-regexp-in-string
                      " writeProtected" ""
                      (replace-regexp-in-string "N-original" "N-edited" line))))
        (replace-match edited t t))
      (skg-request-save-buffer)))

  ;; 3. Approve the foreign fork -> public clone C forks N.
  (let ((confirm-buf (skg-test-wait-for
                      (lambda () (get-buffer "*SKG Fork Confirmation*")) 10)))
    (unless confirm-buf (test-fail "no fork-confirmation for N appeared"))
    (with-current-buffer confirm-buf
      ;; Pick the clone's skgrepo (required), then approve.
      (goto-char (point-min))
      (re-search-forward "^\\* (skg node (skgRepos (nodeRepo ")
      (beginning-of-line)
      (skg--change-repo-at-point "public")
      (skg-approve-fork)))
  (message "✓ foreign N forked into a public clone")

  ;; 4. Reopen P: the public clone C is shown in N's place.
  (let ((p-buf (chain-test--reopen-P)))
    (let ((shown (skg-test-wait-for
                  (lambda ()
                    (with-current-buffer p-buf
                      (string-match-p "(inPlaceOf N)" (buffer-string))))
                  10)))
      (unless shown
        (test-fail "the public clone was not shown in N's place:\n%s"
                   (with-current-buffer p-buf (buffer-string)))))
    ;; 5. Explicitly fork the shown substitute C.
    (with-current-buffer p-buf
      (goto-char (point-min))
      (unless (re-search-forward "(inPlaceOf N)" nil t)
        (test-fail "could not find the shown substitute:\n%s" (buffer-string)))
      (skg-fork-node)))

  ;; 6. In the confirmation buffer, rotate the clone-to-be's skgrepo to
  ;;    private, then approve -> private D forks C.
  (let ((confirm-buf (skg-test-wait-for
                      (lambda () (get-buffer "*SKG Fork Confirmation*")) 10)))
    (unless confirm-buf (test-fail "no fork-confirmation for the clone appeared"))
    (with-current-buffer confirm-buf
      (goto-char (point-min))
      (unless (re-search-forward "^\\* (skg node (skgRepos (nodeRepo " nil t)
        (test-fail "could not find the clone-to-be headline:\n%s" (buffer-string)))
      (beginning-of-line)
      (skg--change-repo-at-point "private")
      (skg-approve-fork)))
  (message "✓ the public clone was forked into a private clone (skgRepos (nodeRepo rotated))")

  ;; 7. Reopen P (all skgrepos unrestricted): drawn from P's home,
  ;;    public, it shows C in N's place. Cycled to the perspective of
  ;;    private (passing foreign's, which shows N itself), the chain end
  ;;    D -- a private-repo node -- is shown in N's place, still marked
  ;;    (inPlaceOf N).
  (let ((p-buf (chain-test--reopen-P)))
    (with-current-buffer p-buf
      (unless (skg-test-wait-for
               (lambda () (string-match-p "(nodeRepo public)) (inPlaceOf N)"
                                          (buffer-string)))
               10)
        (test-fail "from P's home, public, C was not shown for N:\n%s"
                   (buffer-string)))
      (dotimes (_ 3)
        (unless (equal skg-perspective "skgrepo private")
          (skg-cycle-perspective)
          (skg-test-wait-for
           (lambda () (and skg-perspective (not skg--stream-in-progress)))
           10))))
    (unless (with-current-buffer p-buf (equal skg-perspective "skgrepo private"))
      (test-fail "the view never reached the perspective of private"))
    (let ((end-shown
           (skg-test-wait-for
            (lambda ()
              (with-current-buffer p-buf
                (save-excursion
                  (goto-char (point-min))
                  (catch 'found
                    (while (re-search-forward "(inPlaceOf N)" nil t)
                      (let ((line (buffer-substring-no-properties
                                   (line-beginning-position) (line-end-position))))
                        (when (string-match-p "(skgRepos (nodeRepo private))" line)
                          (throw 'found t))))
                    nil))))
            10)))
      (unless end-shown
        (test-fail "the chain end D (a private node) was not shown for N:\n%s"
                   (with-current-buffer p-buf (buffer-string))))
      (message "✓ the chain end D (private) is shown in N's place (inPlaceOf N)")
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

  (message "PASS: Chain-of-forks integration test successful!")
  (kill-emacs 0))

(run-at-time 90 nil (lambda ()
                      (message "TIMEOUT: chain-of-forks integration test timed out!")
                      (kill-emacs 1)))

(integration-test-fork-chain)
