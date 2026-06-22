;;; Integration test for the EXPLICIT fork gesture (skg-fork-node).
;;; Open owned container Q (whose content is the owned M):
;;;  - forking a DIRTY buffer is refused (no confirmation appears);
;;;  - skg-fork-node on M -> a fork-confirmation buffer; APPROVE -> the
;;;    clone is created (overrides M) and drawn in M's place on reopen;
;;;  - on a second node M2, skg-fork-node -> DECLINE strips the lingering
;;;    (viewRequests fork) atom and commits nothing.

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

(defun fork-test--goto-headline (id)
  "Move point onto the headline carrying (id ID) in the current buffer.
Fails the test if it is not found."
  (goto-char (point-min))
  (unless (re-search-forward
           (regexp-quote (format "(id %s) (source owned)" id)) nil t)
    (test-fail "could not find %s's headline:\n%s" id (buffer-string))))

(defun integration-test-fork-owned ()
  "Drive the explicit fork: dirty-refusal, approve, and decline."
  (message "Starting explicit-fork integration test...")
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port (setq skg-port (string-to-number test-port))))

  ;; 1. Open the owned container Q; its content is the owned node M.
  (skg-request-single-root-content-view-from-id "Q")
  (let ((q-buf (skg-test-wait-for (lambda () (fork-test--buffer-showing "Q")) 10)))
    (unless q-buf (test-fail "Q's view never appeared"))
    (with-current-buffer q-buf
      (unless (string-match-p "(id M)" (buffer-string))
        (test-fail "Q's view does not show its content M:\n%s" (buffer-string)))

      ;; 2. Forking a DIRTY buffer is refused: no confirmation appears.
      (set-buffer-modified-p t)
      (fork-test--goto-headline "M")
      (skg-fork-node)
      (when (get-buffer "*SKG Fork Confirmation*")
        (test-fail "a dirty buffer must not produce a fork confirmation"))
      (message "✓ forking a dirty buffer was refused")
      (set-buffer-modified-p nil)

      ;; 3. Clean buffer: fork M for real -> fork-confirmation.
      (fork-test--goto-headline "M")
      (skg-fork-node)))

  ;; 4. The confirmation buffer appears and lists M; APPROVE it.
  (let ((confirm-buf (skg-test-wait-for
                      (lambda () (get-buffer "*SKG Fork Confirmation*")) 10)))
    (unless confirm-buf (test-fail "no fork-confirmation buffer appeared"))
    (with-current-buffer confirm-buf
      (unless (string-match-p "(id M)" (buffer-string))
        (test-fail "confirmation buffer does not list M:\n%s" (buffer-string)))
      (message "✓ skg-fork-node produced a fork-confirmation listing M")
      (skg-approve-fork)))

  ;; 5. Reopen Q fresh: override substitution now draws the clone in M's
  ;;    place, carrying (overridesHere M).
  (let ((q-buf (fork-test--buffer-showing "Q")))
    (when (buffer-live-p q-buf) (kill-buffer q-buf)))
  (skg-request-single-root-content-view-from-id "Q")
  (let ((substituted (skg-test-wait-for
                      (lambda ()
                        (let ((q-buf (fork-test--buffer-showing "Q")))
                          (and q-buf
                               (with-current-buffer q-buf
                                 (string-match-p "(overridesHere M)"
                                                 (buffer-string))))))
                      10)))
    (unless substituted
      (let ((q-buf (fork-test--buffer-showing "Q")))
        (test-fail "the clone was not drawn in M's place on reopen:\n%s"
                   (if q-buf (with-current-buffer q-buf (buffer-string)) "<no Q buffer>"))))
    (message "✓ approve: the clone is drawn in M's place with (overridesHere M)"))

  ;; 6. DECLINE on a second node M2 (in Q2): the lingering fork atom is
  ;;    stripped and nothing is committed.
  (skg-request-single-root-content-view-from-id "Q2")
  (let ((q2-buf (skg-test-wait-for (lambda () (fork-test--buffer-showing "Q2")) 10)))
    (unless q2-buf (test-fail "Q2's view never appeared"))
    (with-current-buffer q2-buf
      (fork-test--goto-headline "M2")
      (skg-fork-node)))
  (let ((confirm-buf (skg-test-wait-for
                      (lambda () (get-buffer "*SKG Fork Confirmation*")) 10)))
    (unless confirm-buf (test-fail "no fork-confirmation buffer for M2 appeared"))
    (with-current-buffer confirm-buf
      (skg-decline-fork)))
  ;; The origin buffer's M2 headline no longer carries a fork request.
  (let ((q2-buf (fork-test--buffer-showing "Q2")))
    (unless (buffer-live-p q2-buf) (test-fail "Q2's buffer vanished after decline"))
    (with-current-buffer q2-buf
      (when (string-match-p "(viewRequests" (buffer-string))
        (test-fail "decline must strip the (viewRequests fork) atom:\n%s"
                   (buffer-string))))
    (message "✓ decline stripped the lingering fork atom"))
  ;; Reopen Q2 fresh: no clone overrides M2 (decline committed nothing).
  (let ((q2-buf (fork-test--buffer-showing "Q2")))
    (when (buffer-live-p q2-buf) (kill-buffer q2-buf)))
  (skg-request-single-root-content-view-from-id "Q2")
  (let ((q2-buf (skg-test-wait-for (lambda () (fork-test--buffer-showing "Q2")) 10)))
    (unless q2-buf (test-fail "Q2's view never reappeared"))
    ;; Give the view a beat, then assert no substitution happened.
    (skg-test-wait-for (lambda () nil) 1)
    (with-current-buffer q2-buf
      (when (string-match-p "(overridesHere M2)" (buffer-string))
        (test-fail "decline must not commit a clone overriding M2:\n%s"
                   (buffer-string))))
    (message "✓ decline committed nothing (no clone overrides M2)"))

  (message "PASS: Explicit-fork integration test successful!")
  (kill-emacs 0))

(run-at-time 60 nil (lambda ()
                      (message "TIMEOUT: explicit-fork integration test timed out!")
                      (kill-emacs 1)))

(integration-test-fork-owned)
