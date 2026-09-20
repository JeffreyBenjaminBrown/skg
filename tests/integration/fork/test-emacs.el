;;; Integration test for a structural fork gesture in Emacs.
;;; Open foreign F as the view root; insert a new content node N and
;;; move F's existing content O beneath N.  Approve the fork and verify
;;; the resulting F/K/N/O graph, that the same buffer immediately has K
;;; as its root, and the absence of an Emacs-visible save failure.

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

(defun fork-test--root-buffer (id)
  "Return a live skg view buffer whose first headline is ID."
  (seq-find
   (lambda (b)
     (and (buffer-live-p b)
          (with-current-buffer b
            (and (boundp 'skg-view-uri) skg-view-uri
                 (save-excursion
                   (goto-char (point-min))
                   (let ((line (buffer-substring-no-properties
                                (line-beginning-position)
                                (line-end-position))))
                     (string-match-p
                      (regexp-quote (format "(id %s)" id)) line)))))))
   (buffer-list)))

(defun fork-test--line-containing (needle)
  "Return the current buffer's whole line containing NEEDLE, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward needle nil t)
      (buffer-substring-no-properties
       (line-beginning-position) (line-end-position)))))

(defun integration-test-fork ()
  "Drive the structural edit -> confirm -> approve -> fork flow."
  (message "Starting structural fork integration test...")
  (let ((test-port (getenv "SKG_TEST_PORT"))
        (k-id nil)
        (n-id nil)
        (fork-buffer nil))
    (when test-port (setq skg-port (string-to-number test-port)))

    ;; 1. Open foreign F itself, with its existing child O visible.
    (skg-request-single-root-content-view-from-id "F")
    (setq fork-buffer
           (skg-test-wait-for
            (lambda () (fork-test--root-buffer "F")) 10))
    (unless fork-buffer (test-fail "F's root view never appeared"))
    (with-current-buffer fork-buffer
      (unless (string-match-p "(id O)" (buffer-string))
        (test-fail "F's view does not expose its existing child O:\n%s"
                   (buffer-string))))

    ;; 2. Insert bare N under F, and move the existing O headline from
    ;; F to N.  The bare node must inherit the eventual clone source.
    (with-current-buffer fork-buffer
      (goto-char (point-min))
      (unless (re-search-forward "^\\*\\* .*?(id O).*$" nil t)
        (test-fail "could not find O directly under F:\n%s"
                   (buffer-string)))
      (let ((o-line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
        (delete-region (line-beginning-position)
                       (min (point-max) (1+ (line-end-position))))
        (goto-char (point-min))
        (forward-line 1)
        (insert "** N-new\n"
                "*** "
                (replace-regexp-in-string "^\\*+ " "" o-line)
                "\n"))
      (goto-char (point-min))
      (skg-request-save-buffer))

    ;; 3. The confirmation names F; choose the owned source and approve.
    (let ((confirm-buf
           (skg-test-wait-for
            (lambda () (get-buffer "*SKG Fork Confirmation*")) 10)))
      (unless confirm-buf (test-fail "no fork-confirmation buffer appeared"))
      (with-current-buffer confirm-buf
        (unless (string-match-p "(id F)" (buffer-string))
          (test-fail "confirmation buffer does not list F:\n%s"
                     (buffer-string)))
        (goto-char (point-min))
        (unless (re-search-forward "^\\* (skg (node (source " nil t)
          (test-fail "could not find clone-to-be source headline:\n%s"
                     (buffer-string)))
        (beginning-of-line)
        (skg--change-source-at-point "owned")
        (skg-approve-fork)))

    ;; 4. Approval must finish cleanly.  In particular, the checked
    ;; telescope writer must not reject N with a home/first-section mismatch.
    (let ((result-buf
           (skg-test-wait-for
            (lambda ()
              (let ((buf (get-buffer "*SKG Fork Result*")))
                (and buf
                     (with-current-buffer buf
                       (and (string-match-p "save \\(successful\\|failed\\)"
                                            (buffer-string))
                            buf)))))
            10)))
      (unless result-buf
        (test-fail "fork approval produced no terminal result"))
      (with-current-buffer result-buf
        (unless (string-match-p "Fork confirmed; save successful\\."
                                (buffer-string))
          (test-fail "Emacs received a failed structural-fork save:\n%s"
                     (buffer-string))))
      (let ((error-buf
             (get-buffer "*SKG Save Errors - Inconsistencies Found*")))
        (when error-buf
          (test-fail "Emacs displayed a save-error buffer:\n%s"
                     (with-current-buffer error-buf (buffer-string))))))

    ;; 5. The exact buffer used for the gesture immediately replaces root F
    ;; with K.  K contains N, and N contains O.
    (unless
        (skg-test-wait-for
         (lambda ()
           (and (buffer-live-p fork-buffer)
                (with-current-buffer fork-buffer
                  (save-excursion
                    (goto-char (point-min))
                    (and (string-match-p "(overridesHere F)"
                                         (buffer-substring-no-properties
                                          (line-beginning-position)
                                          (line-end-position)))
                         (string-match-p "N-new" (buffer-string)))))))
         10)
      (test-fail "F's original buffer did not replace root F with K:\n%s"
                 (if (buffer-live-p fork-buffer)
                     (with-current-buffer fork-buffer (buffer-string))
                   "<buffer was killed>")))
    (with-current-buffer fork-buffer
      (goto-char (point-min))
      (let ((k-line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (unless (and k-line
                       (string-match "(id \\([^ )]+\\))" k-line)
                       (string-match-p "(overridesHere F)" k-line)
                       (string-match-p "(subscribes (out 1))" k-line)
                       (string-match-p "(overrides (out 1))" k-line))
            (test-fail "K lacks its expected F relationships:\n%s"
                       (buffer-string)))
        (setq k-id (match-string 1 k-line)))
      (unless (re-search-forward
               "^\\*\\* .*?(id \\([^ )]+\\)).* N-new$" nil t)
        (test-fail "K does not contain the new N:\n%s" (buffer-string)))
      (setq n-id (match-string 1))
      (unless (re-search-forward "^\\*\\*\\* .*?(id O).* O-original$"
                                 nil t)
        (test-fail "N does not contain O:\n%s" (buffer-string)))
      (goto-char (point-min))
      (when (re-search-forward "^\\*\\* .*?(id O).* O-original$" nil t)
        (test-fail "K still contains O directly:\n%s" (buffer-string))))

    ;; 6. Fresh graph-derived views verify all three nodes independently:
    ;; F still contains O and now has one incoming subscriber/overrider;
    ;; K subscribes to/overrides F and contains N; N contains O.
    (skg-request-single-root-content-view-from-id "F" nil t)
    (let ((f-buf
           (skg-test-wait-for (lambda () (fork-test--root-buffer "F")) 10)))
      (unless f-buf (test-fail "fresh F view never appeared"))
      (with-current-buffer f-buf
        (let ((f-line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
          (unless (and (string-match-p "(subscribes (in 1))" f-line)
                       (string-match-p "(overrides (in 1))" f-line)
                       (string-match-p "(contains (out 1))" f-line))
            (test-fail "F's post-fork graph relationships are wrong:\n%s"
                       (buffer-string))))))

    ;; The original buffer is now K's registered root view; reuse it rather
    ;; than issuing a request which would merely switch back to the same view.
    (let ((k-buf fork-buffer))
      (unless (and (buffer-live-p k-buf)
                   (eq k-buf (fork-test--root-buffer k-id)))
        (test-fail "the fork buffer is not registered as K's root view"))
      (with-current-buffer k-buf
        (unless (and (string-match-p "F-original" (buffer-string))
                     (string-match-p "^\\*\\* .* N-new$" (buffer-string))
                     (not (string-match-p "^\\*\\* .*?(id O).* O-original$"
                                          (buffer-string))))
          (test-fail "K is not F-with-N-in-place-of-O:\n%s"
                     (buffer-string)))))

    (skg-request-single-root-content-view-from-id n-id)
    (let ((n-buf
           (skg-test-wait-for (lambda () (fork-test--root-buffer n-id)) 10)))
      (unless n-buf (test-fail "fresh N view never appeared"))
      (with-current-buffer n-buf
        (unless (string-match-p "^\\*\\* .*?(id O).* O-original$"
                                (buffer-string))
          (test-fail "N's graph view does not contain O:\n%s"
                     (buffer-string)))))

    (message "PASS: Structural fork integration test successful!")
    (kill-emacs 0)))

(run-at-time 60 nil (lambda ()
                      (message "TIMEOUT: structural fork integration test timed out!")
                      (kill-emacs 1)))

(integration-test-fork)
