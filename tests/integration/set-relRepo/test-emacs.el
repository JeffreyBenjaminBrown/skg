;;; Integration test for the relRepo gesture, Emacs client.
;;; Node r (home public) contains k (home private), so the relationship's
;;; default relRepo is private: the prompt offers private, secret and the
;;; no-override choice, which it can know only if the info request names
;;; the recorder. Choosing secret and saving records it.

(load-file "../../../elisp/skg-init.el")
(load-file "../test-wait.el")

(defvar integration-test-phase "starting")

(defun test-fail (message &rest args)
  (apply #'message (concat "FAIL: " message) args)
  (kill-emacs 1))

(defun test-pass (message &rest args)
  (apply #'message (concat "PASS: " message) args))

(defun test-line-of-k ()
  "The text of the line whose metadata names k."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "(gnid k)" nil t)
      (buffer-substring-no-properties
       (line-beginning-position) (line-end-position)))))

(defun integration-test-main ()
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port (setq skg-port (string-to-number test-port))))
  (let ((config-file (getenv "SKG_CONFIG_FILE")))
    (when config-file
      (setq skg-config-dir
            (file-name-directory (expand-file-name config-file)))))
  (setq integration-test-phase "open the view")
  (skg-request-single-root-content-view-from-id "r")
  (let ((view (skg-test-wait-for
               (lambda ()
                 (cl-find-if
                  (lambda (b)
                    (with-current-buffer b
                      (and (derived-mode-p 'skg-content-view-mode)
                           (save-excursion
                             (goto-char (point-min))
                             (search-forward "(gnid k)" nil t)))))
                  (buffer-list))))))
    (unless view (test-fail "the view of r did not open"))
    (switch-to-buffer view)
    (setq integration-test-phase "set the relRepo")
    (goto-char (point-min))
    (search-forward "(gnid k)")
    (beginning-of-line)
    (let (seen-choices)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _)
                   (setq seen-choices choices)
                   "secret")))
        (skg-set-relRepo)
        (unless (skg-test-wait-for
                 (lambda () (string-match-p "(relRepo secret)"
                                            (test-line-of-k))))
          (test-fail "no relRepo request was written: %s"
                     (test-line-of-k))))
      (unless (equal seen-choices
                     (list "private" "secret" skg--relRepo-no-override))
        (test-fail "the prompt offered %S" seen-choices))
      (test-pass "the prompt offered the skgrepos from the default on"))
    (setq integration-test-phase "save")
    (skg-request-save-buffer)
    (skg-test-wait-for-response)
    (unless (skg-test-wait-for
             (lambda ()
               (let ((line (test-line-of-k)))
                 (and line
                      (string-match-p
                       "(skgRepos (nodeRepo private) (relRepo secret))" line)
                      (not (string-match-p "editRequest" line))))))
      (test-fail "the save did not record the relRepo: %s"
                 (test-line-of-k)))
    (test-pass "the save recorded the relRepo: %s" (test-line-of-k)))
  (kill-emacs 0))

(progn
  (run-at-time 25 nil
               (lambda ()
                 (message "TIMEOUT at phase: %s" integration-test-phase)
                 (kill-emacs 1)))
  (integration-test-main))
