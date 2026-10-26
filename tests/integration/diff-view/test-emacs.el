;;; Integration test: the diff view, end to end. See run-test.sh
;;; for the data.
;;;
;;; Phase 1: Open the diff view; its buffers open.
;;; Phase 2: Mark every change commit-ready, through the recursive
;;;          gesture on each root of the "all changes" buffer, and see
;;;          "Commit ready".
;;; Phase 3: Stage and commit; every owned skgrepo is committed, the
;;;          foreign one is not, and the buffers keep their roots,
;;;          which show no change any more.
;;; Phase 4: With d referenced again by hand, open a view of r and
;;;          investigate the unknown node d.

(load-file "../../../elisp/skg-init.el")
(load-file "../test-wait.el")

(defvar integration-test-phase "starting")

(defvar test-messages nil
  "Every message shown, newest first.")

(advice-add 'message :around
            (lambda (original format-string &rest args)
              (when format-string
                (push (apply #'format format-string args) test-messages))
              (apply original format-string args)))

(defun test-fail (message &rest args)
  (apply #'message (concat "✗ FAIL: " message) args)
  (kill-emacs 1))

(defun test-pass (message &rest args)
  (apply #'message (concat "✓ PASS: " message) args))

(defconst test-data-dir
  (expand-file-name "data/" (file-name-directory load-file-name)))

(defun all-changes-buffer ()
  (cl-find-if
   (lambda (b) (string-match-p "diff-view: all changes" (buffer-name b)))
   (buffer-list)))

(defun diff-view-buffers ()
  (cl-remove-if-not
   (lambda (b) (string-match-p "diff-view: " (buffer-name b)))
   (buffer-list)))

(defun git-head-summary (skgrepo)
  "The summary line of SKGREPO's HEAD commit."
  (let ((default-directory (expand-file-name skgrepo test-data-dir)))
    (string-trim (shell-command-to-string "git log -1 --format=%s"))))

(defun phase-1-open-the-diff-view ()
  (setq integration-test-phase "phase-1")
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))))
  (setq skg-config-dir test-data-dir)
  (skg-diffView-open)
  (unless (skg-test-wait-for
           (lambda ()
             (let ((buf (all-changes-buffer)))
               (and buf
                    (with-current-buffer buf
                      (string-match-p "(gnid r)" (buffer-string)))
                    (skg-test-wait-for-response 1)))))
    (test-fail "Timeout waiting for the \"all changes\" buffer"))
  ;; The sequence opens each nonempty buffer in turn.
  (skg-test-wait-for
   (lambda () (>= (length (diff-view-buffers)) 3)) 10)
  (let ((names (sort (mapcar #'buffer-name (diff-view-buffers))
                     #'string<))
        (all-changes (with-current-buffer (all-changes-buffer)
                       (buffer-string))))
    (unless (and (cl-some (lambda (n) (string-match-p "merges" n)) names)
                 (cl-some (lambda (n) (string-match-p "skgrepo changes" n)) names))
      (test-fail "Expected the merges and skgrepo-changes buffers, got %S" names))
    (dolist (expected '("(oldText title)"        ; t's text edit
                        "(homeSkgrepoMove (old alpha) (new beta))" ; m's move
                        "ghost (gnid d)"        ; d's deletion
                        "acquiredFolder"))       ; x's nodeMerge
      (unless (string-match-p (regexp-quote expected) all-changes)
        (test-fail "The \"all changes\" buffer lacks %s:\n%s"
                   expected all-changes)))
    (test-pass "The diff view opened: %S" names)))

(defun root-headline-positions ()
  "The positions of the current buffer's root headlines."
  (save-excursion
    (goto-char (point-min))
    (let ((positions nil))
      (while (re-search-forward "^\\* " nil t)
        (push (line-beginning-position) positions))
      (nreverse positions))))

(defun phase-2-mark-everything ()
  (setq integration-test-phase "phase-2")
  (let ((buf (all-changes-buffer))
        (root-count nil))
    (switch-to-buffer buf)
    (setq root-count (length (root-headline-positions)))
    (dotimes (i root-count)
      ;; Each gesture saves, which rerenders the buffer, so find the
      ;; i-th root afresh. A root an earlier gesture already readied
      ;; is skipped: the gesture toggles, so it would unready it.
      (goto-char (nth i (root-headline-positions)))
      (unless (string-match-p "(ready"
                              (buffer-substring (line-beginning-position)
                                                (line-end-position)))
        (setq test-messages nil)
        (skg-cycle-commit-ready t)
        (unless (skg-test-wait-for
                 (lambda ()
                   (cl-some (lambda (m) (string-match-p "changed files are ready" m))
                            test-messages))
                 20)
          (test-fail "No readiness summary after the gesture on root %d" i))))
    (let ((summary (cl-find-if
                    (lambda (m) (string-match-p "changed files are ready" m))
                    test-messages)))
      (unless (string-match-p "^Commit ready\\." summary)
        (test-fail "Not commit-ready after marking every root: %s\n%s"
                   summary (buffer-string)))
      (test-pass "%s" summary))))

(defun phase-3-stage-and-commit ()
  (setq integration-test-phase "phase-3")
  (setq test-messages nil)
  (cl-letf (((symbol-function 'read-string)
             (lambda (&rest _) "integration commit")))
    (skg-stage-and-commit))
  (unless (skg-test-wait-for
           (lambda ()
             (cl-some (lambda (m) (string-match-p "^Committed in" m))
                      test-messages))
           20)
    (test-fail "No commit reported. Messages: %S" test-messages))
  (skg-test-wait-for-response 20)
  (dolist (skgrepo '("owned/alpha" "owned/beta"))
    (unless (string= (git-head-summary skgrepo) "integration commit")
      (test-fail "%s was not committed: HEAD is %S"
                 skgrepo (git-head-summary skgrepo))))
  (unless (string= (git-head-summary "foreign/gamma") "Initial commit")
    (test-fail "The foreign skgrepo was committed"))
  (unless (skg-test-wait-for
           (lambda ()
             (cl-every (lambda (b)
                         (let ((text (with-current-buffer b (buffer-string))))
                           (and (string-match-p "^\\* " text)
                                (not (string-match-p
                                      "(diff \\|changed)\\|changesetMarker\\|oldText\\|homeSkgrepoMove\\|acquiredFolder"
                                      text)))))
                       (diff-view-buffers)))
           10)
    (test-fail "The buffers of the diff view did not keep their roots without their changes: %S"
               (mapcar (lambda (b) (cons (buffer-name b)
                                         (with-current-buffer b (buffer-string))))
                       (diff-view-buffers))))
  (test-pass "Committed alpha and beta, and the buffers keep their roots, unchanged"))

(defun phase-4-investigate-the-deleted-node ()
  (setq integration-test-phase "phase-4")
  ;; Reference d again, by hand, and have the server reread the files.
  (with-temp-file (expand-file-name "owned/alpha/r.skg" test-data-dir)
    (insert "pid: r\ntitle: the root\ncontains:\n- t\n- m\n- x\n- d\n"))
  (setq test-messages nil)
  (skg-rebuild-session-lifetime-stores)
  (unless (skg-test-wait-for
           (lambda ()
             (cl-some (lambda (m) (string-match-p "views are now invalid" m))
                      test-messages))
           30)
    (test-fail "Timeout waiting for the rebuild"))
  (skg-request-single-root-content-view-from-id "r")
  (let ((view (skg-test-wait-for
               (lambda ()
                 (cl-find-if
                  (lambda (b)
                    (with-current-buffer b
                      (and (derived-mode-p 'skg-content-view-mode)
                           (not (string-match-p "diff-view: " (buffer-name b)))
                           (string-match-p "unknown (gnid d)" (buffer-string)))))
                  (buffer-list))))))
    (unless view (test-fail "Timeout waiting for a view of r showing d as unknown"))
    (switch-to-buffer view)
    (goto-char (point-min))
    (re-search-forward "unknown (gnid d)")
    (beginning-of-line)
    (skg-investigate-unknown-node))
  (let ((answer (skg-test-wait-for-buffer "*skg unknown node*" 20)))
    (unless answer (test-fail "No investigation buffer"))
    (skg-test-wait-for
     (lambda () (with-current-buffer answer
                  (string-match-p "vanished at commit" (buffer-string)))))
    (let ((text (with-current-buffer answer (buffer-string))))
      (dolist (expected '("* d"
                          "title when last present: doomed"
                          "vanished at commit"
                          "integration commit"
                          "r (via contains)"))
        (unless (string-match-p (regexp-quote expected) text)
          (test-fail "The investigation lacks %S:\n%s" expected text)))
      (test-pass "d's history:\n%s" text))))

(defun run-all-tests ()
  (message "=== SKG Diff View Integration Test ===")
  (phase-1-open-the-diff-view)
  (phase-2-mark-everything)
  (phase-3-stage-and-commit)
  (phase-4-investigate-the-deleted-node)
  (message "")
  (test-pass "All diff-view tests passed!")
  (kill-emacs 0))

(progn
  (run-at-time
   120 nil
   (lambda ()
     (message "TIMEOUT: Integration test timed out!")
     (message "Last phase: %s" integration-test-phase)
     (kill-emacs 1)))
  (run-all-tests))
