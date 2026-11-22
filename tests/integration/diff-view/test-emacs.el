;;; Integration test: the diff view, end to end. See run-test.sh
;;; for the data.
;;;
;;; Phase 1: Open the diff view; its buffers open.
;;; Phase 2: Mark and unmark with each letter and each prefix, and
;;;          see what the server says is processed: 4 of 9 changed
;;;          nodes (m, n, n2 and n3).
;;; Phase 3: Commit the processed (C-c r c): those 4 go in, in alpha
;;;          and beta; the other 5 are left, and r keeps its mark on
;;;          its new child n.
;;; Phase 4: Mark every change processed, through 'C-u C-c m b p' on
;;;          each root of the "all changes" buffer, and see "Commit
;;;          ready"; then unmark t, so that not everything is.
;;; Phase 5: Commit everything (C-c r C), typing "yes" when asked;
;;;          alpha is committed, the foreign skgrepo is not, and the
;;;          buffers keep their roots, which show no change any more.
;;; Phase 6: With d referenced again by hand, rebuild, which
;;;          rerenders every view; open a view of r beside the
;;;          buffers of the diff view, one of which r is a root of,
;;;          and investigate the unknown node d.

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
  (skg-test-use-test-port)
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
                        "acquiredFolder"         ; x's nodeMerge
                        "(gnid n3)"))            ; new content, three deep
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

(defun line-matching (regexp)
  "The text of the first line of the current buffer matching REGEXP."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward regexp nil t)
      (test-fail "No line matches %S:\n%s" regexp (buffer-string)))
    (buffer-substring (line-beginning-position) (line-end-position))))

(defun mark-at (regexp command &optional prefix)
  "Run COMMAND, with PREFIX, on the first headline matching REGEXP,
answering yes if it asks, and wait for the processed summary of its
save."
  (goto-char (point-min))
  (re-search-forward regexp)
  (beginning-of-line)
  (setq test-messages nil)
  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
    (funcall command prefix))
  (unless (skg-test-wait-for
           (lambda ()
             (cl-some (lambda (m) (string-match-p "changed nodes are processed" m))
                      test-messages))
           20)
    (test-fail "No processed summary after %s at %S" command regexp)))

(defun expect-processed (regexp processed-regexp what)
  "Fail unless the headline matching REGEXP matches PROCESSED-REGEXP,
or, when that is nil, carries no '(processed ...)'."
  (let ((line (line-matching regexp)))
    (unless (if processed-regexp
                (string-match-p processed-regexp line)
              (not (string-match-p "(processed" line)))
      (test-fail "After %s: %s" what line))
    (test-pass "%s" what)))

(defun phase-2-mark-and-unmark ()
  (setq integration-test-phase "phase-2")
  (switch-to-buffer (all-changes-buffer))
  ;; Each letter, without a prefix, on the deleted node d.
  (mark-at "ghost (gnid d)" #'skg-mark-node-processed)
  (expect-processed "ghost (gnid d)" "(processed node))" "C-c m n p on d")
  (mark-at "ghost (gnid d)" #'skg-mark-rel-processed)
  (expect-processed "ghost (gnid d)" "(processed node rel))" "C-c m r p on d")
  (mark-at "ghost (gnid d)" #'skg-mark-both-unprocessed)
  (expect-processed "ghost (gnid d)" nil "C-c m b u on d")
  ;; The walks, on the new node n: outward naming relationships, then
  ;; contentward naming nodes; n3, three deep, follows both.
  (mark-at "(gnid n)" #'skg-mark-rel-processed '(16))
  (expect-processed "(gnid n)" "(processed rel))" "C-u C-u C-c m r p on n")
  (expect-processed "(gnid n3)" "(processed rel))" "C-u C-u C-c m r p on n, at n3")
  (mark-at "(gnid n)" #'skg-mark-node-processed '(4))
  (expect-processed "(gnid n)" "(processed node rel))" "C-u C-c m n p on n")
  (expect-processed "(gnid n3)" "(processed node rel))" "C-u C-c m n p on n, at n3")
  ;; m's home skgrepo move.
  (mark-at "(gnid m)" #'skg-mark-homeSkgrepo-move-processed)
  (expect-processed "(homeSkgrepoMove (old alpha) (new beta))" "(processed)"
                    "C-c m h p on m")
  ;; r's membership of n, one of r's two changes (it also lost d).
  (mark-at "^\\*\\* .*(gnid n)" #'skg-mark-rel-processed)
  (expect-processed "^\\*\\* .*(gnid n)" "(processed node rel))" "C-c m r p on r's n")
  (expect-summary "^4 of 9 changed nodes are processed, 1 node changed in foreign skgrepos$"
                  "m, n, n2 and n3 are the processed nodes; r is not"))

(defun expect-summary (regexp what)
  "Fail unless the newest processed summary shown matches REGEXP."
  (let ((summary (cl-find-if
                  (lambda (m) (string-match-p "changed nodes are processed" m))
                  test-messages)))
    (unless (and summary (string-match-p regexp summary))
      (test-fail "%s: the summary is %S" what summary))
    (test-pass "%s: %s" what summary)))

(defvar test-prompts nil
  "Every prompt the last commit showed, newest first.")

(defun commit-from-the-message-buffer (command commit-message)
  "Open the commit-message buffer by COMMAND, send COMMIT-MESSAGE from
it, answering \"yes\" if asked (each prompt goes in `test-prompts'),
and return the line reporting what was committed."
  (setq test-messages nil test-prompts nil)
  (funcall command)
  (with-current-buffer skg--commit-message-buffer-name
    (erase-buffer)
    (insert commit-message "\n")
    (cl-letf (((symbol-function 'read-string)
               (lambda (prompt &rest _) (push prompt test-prompts) "yes")))
      (skg-commit-message-send)
      (unless (skg-test-wait-for
               (lambda ()
                 (cl-some (lambda (m) (string-match-p "^Committed " m))
                          test-messages))
               20)
        (test-fail "No commit reported. Messages: %S" test-messages))))
  (skg-test-wait-for-response 20)
  (cl-find-if (lambda (m) (string-match-p "^Committed " m)) test-messages))

(defun git-modified-p (skgrepo file)
  "Whether FILE of SKGREPO differs from its HEAD, by content. Not by
'git status': the server's write of the git index after a commit loses
git's check of a file changed in the same second as the index was
last written, so a left-out file of unchanged size looks clean there."
  (let ((default-directory (expand-file-name skgrepo test-data-dir)))
    (not (string= (string-trim (shell-command-to-string
                                (concat "git rev-parse -q --verify HEAD:" file)))
                  (if (file-exists-p file)
                      (string-trim (shell-command-to-string
                                    (concat "git hash-object " file)))
                    "")))))

(defun phase-3-commit-the-processed ()
  (setq integration-test-phase "phase-3")
  (let ((line (commit-from-the-message-buffer
               #'skg-stage-and-commit "integration commit of the processed")))
    (unless (string-match-p
             "^Committed 4 nodes, in alpha ([0-9a-f]+), beta ([0-9a-f]+)\\. 5 changed nodes are left\\.$"
             line)
      (test-fail "The commit of the processed reported %S" line))
    (when test-prompts
      (test-fail "The commit of the processed asked %S" test-prompts))
    (test-pass "%s" line))
  (dolist (skgrepo '("owned/alpha" "owned/beta"))
    (unless (string= (git-head-summary skgrepo) "integration commit of the processed")
      (test-fail "%s was not committed: HEAD is %S"
                 skgrepo (git-head-summary skgrepo))))
  (dolist (left '("t.skg" "r.skg" "x.skg"))
    (unless (git-modified-p "owned/alpha" left)
      (test-fail "%s, not processed, was committed" left)))
  (when (git-modified-p "owned/alpha" "n3.skg")
    (test-fail "n3, processed, was not committed"))
  (with-current-buffer (all-changes-buffer)
    (unless (skg-test-wait-for
             (lambda ()
               (not (string-match-p "homeSkgrepoMove" (buffer-string))))
             10)
      (test-fail "The buffer was not rerendered after the commit:\n%s"
                 (buffer-string)))
    (expect-processed "^\\*\\* .*(gnid n)" "(processed rel))"
                      "r keeps its mark on its new child n after the commit"))
  (test-pass "Committed the processed nodes; the rest are left, with their marks"))

(defun phase-4-mark-everything-but-t ()
  (setq integration-test-phase "phase-4")
  (switch-to-buffer (all-changes-buffer))
  ;; Every root, recursively. Each gesture saves, which rerenders the
  ;; buffer, so the i-th root is found afresh.
  (dotimes (i (length (root-headline-positions)))
    (goto-char (nth i (root-headline-positions)))
    (setq test-messages nil)
    (skg-mark-both-processed '(4))
    (unless (skg-test-wait-for
             (lambda ()
               (cl-some (lambda (m) (string-match-p "changed nodes are processed" m))
                        test-messages))
             20)
      (test-fail "No processed summary after the gesture on root %d" i)))
  (let ((summary (cl-find-if
                  (lambda (m) (string-match-p "changed nodes are processed" m))
                  test-messages)))
    (unless (string-match-p "^Commit ready\\." summary)
      (test-fail "Not all processed after marking every root: %s\n%s"
                 summary (buffer-string)))
    (test-pass "%s" summary))
  (mark-at "(gnid t)" #'skg-mark-both-unprocessed)
  (expect-summary "^4 of 5 changed nodes are processed, 1 node changed in foreign skgrepos$" "C-c m b u on t"))

(defun phase-5-commit-everything ()
  (setq integration-test-phase "phase-5")
  (let ((line (commit-from-the-message-buffer
               #'skg-stage-and-commit-everything "integration commit of the rest")))
    (unless (string-match-p "^Committed 5 nodes, in alpha ([0-9a-f]+)\\.$" line)
      (test-fail "The commit of everything reported %S" line))
    (unless (equal test-prompts
                   '("4 of 5 changed nodes are processed, 1 node changed in foreign skgrepos. Type yes to commit anyway: "))
      (test-fail "The commit of everything asked %S" test-prompts))
    (test-pass "%s, after asking" line))
  (unless (string= (git-head-summary "owned/alpha") "integration commit of the rest")
    (test-fail "alpha was not committed: HEAD is %S" (git-head-summary "owned/alpha")))
  (unless (string= (git-head-summary "owned/beta") "integration commit of the processed")
    (test-fail "beta, with nothing left, was committed again: HEAD is %S"
               (git-head-summary "owned/beta")))
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
  (test-pass "Committed the rest in alpha, and the buffers keep their roots, unchanged"))

(defvar test-rerender-all-views-sent nil
  "Whether the rebuild's rerender of every view was asked for.")

(defun test-note-rerender-all-views-sent (&rest _)
  (setq test-rerender-all-views-sent t))

(defun phase-6-investigate-the-deleted-node ()
  (setq integration-test-phase "phase-6")
  ;; Reference d again, by hand, and have the server reread the files.
  (with-temp-file (expand-file-name "owned/alpha/r.skg" test-data-dir)
    (insert "pid: r\ntitle: the root\ncontains:\n- t\n- m\n- x\n- d\n"))
  (setq test-messages nil)
  (advice-add 'skg-request-rerender-all-views :after
              #'test-note-rerender-all-views-sent)
  (skg-rebuild-session-lifetime-stores)
  (unless (skg-test-wait-for
           (lambda ()
             (cl-some (lambda (m) (string-match-p "rebuilt successfully" m))
                      test-messages))
           30)
    (test-fail "Timeout waiting for the rebuild"))
  (unless (skg-test-wait-for
           (lambda () (and test-rerender-all-views-sent
                           (not skg--stream-in-progress)))
           30)
    (test-fail "Timeout waiting for the rerender of every view after the rebuild"))
  (advice-remove 'skg-request-rerender-all-views
                 #'test-note-rerender-all-views-sent)
  ;; The buffers of the diff view survive the rebuild, and r is a root
  ;; of one. A view request for r does not switch to it: a content
  ;; view of r opens beside them.
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
    (unless (diff-view-buffers)
      (test-fail "The buffers of the diff view closed when the view of r opened"))
    (test-pass "A content view of r opened beside the buffers of the diff view")
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
  (phase-2-mark-and-unmark)
  (phase-3-commit-the-processed)
  (phase-4-mark-everything-but-t)
  (phase-5-commit-everything)
  (phase-6-investigate-the-deleted-node)
  (message "")
  (test-pass "All diff-view tests passed!")
  (kill-emacs 0))

(progn
  (skg-test-arm-timeout 180 'integration-test-phase)
  (run-all-tests))
