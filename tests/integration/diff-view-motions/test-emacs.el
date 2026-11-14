;;; Integration test: the diff view's motions between changes. See
;;; run-test.sh for the data.
;;;
;;; Phase 1: Open the diff view; a's containers, mentioners and
;;;          subscribers folders each hold a write-protected member
;;;          with change facts.
;;; Phase 2: C-c r n c visits every headline with change facts but
;;;          those three members, and C-c r n C visits them too. Each
;;;          member's node stands editable elsewhere, with a change
;;;          the plain motion visits there (Jeff's requirement).
;;; Phase 3: C-c s e on y inside the mentioners folder and save: y's
;;;          old text now stands only inside that folder, and the
;;;          plain motion visits it and y there.

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

(defun visited-lines (command)
  "The lines COMMAND visits, run from the top of the current buffer
until it stops moving. Line 1 counts when it carries change facts and
the motion would not skip it."
  (goto-char (point-min))
  (let ((visited (and (skg--diffView-change-start-on-line
                       nil (eq command #'skg-diffView-goto-next-change-anywhere))
                      (list 1)))
        (moved t))
    (while moved
      (let ((before (point)))
        (funcall command)
        (setq moved (/= before (point)))
        (when moved (push (line-number-at-pos) visited))))
    (nreverse visited)))

(defun line-text (line)
  (save-excursion
    (goto-char (point-min)) (forward-line (1- line))
    (buffer-substring (line-beginning-position) (line-end-position))))

(defun check-skipped-changes-are-drawn-editable (visited all)
  "Fail unless each line of ALL not in VISITED is a ☮ member of a
folder of other files whose node stands editable elsewhere in the
buffer, where VISITED holds what it names: for 'node', a line there or
under it; for 'rel', a line under it standing for the folder's
viewparent. The skipped lines."
  (let ((skipped (seq-difference all visited)))
    (dolist (line skipped)
      (goto-char (point-min)) (forward-line (1- line))
      (let* ((sexp (skg--metadata-sexp-at-point-or-nil))
             (gnid (format "%s" (cadr (assq 'gnid (cdr sexp)))))
             (objects (cdr (skg--processed-leaves sexp)))
             (partner (save-excursion
                        (org-up-heading-safe) (org-up-heading-safe)
                        (format "%s" (cadr (assq 'gnid (cdr (skg--metadata-sexp-at-point-or-nil))))))))
        (unless (skg--diffView-change-of-other-files-p sexp)
          (test-fail "Skipped line %d is no ☮ member of an (other files) folder: %s"
                     line (line-text line)))
        (goto-char (point-min))
        (unless
            (cl-loop
             while (re-search-forward
                    (format "^\\*+ (skg .*(gnid %s)" (regexp-quote gnid)) nil t)
             thereis
             (save-excursion
               (beginning-of-line)
               (let ((occurrence (skg--metadata-sexp-at-point-or-nil))
                     (start (line-number-at-pos))
                     (end (save-excursion (org-end-of-subtree t t)
                                          (line-number-at-pos))))
                 (and (skg--occurrence-sexp-p occurrence)
                      (not (skg--node-write-protected-p occurrence))
                      (or (not (memq 'node objects))
                          (seq-some (lambda (v) (and (>= v start) (< v end)))
                                    visited))
                      (or (not (memq 'rel objects))
                          (seq-some
                           (lambda (v)
                             (and (> v start) (< v end)
                                  (string-match-p
                                   (format "(gnid %s)" (regexp-quote partner))
                                   (line-text v))))
                           visited))))))
          (test-fail "Skipped line %d (%s) has no editable occurrence with a visited change:\n%s"
                     line gnid (buffer-string)))))
    skipped))

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
                      (string-match-p "subscriberFolder" (buffer-string)))
                    (skg-test-wait-for-response 1)))))
    (test-fail "Timeout waiting for the \"all changes\" buffer"))
  (set-buffer (all-changes-buffer))
  (dolist (folder '("containerFolder" "mentionerFolder" "subscriberFolder"))
    (unless (string-match-p folder (buffer-string))
      (test-fail "The buffer lacks a %s:\n%s" folder (buffer-string))))
  (test-pass "The diff view shows a's three folders of other files"))

(defun phase-2-plain-and-capital-motions ()
  (setq integration-test-phase "phase-2")
  (set-buffer (all-changes-buffer))
  (let* ((visited (visited-lines #'skg-diffView-goto-next-change))
         (all (visited-lines #'skg-diffView-goto-next-change-anywhere))
         (skipped (check-skipped-changes-are-drawn-editable visited all))
         (skipped-gnids (sort (mapcar (lambda (line)
                                        (and (string-match "(gnid \\([^)]+\\))"
                                                           (line-text line))
                                             (match-string 1 (line-text line))))
                                      skipped)
                              #'string<)))
    (unless (equal skipped-gnids '("r2" "s" "y"))
      (test-fail "Expected r2, s and y skipped, got %S in:\n%s"
                 skipped-gnids (buffer-string)))
    (unless (> (length visited) 3)
      (test-fail "The plain motion visited too little: %S" visited))
    (test-pass "C-c r n c skips r2, s and y in a's folders, each drawn editable elsewhere")))

(defun phase-3-an-editable-member ()
  (setq integration-test-phase "phase-3")
  (set-buffer (all-changes-buffer))
  (goto-char (point-min))
  (re-search-forward "mentionerFolder")
  (re-search-forward "(gnid y)")
  (skg-request-editable-view)
  (setq test-messages nil)
  (skg-request-save-buffer)
  (unless (skg-test-wait-for
           (lambda ()
             (and (cl-some (lambda (m) (string-match-p "changed files are processed" m))
                           test-messages)
                  (with-current-buffer (all-changes-buffer)
                    (save-excursion
                      (goto-char (point-min))
                      (and (re-search-forward "mentionerFolder" nil t)
                           (let ((old-text (save-excursion
                                             (re-search-forward "(oldText" nil t)))
                                 (next-folder (save-excursion
                                                (re-search-forward "subscriberFolder" nil t))))
                             (and old-text next-folder
                                  (< old-text next-folder))))))))
           20)
    (test-fail "y's old text never moved into the mentioners folder:\n%s"
               (with-current-buffer (all-changes-buffer) (buffer-string))))
  (set-buffer (all-changes-buffer))
  (let* ((visited (visited-lines #'skg-diffView-goto-next-change))
         (all (visited-lines #'skg-diffView-goto-next-change-anywhere))
         (old-text-line (save-excursion
                          (goto-char (point-min))
                          (re-search-forward "(oldText")
                          (line-number-at-pos)))
         (y-line (save-excursion
                   (goto-char (point-min))
                   (re-search-forward "mentionerFolder")
                   (re-search-forward "(gnid y)")
                   (line-number-at-pos))))
    (check-skipped-changes-are-drawn-editable visited all)
    (unless (and (memq old-text-line visited) (memq y-line visited))
      (test-fail "The plain motion missed y (line %d) or its old text (line %d), visiting %S in:\n%s"
                 y-line old-text-line visited (buffer-string)))
    (test-pass "C-c r n c visits y, made editable in the mentioners folder, and its old text there")))

(defun run-integration-test ()
  (condition-case err
      (progn
        (phase-1-open-the-diff-view)
        (phase-2-plain-and-capital-motions)
        (phase-3-an-editable-member)
        (message "✓ All phases passed")
        (kill-emacs 0))
    (error (test-fail "Error in %s: %S" integration-test-phase err))))

(run-integration-test)
