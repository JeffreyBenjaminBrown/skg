;;; Integration test: forks of one note meet, in Emacs
;;; ('docs/forks.org', "Forks of one note meet"; run-test.sh gives the
;;; data). The user's own gestures, and what the user then sees:
;;; 1. R's view. N stands editable in R's forks folder, its herald bK
;;;    (a forkee); in N's forks folder R is a repeat, showing nothing,
;;;    and T (herald Kb, a forker) shows Y, with Z folded in its folder
;;;    of what N hides in it; each containers folder's member (Shelf)
;;;    is folded.
;;; 2. X4, which N shows there, is cut (org-cut-subtree) and the view
;;;    saved: X4 stands in N's folder of what R hides in it.
;;; 3. N's view. Y is cut from T and pasted into N's own content
;;;    (org-paste-subtree), and the view saved: T stands first in N's
;;;    forks folder, claiming, its herald bKb (a mutual); R follows;
;;;    Y stands in N's content.

(load-file "../../../elisp/skg-init.el")
(load-file "../test-wait.el")

(defun test-fail (message &rest args)
  "Report test failure and exit."
  (apply #'message (concat "✗ FAIL: " message) args)
  (message "Buffer:\n%s" (buffer-substring-no-properties (point-min) (point-max)))
  (kill-emacs 1))

;; ——— helpers ———

(defun forks-meet-goto-line-showing (gnid &optional nth)
  "Move point to the start of the NTH (from 0, default 0) line showing
GNID. Fail if there is none."
  (goto-char (point-min))
  (dotimes (_ (1+ (or nth 0)))
    (unless (re-search-forward
             (concat "^\\*+ (skg .*" (regexp-quote (format "(gnid %s)" gnid)))
             nil t)
      (test-fail "no line %d showing %s" (or nth 0) gnid)))
  (beginning-of-line))

(defun forks-meet-depth ()
  "The star count of the headline at point."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward "*")
    (- (point) (line-beginning-position))))

(defun forks-meet-line ()
  "The text of the line at point."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun forks-meet-herald ()
  "The herald the user sees on the line at point."
  (let ((herald
         (seq-some (lambda (overlay)
                     (and (overlay-get overlay 'heralds)
                          (overlay-get overlay 'display)))
                   (overlays-in (line-beginning-position) (line-end-position)))))
    (and herald (substring-no-properties herald))))

(defun forks-meet-drawn-folded-p ()
  "Whether the headline at point is drawn folded: hidden, or, in a view
just opened, still carrying the server's 'folded' atom, which the
Emacs client acts on only at a save's redraw (see TODO/problems.org,
the entry on 'skg-open-org-buffer-from-text')."
  (or (invisible-p (point))
      (string-match-p "^\\*+ (skg \\((vnid [^)]*) \\)?folded " (forks-meet-line))))

(defun forks-meet-next-line ()
  "Move point to the next line, folded or not."
  (forward-line 1))

(defun forks-meet-wait-for-view (buffer-name)
  "Wait for BUFFER-NAME and the response that fills it; return it."
  (let ((buffer (skg-test-wait-for-buffer buffer-name 15)))
    (unless buffer (test-fail "no buffer %s" buffer-name))
    (skg-test-wait-for-response 15)
    buffer))

(defun forks-meet-save ()
  "Save the view in the current buffer and wait for its redrawing."
  (skg-request-save-buffer)
  (unless (skg-test-wait-for-response 20)
    (test-fail "the save got no response")))

;; ——— phases ———

(defun phase-1-rs-view ()
  "R's view as drawn."
  (message "=== PHASE 1: R's view ===")
  (skg-request-single-root-content-view-from-id "R")
  (with-current-buffer (forks-meet-wait-for-view "*R*")
    (message "R's view:\n%s" (buffer-substring-no-properties (point-min) (point-max)))
    (forks-meet-goto-line-showing "N")
    (let ((line (forks-meet-line)) (herald (forks-meet-herald)))
      (message "N's herald in R's forks folder: %s" herald)
      (when (or (string-match-p "writeProtected" line)
                (not herald)
                (not (string-match-p "bK" herald))
                (string-match-p "Kb" herald))
        (test-fail "N should stand editable in R's forks folder, a forkee (bK); herald %S"
                   herald)))
    (forks-meet-goto-line-showing "R" 1)
    (let ((repeat-depth (forks-meet-depth)))
      (unless (string-match-p "writeProtected" (forks-meet-line))
        (test-fail "R in N's forks folder should be a write-protected repeat"))
      (forks-meet-next-line)
      (unless (<= (forks-meet-depth) repeat-depth)
        (test-fail "the repeat R should show nothing")))
    (forks-meet-goto-line-showing "T")
    (let ((t-depth (forks-meet-depth)) (herald (forks-meet-herald)))
      (message "T's herald in N's forks folder: %s" herald)
      (when (or (string-match-p "writeProtected" (forks-meet-line))
                (not herald)
                (not (string-match-p "Kb" herald))
                (string-match-p "bK" herald))
        (test-fail "T should stand editable in N's forks folder, a forker (Kb); herald %S"
                   herald))
      (forks-meet-next-line)
      (unless (and (string-match-p "hiddenInForkPartnerFolder" (forks-meet-line))
                   (= (forks-meet-depth) (1+ t-depth)))
        (test-fail "T's first child should be its folder of what N hides in it"))
      (forks-meet-next-line)
      (unless (and (string-match-p "(gnid Z)" (forks-meet-line))
                   (forks-meet-drawn-folded-p))
        (test-fail "that folder should hold Z, folded"))
      (forks-meet-goto-line-showing "Y")
      (unless (and (= (forks-meet-depth) (1+ t-depth))
                   (not (invisible-p (point))))
        (test-fail "T should show Y")))
    (goto-char (point-min))
    (let ((containers-folders 0))
      (while (re-search-forward "^\\*+ (skg .*containerFolder" nil t)
        (setq containers-folders (1+ containers-folders))
        (when (invisible-p (line-beginning-position))
          (test-fail "a containers folder's headline should show"))
        (forward-line 1)
        (unless (and (string-match-p "(gnid Shelf)" (forks-meet-line))
                     (forks-meet-drawn-folded-p))
          (test-fail "a containers folder's member, Shelf, should be folded")))
      (unless (= containers-folders 2)
        (test-fail "N and T should each carry a containers folder; found %d"
                   containers-folders)))
    (message "✓ PASS [phase 1]: R's view")))

(defun phase-2-cut-x4-from-n ()
  "Cut X4 from N in R's view; save."
  (message "=== PHASE 2: Cut X4 from N in R's view ===")
  (with-current-buffer "*R*"
    (forks-meet-goto-line-showing "X4")
    (org-cut-subtree)
    (forks-meet-save)
    (message "R's view after the save:\n%s"
             (buffer-substring-no-properties (point-min) (point-max)))
    (forks-meet-goto-line-showing "N")
    (let ((n-depth (forks-meet-depth)))
      (forks-meet-next-line)
      (unless (and (string-match-p "hiddenInForkPartnerFolder" (forks-meet-line))
                   (= (forks-meet-depth) (1+ n-depth)))
        (test-fail "N's first child should now be its folder of what R hides in it"))
      (forks-meet-next-line)
      (unless (and (string-match-p "(gnid X4)" (forks-meet-line))
                   (invisible-p (point)))
        (test-fail "that folder should hold X4, folded")))
    (goto-char (point-min))
    (while (re-search-forward "^\\*+ (skg .*containerFolder" nil t)
      (forward-line 1)
      (unless (and (string-match-p "(gnid Shelf)" (forks-meet-line))
                   (invisible-p (point)))
        (test-fail "after the redraw, a containers folder's member should be hidden")))
    (message "✓ PASS [phase 2]: X4 stands in N's folder of what R hides in it")))

(defun phase-3-move-y-from-t-into-n ()
  "In N's view, cut Y from T and paste it into N's content; save."
  (message "=== PHASE 3: Move Y from T into N, in N's view ===")
  (skg-request-single-root-content-view-from-id "N")
  (with-current-buffer (forks-meet-wait-for-view "*N*")
    (message "N's view:\n%s" (buffer-substring-no-properties (point-min) (point-max)))
    (forks-meet-goto-line-showing "Y")
    (org-cut-subtree)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (org-paste-subtree 2)
    (forks-meet-save)
    (message "N's view after the save:\n%s"
             (buffer-substring-no-properties (point-min) (point-max)))
    (goto-char (point-min))
    (unless (re-search-forward "^\\*\\* (skg .*forksFolder" nil t)
      (test-fail "N should show its forks folder"))
    (forward-line 1)
    (let ((line (forks-meet-line)) (herald (forks-meet-herald)))
      (message "T's herald, first in N's forks folder: %s" herald)
      (unless (and (string-match-p "(gnid T)" line)
                   (not (string-match-p "noClaim" line))
                   (not (string-match-p "writeProtected" line))
                   herald
                   (string-match-p "bKb" herald))
        (test-fail "T should stand first in N's forks folder, an editable mutual (bKb); herald %S"
                   herald)))
    (let ((member-depth (forks-meet-depth)))
      (forward-line 1)
      (while (> (forks-meet-depth) member-depth) (forward-line 1))
      (unless (and (= (forks-meet-depth) member-depth)
                   (string-match-p "(gnid R)" (forks-meet-line)))
        (test-fail "R should follow T in N's forks folder")))
    (forks-meet-goto-line-showing "Y")
    (unless (= (forks-meet-depth) 2)
      (test-fail "Y should stand in N's own content"))
    (message "✓ PASS [phase 3]: T is a mutual, first, and Y is N's")))

(let ((test-port (getenv "SKG_TEST_PORT")))
  (when test-port
    (setq skg-port (string-to-number test-port))
    (message "Using test port: %d" skg-port)))

(run-at-time 60 nil (lambda ()
                      (message "TIMEOUT: Integration test timed out!")
                      (kill-emacs 1)))

(skg-tcp-connect-to-rust)
(unless (skg-herald-rules-ensure)
  (test-fail "no herald rules from the integration server"))
(phase-1-rs-view)
(phase-2-cut-x4-from-n)
(phase-3-move-y-from-t-into-n)
(message "PASS: Integration test successful!")
(kill-emacs 0)
