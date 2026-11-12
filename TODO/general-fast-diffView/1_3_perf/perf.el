;;; -*- lexical-binding: t; -*-
;;; ONE-TIME TOOL of plan 1_3 ('../1_3_plan.org'): times two readiness
;;; gestures in batch Emacs against an isolated server over a copy of
;;; Jeff's data, and counts the lines each save changes. See
;;; 'README.org' beside this file for how to run it.
;;;
;;; Environment: PERF_DATA, the copy's data root (holding its
;;; skgconfig.toml), and PERF_PORT, the isolated server's port.
(load-file "/home/ubuntu/host/elisp/skg-init.el")
(load-file "/home/ubuntu/host/tests/integration/test-wait.el")
(require 'cl-lib)
(defvar perf-times (make-hash-table :test 'equal))
(defvar perf-depth (make-hash-table :test 'equal))
(defvar perf-before nil)
(defvar perf-new nil)
(defvar perf-paths nil
  "How each buffer the gesture redrew was redrawn, latest first.")
(advice-add 'skg-replace-buffer-with-new-content :before
            (lambda (_p content &rest _)
              (setq perf-new content)
              (push (cons (buffer-name) 'whole) perf-paths))
            '((name . capture)))
(advice-add 'skg--patch-buffer-with-delta :around
            (lambda (orig content viewnode-ids)
              (setq perf-new content)
              (let ((patched (funcall orig content viewnode-ids)))
                (push (cons (buffer-name)
                            (if patched
                                (format "patch of %d headlines"
                                        (length viewnode-ids))
                              "patch failed"))
                      perf-paths)
                patched))
            '((name . capture)))
(defun perf-strip (line)
  (replace-regexp-in-string " \\(focused\\|folded\\|bodyFolded\\)\\_>" "" line))
(defun perf-changed-lines ()
  "How many lines of the server's text the buffer did not hold."
  (let* ((old (split-string perf-before "\n"))
         (new (mapcar #'perf-strip (split-string (or perf-new "") "\n")))
         (oldset (make-hash-table :test 'equal)) (n 0))
    (dolist (l old) (puthash l t oldset))
    (dolist (l new) (unless (gethash l oldset) (setq n (1+ n))))
    (list :old-lines (length old) :new-lines (length new)
          :new-lines-not-in-old n)))
(defun perf-wrap (sym)
  (when (fboundp sym)
    (advice-add sym :around
                (lambda (orig &rest args)
                  (let ((t0 (float-time)) (d (gethash sym perf-depth 0)))
                    (puthash sym (1+ d) perf-depth)
                    (unwind-protect (apply orig args)
                      (puthash sym d perf-depth)
                      (when (= d 0)
                        (puthash sym (+ (gethash sym perf-times 0)
                                        (- (float-time) t0))
                                 perf-times)))))
                '((name . perf)))))
(defconst perf-syms
  '(skg-cycle-commit-ready skg-request-save-buffer
    skg--buffer-snapshot-with-save-metadata skg-handle-save-sexp
    skg-replace-buffer-with-new-content org-fold-show-all
    skg-remove-focused-metadata skg-fold-marked-headlines
    skg-remove-folded-metadata skg--capture-clean-baseline
    skg-link-annotations-refresh heralds-after-change heralds-apply-to-line
    skg--patch-buffer-with-delta skg--reherald-after-rewrites
    skg--collateral-view-handler skg--apply-streamed-view-update
    garbage-collect))
(defun perf-report (label t0)
  (message "=== %s: wall %.0f ms (server time included)"
           label (* 1000 (- (float-time) t0)))
  (let (rows)
    (maphash (lambda (k v) (push (cons k v) rows)) perf-times)
    (dolist (r (sort rows (lambda (a b) (> (cdr a) (cdr b)))))
      (message "  %7.0f ms  %s" (* 1000 (cdr r)) (car r))))
  (clrhash perf-times))
(defun all-changes-buffer ()
  (cl-find-if (lambda (b) (string-match-p "diff-view: all changes" (buffer-name b)))
              (buffer-list)))
(setq skg-port (string-to-number (or (getenv "PERF_PORT") (error "set PERF_PORT"))))
(setq skg-config-dir (file-name-as-directory
                      (or (getenv "PERF_DATA") (error "set PERF_DATA"))))
(message "compiled client? %s"
         (byte-code-function-p (symbol-function 'heralds-apply-to-line)))
(let ((t0 (float-time)))
  (skg-diffView-open)
  (unless (skg-test-wait-for
           (lambda () (let ((b (all-changes-buffer)))
                        (and b (with-current-buffer b (> (buffer-size) 1000))
                             (skg-test-wait-for-response 1))))
           300)
    (error "no all-changes buffer"))
  (skg-test-wait-for (lambda () nil) 3)
  (message "=== open: %.0f ms" (* 1000 (- (float-time) t0))))
(dolist (s perf-syms) (perf-wrap s))
(switch-to-buffer (all-changes-buffer))
(message "buffer: %d chars, %d headlines, fold style %s"
         (buffer-size) (count-matches "^\\*+ " (point-min) (point-max))
         org-fold-core-style)
(defun perf-gesture (label search prefix)
  (goto-char (point-min))
  (unless (re-search-forward search nil t) (error "not found: %s" search))
  (beginning-of-line)
  (message "--- %s on: %s" label
           (buffer-substring (point) (min (line-end-position) (+ (point) 120))))
  (clrhash perf-times)
  (setq perf-before (buffer-string) perf-new nil perf-paths nil)
  (garbage-collect)
  (let ((t0 (float-time)))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (skg-cycle-commit-ready prefix))
    (skg-test-wait-for-response 120)
    (skg-test-wait-for (lambda () nil) 2) ; let link statuses settle
    (perf-report label (+ t0 2.0))
    (message "  lines: %S" (perf-changed-lines))
    (message "  redrawn: %S" (reverse perf-paths))))
;; Jeff's gestures of 2026-11-12: a plain mark on a changeset marker,
;; and the recursive one on a new root.
(perf-gesture "marker"
              "(gnid 3e404b13-5f5a-4a68-a72c-8b701895a83f)[^\n]*\n\\*+ (skg (vnid [0-9a-z]+) (changesetMarker"
              nil)
(perf-gesture "recursive" "(gnid 0311410f-7c27-42bc-9705-37da7d1af688)" '(4))
;; The plain mark again, toggling it back: the first save after the
;; view opened is redrawn whole (the opened text keeps the server's
;; fold atoms as text), so this one shows a later save.
(perf-gesture "marker again"
              "(gnid 3e404b13-5f5a-4a68-a72c-8b701895a83f)[^\n]*\n\\*+ (skg (vnid [0-9a-z]+) (changesetMarker"
              nil)
(kill-emacs 0)
