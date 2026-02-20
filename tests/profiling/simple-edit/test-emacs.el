;;; -*- lexical-binding: t; -*-
;;;
;;; Benchmark: change a single headline title (simple edit).

(load-file "../../../elisp/skg-init.el")
(load-file "../profiling-lib.el")

(defun benchmark-phase-edit ()
  "Phase 2: Change the title of headline '1' to '1-edited'."
  (message "=== PHASE 2: Editing buffer (single title change) ===")
  (let ((t3 (current-time)))
    (with-current-buffer "*skg: 0*"
      (goto-char (point-min))
      (if (re-search-forward
           "^\\(\\*+\\) (skg .+))) \\(1\\)$" nil t)
          (progn
            (replace-match "1-edited" t t nil 2)
            (message "Changed title '1' to '1-edited'"))
        (progn
          (message "FAIL: Could not find headline with title '1'")
          (kill-emacs 1))))
    (let ((t4 (current-time)))
      (setq benchmark-edit-time
            (benchmark-format-time t3 t4))
      (message "emacs_edit: %ss" benchmark-edit-time)
      (benchmark-phase-save))))

(run-profiling-test)
