;;; -*- lexical-binding: t; -*-
;;;
;;; Benchmark: rotate all ~781 headline titles (massive edit).

(load-file "../../../elisp/skg-init.el")
(load-file "../profiling-lib.el")

(defun benchmark-phase-edit ()
  "Phase 2: Rotate all headline titles in the buffer."
  (message "=== PHASE 2: Editing buffer (rotating titles) ===")
  (let ((t3 (current-time)))
    (with-current-buffer "*skg: 0*"
      ;; Collect all headline positions and their titles
      ;; Collect headlines top-to-bottom.
      (let ((headlines '()))
        (goto-char (point-min))
        (while (re-search-forward
                "^\\(\\*+\\) (skg .+))) \\(.*\\)$" nil t)
          (push (list (match-beginning 2) (match-end 2)
                      (match-string 2))
                headlines))
        (setq headlines (nreverse headlines))
        ;; headlines is now top-to-bottom: ((beg end title) ...)
        (message "Found %d headlines to rotate" (length headlines))
        ;; Shift titles down: headline N gets title from N-1.
        ;; First headline gets "Gone!".
        ;; Build replacement list, then apply bottom-to-top
        ;; so positions stay valid.
        (when headlines
          (let* ((old-titles (mapcar (lambda (hl) (nth 2 hl)) headlines))
                 (new-titles (cons "Gone!" (butlast old-titles)))
                 (pairs (cl-mapcar #'list headlines new-titles)))
            (dolist (pair (nreverse pairs))
              (let ((beg (nth 0 (car pair)))
                    (end (nth 1 (car pair)))
                    (new-title (cadr pair)))
                (goto-char beg)
                (delete-region beg end)
                (insert new-title)))))))
    (let ((t4 (current-time)))
      (setq benchmark-edit-time
            (benchmark-format-time t3 t4))
      (message "emacs_edit: %ss" benchmark-edit-time)
      (benchmark-phase-save))))

(run-profiling-test)
