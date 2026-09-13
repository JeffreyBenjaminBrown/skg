;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'skg-buffer)
(require 'skg-request-save)

(defun test-skg-two-dirty-buffer-warning--make-view (name uri)
  "Make a clean skg view buffer named NAME with view URI URI."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (insert "* view\n")
      (skg-content-view-mode)
      (setq skg-view-uri uri)
      (skg--install-two-dirty-buffer-warning-hooks)
      (set-buffer-modified-p nil))
    buffer))

(defun test-skg-two-dirty-buffer-warning--make-dirty (buffer)
  "Make BUFFER dirty without exercising its user-edit warning."
  (with-current-buffer buffer
    (let ((skg--buffer-warned_two-dirty-buffers_since-last-looked-here t))
      (goto-char (point-max))
      (insert "dirty\n"))))

(defun test-skg-two-dirty-buffer-warning--kill-views (&rest buffers)
  "Kill BUFFERS without asking whether to save them."
  (dolist (buffer buffers)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (set-buffer-modified-p nil))
      (kill-buffer buffer))))

(ert-deftest test-skg-two-dirty-buffer-warning-repeats-after-point-returns ()
  "Accepting permits edits until point leaves the buffer and returns."
  (let ((first (test-skg-two-dirty-buffer-warning--make-view
                " *skg dirty warning first*" "first"))
        (second (test-skg-two-dirty-buffer-warning--make-view
                 " *skg dirty warning second*" "second"))
        (prompt-count 0))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((first-window (selected-window))
                (second-window (split-window-right)))
            (set-window-buffer first-window first)
            (set-window-buffer second-window second)
            (select-window second-window)
            (test-skg-two-dirty-buffer-warning--make-dirty first)
            (with-current-buffer second
              (let ((noninteractive nil))
                (cl-letf (((symbol-function 'yes-or-no-p)
                           (lambda (_prompt)
                             (setq prompt-count (1+ prompt-count))
                             t)))
                  (goto-char (point-max))
                  (insert "first edit\n")
                  (should (= prompt-count 1))
                  (should
                   skg--buffer-warned_two-dirty-buffers_since-last-looked-here)
                  (insert "same visit\n")
                  (should (= prompt-count 1))
                  (select-window first-window)
                  (select-window second-window)
                  (should-not
                   skg--buffer-warned_two-dirty-buffers_since-last-looked-here)
                  (insert "after returning\n")
                  (should (= prompt-count 2)))))))
      (test-skg-two-dirty-buffer-warning--kill-views first second))))

(ert-deftest test-skg-two-dirty-buffer-warning-declining-aborts-edit ()
  "Declining preserves the buffer and asks again on another edit attempt."
  (let ((first (test-skg-two-dirty-buffer-warning--make-view
                " *skg dirty warning decline first*" "decline-first"))
        (second (test-skg-two-dirty-buffer-warning--make-view
                 " *skg dirty warning decline second*" "decline-second"))
        (prompt-count 0))
    (unwind-protect
        (progn
          (test-skg-two-dirty-buffer-warning--make-dirty first)
          (with-current-buffer second
            (let ((before (buffer-string))
                  (noninteractive nil))
              (cl-letf (((symbol-function 'yes-or-no-p)
                         (lambda (_prompt)
                           (setq prompt-count (1+ prompt-count))
                           nil)))
                (goto-char (point-max))
                (should-error (insert "refused\n") :type 'user-error)
                (should (string= before (buffer-string)))
                (should-not
                 skg--buffer-warned_two-dirty-buffers_since-last-looked-here))
              (run-hooks 'post-command-hook)
              (should (memq #'org-before-change-function
                            before-change-functions))
              (cl-letf (((symbol-function 'yes-or-no-p)
                         (lambda (_prompt)
                           (setq prompt-count (1+ prompt-count))
                           t)))
                (insert "accepted\n")
                (should (= prompt-count 2))))))
      (test-skg-two-dirty-buffer-warning--kill-views first second))))

(ert-deftest test-skg-two-dirty-buffer-warning-quitting-rearms-warning ()
  "Quitting the prompt aborts the edit without losing modification hooks."
  (let ((first (test-skg-two-dirty-buffer-warning--make-view
                " *skg dirty warning quit first*" "quit-first"))
        (second (test-skg-two-dirty-buffer-warning--make-view
                 " *skg dirty warning quit second*" "quit-second")))
    (unwind-protect
        (progn
          (test-skg-two-dirty-buffer-warning--make-dirty first)
          (with-current-buffer second
            (let ((before (buffer-string))
                  (noninteractive nil)
                  (quit-caught nil))
              (cl-letf (((symbol-function 'yes-or-no-p)
                         (lambda (_prompt) (signal 'quit nil))))
                (goto-char (point-max))
                (condition-case nil
                    (insert "quit\n")
                  (quit (setq quit-caught t)))
                (should quit-caught)
                (should (string= before (buffer-string))))
              (run-hooks 'post-command-hook)
              (should (memq #'skg-warn-if-other-buffer-modified
                            before-change-functions))
              (should (memq #'org-before-change-function
                            before-change-functions)))))
      (test-skg-two-dirty-buffer-warning--kill-views first second))))

(ert-deftest test-skg-two-dirty-buffer-warning-ignores-server-redraw ()
  "A server redraw neither prompts nor leaves the next edit unchecked."
  (let ((first (test-skg-two-dirty-buffer-warning--make-view
                " *skg dirty warning redraw first*" "redraw-first"))
        (second (test-skg-two-dirty-buffer-warning--make-view
                 " *skg dirty warning redraw second*" "redraw-second")))
    (unwind-protect
        (progn
          (test-skg-two-dirty-buffer-warning--make-dirty first)
          (with-current-buffer second
            (setq skg--buffer-warned_two-dirty-buffers_since-last-looked-here
                  t)
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (_prompt)
                         (ert-fail "Server redraw prompted for an edit"))))
              (skg-replace-buffer-with-new-content
               nil "* (skg (node (id root))) redrawn\n"))
            (should-not (buffer-modified-p))
            (should-not
             skg--buffer-warned_two-dirty-buffers_since-last-looked-here)
            (should (memq #'skg-warn-if-other-buffer-modified
                          before-change-functions))))
      (test-skg-two-dirty-buffer-warning--kill-views first second))))

(provide 'test-skg-two-dirty-buffer-warning)
