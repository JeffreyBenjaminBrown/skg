;;; Integration test for the save-lock lifecycle (plan_v2 §20.2a).
;;;
;;; Opens THREE overlapping views, edits and saves one, and asserts that the
;;; truly-non-collateral view becomes editable once the save stream settles --
;;; i.e. the lock the save took out over every open view was actually released.
;;;
;;; Graph: a contains b; solo is standalone.
;;;   *a*    -- the SAVED view.
;;;   *b*    -- COLLATERAL to saving a (b's view shows a as containerward, so its
;;;             pid set contains a; saving a re-renders it).
;;;   *solo* -- truly NON-COLLATERAL to saving a (its pid set is {solo}).
;;;
;;; Response-handler advice observes both intermediate protocol boundaries
;;; synchronously, before the next framed message can be dispatched.
;;;
;;; File system operations (backup/cleanup) are handled by run-test.sh.

(load-file "../../../elisp/skg-init.el")
(load-file "../test-wait.el")

(defvar integration-test-phase "starting")
(defvar integration-test-completed nil)
(defvar integration-test-saw-broad-lock nil)
(defvar integration-test-saw-relaxed-lock nil)

(defun integration-test-observe-broad-lock (original payload)
  "Assert that broad acknowledgement has not unlocked any local view."
  (funcall original payload)
  (dolist (name '("*a*" "*b*" "*solo*"))
    (unless (buffer-local-value 'skg--save-lock-overlay (get-buffer name))
      (message "✗ FAIL: %s unlocked at broad save-lock" name)
      (kill-emacs 1)))
  (setq integration-test-saw-broad-lock t)
  (message "✓ broad save-lock retained every local view"))

(defun integration-test-observe-relaxed-lock (original saved-uri payload)
  "Assert the narrowed intermediate lock set before collateral streaming."
  (funcall original saved-uri payload)
  (unless (and (buffer-local-value 'skg--save-lock-overlay
                                    (get-buffer "*a*"))
               (buffer-local-value 'skg--save-lock-overlay
                                    (get-buffer "*b*"))
               (not (buffer-local-value 'skg--save-lock-overlay
                                         (get-buffer "*solo*"))))
    (message "✗ FAIL: unexpected lock set after save-relax-lock: a=%S b=%S solo=%S payload=%s"
             (not (null (buffer-local-value 'skg--save-lock-overlay
                                             (get-buffer "*a*"))))
             (not (null (buffer-local-value 'skg--save-lock-overlay
                                             (get-buffer "*b*"))))
             (not (null (buffer-local-value 'skg--save-lock-overlay
                                             (get-buffer "*solo*"))))
             payload)
    (kill-emacs 1))
  (setq integration-test-saw-relaxed-lock t)
  (message "✓ save-relax-lock freed only the unrelated clean view"))

(defun phase-1-open-three-views ()
  "Open views *a*, *b*, and *solo*."
  (message "=== PHASE 1: Open three overlapping views ===")
  (setq integration-test-phase "phase-1-open-three-views")
  (dolist (id '("a" "b" "solo"))
    (skg-request-single-root-content-view-from-id id)
    (skg-test-wait-for-buffer (format "*%s*" id))
    (unless (get-buffer (format "*%s*" id))
      (message "✗ FAIL [phase 1]: buffer *%s* was not created" id)
      (kill-emacs 1))
    (message "✓ opened view *%s*" id)))

(defun phase-2-edit-and-save-a ()
  "Edit a's title in *a* and save it."
  (message "=== PHASE 2: Edit a's title and save *a* ===")
  (setq integration-test-phase "phase-2-edit-and-save-a")
  (advice-add 'skg--broad-save-lock-handler :around
              #'integration-test-observe-broad-lock)
  (advice-add 'skg--save-relax-lock-handler :around
              #'integration-test-observe-relaxed-lock)
  (with-current-buffer "*a*"
    (goto-char (point-min))
    (end-of-line)            ;; end of the root line (a's title)
    (insert " edited")
    (message "Buffer *a* after edit:\n%s"
             (buffer-substring-no-properties (point-min) (point-max)))
    (skg-request-save-buffer))
  (skg-test-wait-for-response))

(defun phase-3-verify-locks-released ()
  "After the save settles, every view's save-lock is released, and the
truly-non-collateral view *solo* is genuinely editable."
  (message "=== PHASE 3: Verify locks released after save ===")
  (setq integration-test-phase "phase-3-verify-locks-released")

  ;; The streaming guard must be clear -- a wedged guard would block the next op.
  (when skg--stream-in-progress
    (message "✗ FAIL [§20.2a]: skg--stream-in-progress still set after save: %S"
             skg--stream-in-progress)
    (kill-emacs 1))
  (unless (and integration-test-saw-broad-lock
               integration-test-saw-relaxed-lock)
    (message "✗ FAIL: both intermediate lock messages were not observed")
    (kill-emacs 1))

  (dolist (spec '(("*solo*" . "non-collateral")
                  ("*a*"    . "saved")
                  ("*b*"    . "collateral")))
    (let* ((name (car spec))
           (role (cdr spec))
           (buf  (get-buffer name)))
      (unless buf
        (message "✗ FAIL [§20.2a]: buffer %s (%s) no longer exists" name role)
        (kill-emacs 1))
      (when (buffer-local-value 'skg--save-lock-overlay buf)
        (message "✗ FAIL [§20.2a]: %s view %s still save-locked after the stream settled"
                 role name)
        (kill-emacs 1))
      (message "✓ %s view %s has no lingering save-lock" role name)))

  ;; Prove the non-collateral buffer is genuinely editable: the lock overlay's
  ;; modification-hook is gone, not merely the bookkeeping variable cleared.
  (with-current-buffer "*solo*"
    (condition-case err
        (progn
          (goto-char (point-max))
          (insert "\n;; editable after save")
          (message "✓ PASS [§20.2a]: non-collateral view *solo* is editable after the save settled"))
      (error
       (message "✗ FAIL [§20.2a]: editing non-collateral view *solo* errored: %S" err)
       (kill-emacs 1)))))

(defun run-all-tests ()
  "Main orchestrator."
  (message "=== SKG Save-Lock Lifecycle Integration Test ===")
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))
      (message "Using test port: %d" skg-port)))

  (phase-1-open-three-views)
  (phase-2-edit-and-save-a)
  (phase-3-verify-locks-released)

  (message "✓ PASS: All phases completed successfully!")
  (setq integration-test-completed t)
  (kill-emacs 0))

(progn ;; Run the test with a timeout in case things hang.
  (run-at-time
   30 nil
   (lambda ()
     (message "TIMEOUT: Integration test timed out!")
     (message "Last phase: %s" integration-test-phase)
     (kill-emacs 1)))
  ( ;; the test
   run-all-tests))
