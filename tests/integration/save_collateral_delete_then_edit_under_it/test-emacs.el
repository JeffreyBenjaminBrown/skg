;;; Integration test for delete-then-edit-under-deleted collateral updates
;;;
;;; Buffer 1: multi-root view (scaffolded "1" + standalone subee root).
;;; Buffer 2: manually constructed (indef 11 + subee + subee-1).
;;;
;;; Phase 4: delete 11 from buffer 2 → 11 becomes DeletedNode,
;;;   scaffolds become DeletedScaff in collateral buffer 1.
;;; Phase 6: add subee-2 under subee in buffer 1 → collateral
;;;   buffer 2 picks up subee-2.
;;;
;;; Exercises DeletedNode / DeletedScaff degradation path in
;;; complete_viewtree, and editing under a DeletedNode.

;; Load the project elisp configuration
(load-file "../../../elisp/skg-init.el")
(load-file "../save_collateral_break_cycle/test-helpers.el")

;; Test result tracking
(defvar integration-test-phase "starting")
(defvar integration-test-completed nil)

;; ——— helpers ———

(defun goto-nth-headline-with-title (title n)
  "Move point to the Nth headline (1-based) with title TITLE.
Uses headline-titles style parsing. Returns t if found, nil otherwise."
  (goto-char (point-min))
  (let ((count 0)
        (found nil))
    (while (and (not found) (not (eobp)))
      (let* ((line (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position)))
             (parts (skg-split-as-stars-metadata-title line)))
        (when (and parts (string= (nth 2 parts) title))
          (setq count (1+ count))
          (when (= count n)
            (setq found t))))
      (unless found (forward-line 1)))
    found))

;; ——— phases ———

(defun phase-1-open-buffer-1 ()
  "Open buffer 1: request single-root content view from id \"1\"."
  (message "=== PHASE 1: Open buffer 1 (content view of node 1) ===")
  (setq integration-test-phase "phase-1-open-buffer-1")
  (skg-request-single-root-content-view-from-id "1")
  (sleep-for 2.0)
  (let ((buf (get-buffer "*skg: 1*")))
    (unless buf
      (message "✗ FAIL [phase 1]: Buffer *skg: 1* was not created")
      (kill-emacs 1))
    (message "Buffer 1 initial content:\n%s"
             (with-current-buffer buf
               (buffer-substring-no-properties (point-min) (point-max))))
    (message "✓ PASS [phase 1]: Buffer *skg: 1* created successfully")))

(defun phase-2-extend-buffer-1-with-second-root-and-alias ()
  "Add an aliases view request to node 11, append a standalone
subee root with its child subee-1, and save.
The child must be supplied explicitly: the standalone subee root
is definitive, so the save pipeline generates a save instruction
for it. Without subee-1 in the buffer, that instruction would set
contains to [], overwriting subee.skg on disk."
  (message "=== PHASE 2: Extend buffer 1 with second root and alias ===")
  (setq integration-test-phase "phase-2-extend-buffer-1")
  (with-current-buffer "*skg: 1*"
    ;; Add aliases view request to node 11.
    (goto-nth-headline-with-title "11" 1)
    (skg-edit-metadata-at-point
     '(skg (node (viewRequests aliases))))
    ;; Append standalone subee root with child.
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "* (skg (node (id subee) (source main))) subee\n")
    (insert "** (skg (node (id subee-1) (source main))) subee-1")
    (skg-request-save-buffer))
  (sleep-for 2.0)
  (let ((buf (get-buffer "*skg: 1*")))
    (message "Buffer 1 after multi-root save:\n%s"
             (with-current-buffer buf
               (buffer-substring-no-properties (point-min) (point-max))))
    (assert-headline-types-and-titles
     buf
     '((1 node "1")
       (2 node "11")
       (3 aliasCol "its aliases")
       (4 alias "eleven")
       (3 subscribeeCol "it subscribes to these")
       (4 node "subee")
       (4 hiddenOutsideOfSubscribeeCol "hidden from all subscriptions")
       (5 node "also-hidden")
       (1 node "subee")
       (2 node "subee-1"))
     "phase 2: buffer 1 after multi-root save")))

(defun phase-3-open-buffer-2 ()
  "Create buffer 2 manually with two roots: indef 11 and subee.
subee-1 must be supplied explicitly: the save pipeline's SkgNodeMap
(built from save_instructions) gives subee empty contains, which
takes priority over subee.skg on disk."
  (message "=== PHASE 3: Create and save buffer 2 ===")
  (setq integration-test-phase "phase-3-open-buffer-2")
  (let ((buf (get-buffer-create "*skg-test-buf2*")))
    (with-current-buffer buf
      (erase-buffer)
      (org-mode)
      (setq skg-view-uri (org-id-uuid))
      (insert "* (skg (node (id 11) (source main) indefinitive)) 11\n")
      (insert "* (skg (node (id subee) (source main))) subee\n")
      (insert "** (skg (node (id subee-1) (source main))) subee-1")
      (skg-request-save-buffer))
    (sleep-for 2.0)
    (message "Buffer 2 after save:\n%s"
             (with-current-buffer buf
               (buffer-substring-no-properties (point-min) (point-max))))
    (assert-headline-types-and-titles
     buf
     '((1 node "11")
       (1 node "subee")
       (2 node "subee-1"))
     "phase 3: buffer 2 initial")))

(defun phase-4-delete-11-from-buffer-2 ()
  "Remove indefinitive, add editRequest delete to 11, and save buffer 2."
  (message "=== PHASE 4: Delete 11 from buffer 2 ===")
  (setq integration-test-phase "phase-4-delete-11")
  (with-current-buffer "*skg-test-buf2*"
    (goto-char (point-min))
    (skg-edit-metadata-at-point
     ;; Remove 'indefinitive' and add 'editRequest delete'.
     '(skg (node (DELETE indefinitive) (editRequest delete))))
    (message "Buffer 2 after metadata edit:\n%s"
             (buffer-substring-no-properties (point-min) (point-max)))
    (skg-request-save-buffer))
  (sleep-for 2.0))

(defun phase-5-verify-after-delete ()
  "Verify both buffers after deleting 11."
  (message "=== PHASE 5: Verify both buffers after delete ===")
  (setq integration-test-phase "phase-5-verify-after-delete")

  ;; Verify buffer 2: '11' became DeletedNode, 'subee' is unchanged
  (let ((buf2 (get-buffer "*skg-test-buf2*")))
    (unless buf2
      (message "✗ FAIL [phase 5]: Buffer 2 no longer exists")
      (kill-emacs 1))
    (message "Buffer 2 after delete:\n%s"
             (with-current-buffer buf2
               (buffer-substring-no-properties (point-min) (point-max))))
    (assert-headline-types-and-titles
     buf2
     '((1 deleted "11")
       (1 node "subee")
       (2 node "subee-1"))
     "phase 5: buffer 2 after delete"))

  ;; Verify buffer 1 (collateral): 11 → DeletedNode,
  ;; its scaffolds → DeletedScaff, standalone subee unaffected.
  (let ((buf1 (get-buffer "*skg: 1*")))
    (unless buf1
      (message "✗ FAIL [phase 5]: Buffer 1 no longer exists")
      (kill-emacs 1))
    (message "Buffer 1 after collateral delete:\n%s"
             (with-current-buffer buf1
               (buffer-substring-no-properties (point-min) (point-max))))
    (assert-headline-types-and-titles
     buf1
     '((1 node "1")
       (2 deleted "11")
       (3 deletedScaffold "")
       (4 node "subee")
       (4 deletedScaffold "")
       (5 node "also-hidden")
       (1 node "subee")
       (2 node "subee-1"))
     "phase 5: buffer 1 after collateral delete")))

(defun phase-6-add-subee-2-from-buffer-1 ()
  "Add subee-2 as a child of the standalone subee root in buffer 1 and save."
  (message "=== PHASE 6: Add subee-2 in buffer 1 ===")
  (setq integration-test-phase "phase-6-add-subee-2")
  (with-current-buffer "*skg: 1*"
    ;; Find the SECOND subee headline (the standalone root, which is
    ;; definitive). The first subee is indefinitive under the deleted
    ;; scaffold and won't update subee.skg's contains.
    (when (goto-nth-headline-with-title "subee" 2)
      (let ((subee-depth
             (let* ((line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position)))
                    (parts (skg-split-as-stars-metadata-title line)))
               (length (string-trim-right (nth 0 parts))))))
        ;; Move forward past all children (deeper headlines)
        (forward-line 1)
        (while (and (not (eobp))
                    (let* ((line (buffer-substring-no-properties
                                  (line-beginning-position) (line-end-position)))
                           (parts (skg-split-as-stars-metadata-title line)))
                      (and parts
                           (> (length (string-trim-right (nth 0 parts)))
                              subee-depth))))
          (forward-line 1))
        ;; Insert subee-2 as a child of the standalone subee root.
        (let ((child-stars (make-string (1+ subee-depth) ?*)))
          (insert (format "%s subee-2\n" child-stars)))))
    (message "Buffer 1 after inserting subee-2:\n%s"
             (buffer-substring-no-properties (point-min) (point-max)))
    (skg-request-save-buffer))
  (sleep-for 2.0))

(defun phase-7-verify-after-add ()
  "Verify both buffers after adding subee-2."
  (message "=== PHASE 7: Verify both buffers after adding subee-2 ===")
  (setq integration-test-phase "phase-7-verify-after-add")

  ;; Verify buffer 1: subee-2 appears under the standalone subee root
  (let ((buf1 (get-buffer "*skg: 1*")))
    (unless buf1
      (message "✗ FAIL [phase 7]: Buffer 1 no longer exists")
      (kill-emacs 1))
    (message "Buffer 1 after adding subee-2:\n%s"
             (with-current-buffer buf1
               (buffer-substring-no-properties (point-min) (point-max))))
    (assert-headline-types-and-titles
     buf1
     '((1 node "1")
       (2 deleted "11")
       (3 deletedScaffold "")
       (4 node "subee")
       (4 deletedScaffold "")
       (5 node "also-hidden")
       (1 node "subee")
       (2 node "subee-1")
       (2 node "subee-2"))
     "phase 7: buffer 1 after adding subee-2"))

  ;; Verify buffer 2: gains subee-2 from collateral update
  (let ((buf2 (get-buffer "*skg-test-buf2*")))
    (unless buf2
      (message "✗ FAIL [phase 7]: Buffer 2 no longer exists")
      (kill-emacs 1))
    (message "Buffer 2 after collateral update:\n%s"
             (with-current-buffer buf2
               (buffer-substring-no-properties
                (point-min) (point-max))))
    (assert-headline-types-and-titles
     buf2
     '((1 deleted "11")
       (1 node "subee")
       (2 node "subee-1")
       (2 node "subee-2"))
     "phase 7: buffer 2 after updating collateral view")))

(defun run-all-tests ()
  "Main orchestrator."
  (message "=== SKG Collateral Delete Then Edit Under It Integration Test ===")

  ;; Set the port from environment variable if available
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))
      (message "Using test port: %d" skg-port)))

  ;; Wait a moment for server to be fully ready
  (sleep-for 0.25)

  (phase-1-open-buffer-1)
  (phase-2-extend-buffer-1-with-second-root-and-alias)
  (phase-3-open-buffer-2)
  (phase-4-delete-11-from-buffer-2)
  (phase-5-verify-after-delete)
  (phase-6-add-subee-2-from-buffer-1)
  (phase-7-verify-after-add)

  (message "✓ PASS: All phases completed successfully!")
  (setq integration-test-completed t)
  (kill-emacs 0))

(progn ;; Run the test with a timeout in case things hang.
  (run-at-time
   40 nil
   (lambda ()
     (message "TIMEOUT: Integration test timed out!")
     (message "Last phase: %s" integration-test-phase)
     (kill-emacs 1)))
  ( ;; the test
   run-all-tests))
