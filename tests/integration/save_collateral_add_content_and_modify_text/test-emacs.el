;;; Integration test for collateral buffer updates: title edit + add child
;;;
;;; Opens two buffers over a containment cycle (a contains b, b contains a).
;;; From buffer B, edits a's title and adds a new child "c" of a (no
;;; metadata — the server reads it as an occurrence with a random UUID).
;;; Verifies that buffer B's save response reflects the changes (title,
;;; new node with UUID). Then checks collateral buffer A, which the
;;; refresh of its collateral rerender gives the new title and the new
;;; child c, reporting c as a buffer correction of A.

;; Load the project elisp configuration
(load-file "../../../elisp/skg-init.el")
(load-file "../save_collateral_break_cycle/test-helpers.el")
(load-file "../test-wait.el")

;; Test result tracking
(defvar integration-test-phase "starting")
(defvar integration-test-completed nil)

(defun phase-1-open-buffer-a ()
  "Open buffer A and verify its initial structure."
  (message "=== PHASE 1: Open buffer A ===")
  (setq integration-test-phase "phase-1-open-buffer-a")
  (skg-request-single-root-content-view-from-id "a")
  (skg-test-wait-for-buffer "*a*")
  (let ((buf (get-buffer "*a*")))
    (unless buf
      (message "✗ FAIL [phase 1]: Buffer *a* was not created")
      (kill-emacs 1))
    (assert-headline-structure
     buf
     '((1 member "a")
       (3 member "b")         ; in a's containers folder
       (4 container "a")      ; its chain
       (5 container "b")
       (2 member     "b")
       (3 member     "a"))
     "phase 1: buffer A initial")))

(defun phase-2-open-buffer-b ()
  "Open buffer B and verify its initial structure."
  (message "=== PHASE 2: Open buffer B ===")
  (setq integration-test-phase "phase-2-open-buffer-b")
  (skg-request-single-root-content-view-from-id "b")
  (skg-test-wait-for-buffer "*b*")
  (let ((buf (get-buffer "*b*")))
    (unless buf
      (message "✗ FAIL [phase 2]: Buffer *b* was not created")
      (kill-emacs 1))
    (assert-headline-structure
     buf
     '((1 member "b")
       (3 member "a")         ; in b's containers folder
       (4 container "b")      ; its chain
       (5 container "a")
       (2 member     "a")
       (3 member     "b"))
     "phase 2: buffer B initial")))

(defun phase-3-edit-and-save ()
  "Edit a's title, add child c of a, and save buffer B."
  (message "=== PHASE 3: Edit a's title, add child c, save ===")
  (setq integration-test-phase "phase-3-edit-and-save")
  (let ((buf (get-buffer "*b*")))
    ;; Verify pre-edit structure
    (assert-headline-titles
     buf
     '((1 member "b")
       (2 member "")          ; b's containers folder
       (3 member "a")
       (4 container "b")
       (5 container "a")
       (2 member     "a")
       (3 member     "b"))
     "phase 3: buffer B before edit")
    (with-current-buffer buf
      ;; Change a's title on line 6 (the editable a):
      ;; replace trailing "a" with new title.
      (goto-char (point-min))
      (forward-line 5)
      (end-of-line)
      (delete-char -1)
      (insert "Node a was given this longer title")
      ;; Add a new child of a with no metadata
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "*** c"))
    ;; Verify post-edit structure
    (assert-headline-titles
     buf
     '((1 member "b")
       (2 member "")
       (3 member "a")
       (4 container "b")
       (5 container "a")
       (2 member     "Node a was given this longer title")
       (3 member     "b")
       (3 member     "c"))
     "phase 3: buffer B after edit")
    (with-current-buffer buf
      (skg-request-save-buffer))
    (skg-test-wait-for-response)))

(defun phase-4-verify-saved-buffer ()
  "Verify that buffer B's save response reflects the edits."
  (message "=== PHASE 4: Verify buffer B after save ===")
  (setq integration-test-phase "phase-4-verify-saved-buffer")
  (let ((buf (get-buffer "*b*")))
    (unless buf
      (message "✗ FAIL [phase 4]: Buffer *b* no longer exists")
      (kill-emacs 1))
    (assert-headline-titles
     buf
     '((1 member "b")
       (2 member "")
       (3 member "Node a was given this longer title")
       (4 container "b")
       (5 container "Node a was given this longer title")
       (2 member     "Node a was given this longer title")
       (3 member     "b")
       (3 member     "c"))
     "phase 4: buffer B after save")
    (with-current-buffer buf ;; Verify the line for c has its title and an ID
      (goto-char (point-min))
      (forward-line 7)
      (let* ((line (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position)))
             (parts (skg-split-as-stars-metadata-title line))
             (metadata-str (nth 1 parts))
             (title (nth 2 parts))
             (sexp (condition-case nil
                       (car (read-from-string metadata-str))
                     (error nil)))
             (gnid (skg--extract-id-from-metadata-sexp sexp)))
        (unless (equal title "c")
          (message "✗ FAIL [phase 4]: line for c had title %S, expected \"c\"" title)
          (kill-emacs 1))
        (unless gnid
          (message "✗ FAIL [phase 4]: line for c has no node id in metadata")
          (message "  Metadata: %S" metadata-str)
          (kill-emacs 1))
        (message "✓ PASS [phase 4]: c was assigned UUID %s" gnid)))))

(defun phase-5-verify-collateral ()
  "Check collateral buffer A.
The collateral update succeeds: buffer A picks up the title change
and the new child c."
  (message "=== PHASE 5: Verify collateral buffer A ===")
  (setq integration-test-phase "phase-5-verify-collateral")
  (let ((buf (get-buffer "*a*")))
    (unless buf
      (message "✗ FAIL [phase 5]: Buffer *a* no longer exists")
      (kill-emacs 1))
    (message "Buffer A content: %S"
             (with-current-buffer buf
               (buffer-substring-no-properties (point-min) (point-max))))
    (assert-headline-titles
     buf
     '((1 member "Node a was given this longer title")
       (2 member "")
       (3 member "b")
       (4 container "Node a was given this longer title")
       (5 container "b")
       (2 member     "b")
       (3 member     "Node a was given this longer title")
       (2 member     "c"))
     "phase 5: buffer A after collateral update")))

(defun phase-6-verify-correction-in-warnings-buffer ()
  "Buffer A's editable a lacked c, which the save drew there: a
buffer correction of buffer A, which the warnings buffer shows under
a headline naming buffer A."
  (message "=== PHASE 6: Verify the buffer correction in the warnings buffer ===")
  (setq integration-test-phase "phase-6-verify-correction-in-warnings-buffer")
  (let ((buf (get-buffer skg-server-warnings-buffer-name)))
    (unless buf
      (message "✗ FAIL [phase 6]: no buffer %s" skg-server-warnings-buffer-name)
      (kill-emacs 1))
    (let ((text (with-current-buffer buf
                  (buffer-substring-no-properties (point-min) (point-max)))))
      (message "Warnings buffer content: %S" text)
      (unless (string-match-p
               (concat "^\\* Buffer corrections in \\*a\\*\n"
                       "\\*\\* Drew what the graph says belongs here\\.\n"
                       "\\*\\*\\* c\n")
               text)
        (message "✗ FAIL [phase 6]: the buffer correction drawing c is not under buffer A's headline")
        (kill-emacs 1))
      (when (string-match-p "^\\* Buffer corrections in \\*b\\*$" text)
        (message "✗ FAIL [phase 6]: the saved buffer B reported a buffer correction")
        (kill-emacs 1))
      (message "✓ PASS [phase 6]: the buffer correction is under buffer A's headline"))))

(defun run-all-tests ()
  "Main orchestrator function that runs all integration tests."
  (message "=== SKG Collateral Add Content Integration Test ===")

  ;; Set the port from environment variable if available
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))
      (message "Using test port: %d" skg-port)))

  (phase-1-open-buffer-a)
  (phase-2-open-buffer-b)
  (phase-3-edit-and-save)
  (phase-4-verify-saved-buffer)
  (phase-5-verify-collateral)
  (phase-6-verify-correction-in-warnings-buffer)

  (message "✓ PASS: All phases completed successfully!")
  (setq integration-test-completed t)
  (kill-emacs 0))

(progn ;; Run the test with a timeout in case things hang.
  (run-at-time
   20 nil
   (lambda ()
     (message "TIMEOUT: Integration test timed out!")
     (message "Last phase: %s" integration-test-phase)
     (kill-emacs 1)))
  ( ;; the test
   run-all-tests))
