(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'cl-lib)
(require 'heralds-minor-mode)
(require 'skg-reload)
(require 'skg-request-reload-paths)
(require 'skg-worktree-guard)

(ert-deftest test-skg-git-worktree-classifier-allows-index-and-ref-operations ()
  (dolist (args '(("status")
                  ("diff" "--cached")
                  ("add" "--" "node.skg")
                  ("reset" "HEAD" "--" "node.skg")
                  ("restore" "--staged" "--" "node.skg")
                  ("rm" "--cached" "--" "node.skg")
                  ("commit" "-m" "message")))
    (should (skg--git-command-preserves-worktree-p args))))

(ert-deftest test-skg-git-worktree-classifier-refuses-writers-and-unknowns ()
  (dolist (args '(("checkout" "other")
                  ("switch" "other")
                  ("reset" "--hard" "HEAD")
                  ("restore" "node.skg")
                  ("restore" "--staged" "--worktree" "node.skg")
                  ("rm" "node.skg")
                  ("apply" "change.patch")
                  ("stash" "pop")
                  ("pull")
                  ("merge" "other")
                  ("rebase" "main")
                  ("new-future-command")))
    (should-not (skg--git-command-preserves-worktree-p args))))

(ert-deftest test-skg-magit-guards-are-installed-at-both-process-seams ()
  (require 'magit-process)
  (should (advice-member-p #'skg--guard-magit-call-git 'magit-call-git))
  (should (advice-member-p #'skg--guard-magit-start-git 'magit-start-git)))

(ert-deftest test-skg-magit-guard-refuses-only-when-a-dirty-view-is-at-risk ()
  (let ((view (generate-new-buffer "*skg dirty guard test*")))
    (unwind-protect
        (progn
          (with-current-buffer view
            (setq-local skg-view-uri "dirty-view")
            (insert "dirty")
            (set-buffer-modified-p t))
          (cl-letf (((symbol-function 'skg--magit-repository-contains-source-p)
                     (lambda () t)))
            (should-error (skg--guard-magit-git-args '("checkout" "other"))
                          :type 'user-error)
            (should-not (skg--guard-magit-git-args '("add" "node.skg"))))
          (cl-letf (((symbol-function 'skg--magit-repository-contains-source-p)
                     (lambda () nil)))
            (should-not (skg--guard-magit-git-args '("checkout" "other")))))
      (with-current-buffer view (set-buffer-modified-p nil))
      (kill-buffer view))))

(ert-deftest test-skg-raw-file-guard-names-dirty-views ()
  (let ((view (generate-new-buffer "*skg dirty raw guard test*")))
    (unwind-protect
        (with-current-buffer view
          (setq-local skg-view-uri "dirty-view")
          (insert "dirty")
          (set-buffer-modified-p t)
          (let ((message (condition-case err
                             (progn
                               (skg--refuse-worktree-write-if-views-dirty
                                "Raw .skg save")
                               nil)
                           (user-error (error-message-string err)))))
            (should (string-match-p "Raw \\.skg save refused" message))
            (should (string-match-p (regexp-quote (buffer-name view)) message))))
      (with-current-buffer view (set-buffer-modified-p nil))
      (kill-buffer view))))

(ert-deftest test-skg-streamed-update-never-overwrites-a-newly-dirty-buffer ()
  (let ((view (generate-new-buffer "*skg late dirty test*")))
    (unwind-protect
        (with-current-buffer view
          (setq-local skg-view-uri "late-dirty-uri")
          (insert "local survives")
          (set-buffer-modified-p t)
          (skg--apply-streamed-view-update
           "((view-uri late-dirty-uri) (content \"incoming\"))"
           'reload "test")
          (should (equal (buffer-string) "local survives"))
          (should (buffer-modified-p))
          (should skg--disk-client-conflict))
      (with-current-buffer view (set-buffer-modified-p nil))
      (kill-buffer view))))

(ert-deftest test-skg-reload-paths-enters-maintenance ()
  (let (submitted)
    (cl-letf (((symbol-function 'skg--confirm-explicit-reload-with-dirty-views)
               (lambda () t))
              ((symbol-function 'skg-begin-maintenance)
               (lambda (&rest arguments) (setq submitted arguments))))
      (skg-reload-paths '("/source/node.skg") '("node-id")))
    (should (equal (car submitted) "explicit-partial-reload"))
    (should (equal (nth 2 submitted) '("/source/node.skg")))
    (should (equal (nth 3 submitted) '("node-id")))))

(ert-deftest test-skg-recovery-disk-race-starts-a-successor-incident ()
  "Changed recovery bytes stay unresolved and trigger a fresh exact sweep."
  (let ((skg--pending-recovery-incidents
         '(((incident-id old-incident))))
        handler
        submitted-incident
        (sweeps 0))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'skg-tcp-connect-to-rust)
               (lambda () 'fake-process))
              ((symbol-function 'skg-register-response-handler)
               (lambda (_type callback &optional _terminal)
                 (setq handler callback)))
              ((symbol-function 'skg-submit-request)
               (lambda (_tcp _request &optional _content incident)
                 (setq submitted-incident incident)))
              ((symbol-function 'skg-big-nonfatal-message)
               (lambda (&rest _)))
              ((symbol-function 'skg--request-reload-full-sweep)
               (lambda () (setq sweeps (1+ sweeps)))))
      (skg-recover-reload-incident "old-incident")
      (should (equal submitted-incident "old-incident"))
      (funcall handler nil
               "((terminal-status failed)\
                  (content \"disk changed\")\
                  (successor-required true))")
      (should (= sweeps 1))
      (should skg--pending-recovery-incidents))))

(ert-deftest test-skg-reload-preserves-herald-rules-on-load-error ()
  "A load error mid-reload must NOT strip the herald rule table.

Regression guard for a volatile-session-state bug: the herald rule
table is fetched once per connect (`skg-request-herald-rules') and has
no on-disk source.  `skg-reload' unloads `heralds-minor-mode' -- which
wipes the table -- and is supposed to re-install the captured copy
afterward.  When the re-install was a plain sequential step, any error
in a reloaded file (a stray edit-in-progress, say) aborted `skg-reload'
before that step, leaving the table nil for the rest of the session;
every `heralds-minor-mode' toggle then reported \"no herald rule table
from the server\" even though the server was healthy.

The fix wraps the re-install in `unwind-protect'.  Here we stub the
destructive `skg--reload-modules' so the real reload machinery never
runs (no feature is actually unloaded); the stub mimics the failure by
wiping the table and signalling.  `skg-reload' must still surface the
error AND leave the captured table installed."
  (let ((heralds--transform-rules '(skg test-sentinel)))
    (cl-letf (((symbol-function 'skg--reload-modules)
               (lambda (&rest _)
                 (setq heralds--transform-rules nil)
                 (error "simulated load error during reload"))))
      (should-error (skg-reload))
      (should (equal heralds--transform-rules '(skg test-sentinel))))))

(ert-deftest test-skg-reload-refreshes-detached-recovery-commands ()
  (should (member "skg-recovery-ui.el"
                  skg--reload-by-evaluation-files))
  (should (< (cl-position "skg-recovery-archive.el"
                          skg--reload-by-evaluation-files :test #'equal)
             (cl-position "skg-recovery-ui.el"
                          skg--reload-by-evaluation-files :test #'equal)))
  (should (< (cl-position "skg-recovery-ui.el"
                          skg--reload-by-evaluation-files :test #'equal)
             (cl-position "skg-maintenance.el"
                          skg--reload-by-evaluation-files :test #'equal))))

(ert-deftest test-skg-reload-selection-is-a-distinct-id-stack-entry-path ()
  "Only the explicit reload command installs TO-RELOAD selection state."
  (let ((skg-id-stack '(("id-a" "Alpha") ("id-b" "Beta"))))
    (unwind-protect
        (save-window-excursion
          (skg-view-id-stack)
          (should-not skg-reload-selection-mode)
          (should-not (member "TO-RELOAD" org-todo-keywords-1))
          (skg-reload-from-id-stack)
          (should skg-reload-selection-mode)
          (should (member "TO-RELOAD" org-todo-keywords-1))
          (should (equal (mapcar #'cdr skg--reload-selection-entries)
                         '("id-a" "id-b")))
          (goto-char (point-min))
          (org-shiftright)
          (should (equal (org-get-todo-state) "TO-RELOAD"))
          (org-shiftleft)
          (should-not (org-get-todo-state)))
      (dolist (name '("*skg-id-stack*" "*skg-reload-from-id-stack*"))
        (when-let ((buffer (get-buffer name)))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(ert-deftest test-skg-reload-selection-submits-deduplicated-marked-ids ()
  (let ((skg-id-stack '(("same" "First") ("same" "Second")
                        ("other" "Other")))
        submitted)
    (unwind-protect
        (save-window-excursion
          (skg-reload-from-id-stack)
          (dolist (entry (butlast skg--reload-selection-entries))
            (goto-char (marker-position (car entry)))
            (org-todo "TO-RELOAD"))
          (cl-letf (((symbol-function 'skg--dirty-view-buffers)
                     (lambda () nil))
                    ((symbol-function 'skg-begin-maintenance)
                     (lambda (&rest arguments)
                       (setq submitted arguments))))
            (skg--submit-reload-selection))
          (should (equal (car submitted) "explicit-partial-reload"))
          (should-not (cadr submitted))
          (should-not (nth 2 submitted))
          (should (equal (nth 3 submitted) '("same")))
          (should (functionp (nth 4 submitted)))
          (dolist (entry (butlast skg--reload-selection-entries))
            (goto-char (marker-position (car entry)))
            (should (equal (org-get-todo-state) "TO-RELOAD")))
          (funcall
           (nth 4 submitted)
           '((requested-id-outcomes
              (((requested-id same) (pid same) (status acknowledged)
                (reason nil) (paths ("/source/same.skg")))))))
          (dolist (entry (butlast skg--reload-selection-entries))
            (goto-char (marker-position (car entry)))
            (should-not (org-get-todo-state))))
      (when-let ((buffer (get-buffer "*skg-reload-from-id-stack*")))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(ert-deftest test-skg-reload-selection-clears-only-acknowledged-marks ()
  (let ((skg-id-stack '(("good" "Good") ("bad" "Bad"))))
    (unwind-protect
        (save-window-excursion
          (skg-reload-from-id-stack)
          (dolist (entry skg--reload-selection-entries)
            (goto-char (marker-position (car entry)))
            (org-todo "TO-RELOAD"))
          (skg--apply-reload-selection-result
           '((requested-id-outcomes
              (((requested-id good) (pid good) (status acknowledged)
                (reason nil) (paths ("/a/good.skg")))
               ((requested-id bad) (pid nil) (status rejected)
                (reason "not found") (paths ()))))))
          (goto-char (marker-position
                      (car (car skg--reload-selection-entries))))
          (should-not (org-get-todo-state))
          (goto-char (marker-position
                      (car (cadr skg--reload-selection-entries))))
          (should (equal (org-get-todo-state) "TO-RELOAD"))
          (should (= (length skg--reload-selection-reason-overlays) 1))
          (should (string-match-p
                   "not found"
                   (overlay-get (car skg--reload-selection-reason-overlays)
                                'after-string))))
      (when-let ((buffer (get-buffer "*skg-reload-from-id-stack*")))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(ert-deftest test-skg-explicit-reload-asks-once-before-dirty-view-archive ()
  (let (prompt)
    (cl-letf (((symbol-function 'skg--dirty-view-buffers)
               (lambda () '(one two)))
              ((symbol-function 'yes-or-no-p)
               (lambda (text) (setq prompt text) nil)))
      (should-not (skg--confirm-explicit-reload-with-dirty-views)))
    (should (string-match-p "Archive 2 dirty Skg view" prompt))
    (should (string-match-p "detached recovery" prompt))))

(ert-deftest test-skg-reload-selection-refuses-ordinary-save ()
  (should-error (skg--reload-selection-refuse-save) :type 'user-error))

(ert-deftest test-skg-reload-full-sweep-is-only-an-observation-hint ()
  (let (handler submitted)
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust)
               (lambda () 'fake-process))
              ((symbol-function 'skg-register-response-handler)
               (lambda (type callback terminal)
                 (setq handler (list type callback terminal))))
              ((symbol-function 'skg-submit-request)
               (lambda (_tcp request &rest _)
                 (setq submitted (read request)))))
      (skg--request-reload-full-sweep))
    (should (equal (car handler) 'reload-paths))
    (should (nth 2 handler))
    (should (equal (cdr (assoc 'full-sweep submitted)) "true"))
    (should-not (assoc 'incident-id submitted))
    (funcall (cadr handler) nil
             "((observation-queued true) (terminal-status complete))")))

(provide 'test-skg-reload)
