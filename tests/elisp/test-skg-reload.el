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

(ert-deftest test-skg-reload-conflict-preserves-local-and-opens-structured-report ()
  (let ((dirty (generate-new-buffer "*skg dirty conflict test*"))
        (clean (generate-new-buffer "*skg clean conflict test*"))
        shown)
    (unwind-protect
        (progn
          (with-current-buffer dirty
            (setq-local skg-view-uri "dirty-uri")
            (setq-local skg--last-rendered-content "base text")
            (insert "local text")
            (set-buffer-modified-p t))
          (with-current-buffer clean
            (setq-local skg-view-uri "clean-uri")
            (insert "updated text")
            (set-buffer-modified-p nil))
          (cl-letf (((symbol-function 'skg-big-nonfatal-message)
                     (lambda (name message content)
                       (setq shown (list name message content)))))
            (skg--handle-reload-conflicts
             '((incident-id incident-one)
               (conflicted-views
                (((view-uri dirty-uri) (pids (node-a))
                  (paths ("/source/node-a.skg"))
                  (incoming "incoming text"))))
               (updated-views
                (((view-uri clean-uri) (pids (node-a))
                  (paths ("/source/node-a.skg"))))
               (files-affected ("/source/node-a.skg")))))
          (with-current-buffer dirty
            (should (equal (buffer-string) "local text"))
            (should (buffer-modified-p))
            (should (equal (alist-get 'base skg--disk-client-conflict)
                           "base text"))
            (should (equal (alist-get 'incoming skg--disk-client-conflict)
                           "incoming text"))
            (should-error (skg-request-save-buffer) :type 'user-error))
          (should (equal (car shown) "*SKG Disk-Client Conflicts*"))
          (should (string-prefix-p
                   "* WARNING: Disk-client conflict(s)" (caddr shown)))
          (should (string-match-p
                   "^\\*\\* buffers that have been updated$" (caddr shown)))
          (should-not (string-match-p "^  \\*" (caddr shown))))
      (dolist (buffer (list dirty clean))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))))))

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

(ert-deftest test-skg-reload-request-locks-and-reports-dirty-uri ()
  (let ((view (generate-new-buffer "*skg dirty request test*"))
        (skg--active-request-id nil)
        (skg--stream-in-progress nil)
        submitted)
    (unwind-protect
        (progn
          (with-current-buffer view
            (setq-local skg-view-uri "dirty-request-uri")
            (insert "local")
            (set-buffer-modified-p t))
          (cl-letf (((symbol-function 'skg-tcp-connect-to-rust)
                     (lambda () 'fake-process))
                    ((symbol-function 'skg-submit-request)
                     (lambda (_tcp request &optional _content incident)
                       (setq submitted (list (read request) incident)))))
            (skg-reload-paths '("/source/node.skg") nil "same-incident"))
          (should (equal (cadr submitted) "same-incident"))
          (should (equal
                   (cadr (assoc 'dirty-view-uris (car submitted)))
                   "dirty-request-uri"))
          (with-current-buffer view
            (should skg--save-lock-overlay)))
      (skg--end-stream)
      (skg--unlock-all-save-locked)
      (setq skg--request-draft nil)
      (with-current-buffer view (set-buffer-modified-p nil))
      (kill-buffer view))))

(ert-deftest test-skg-recovery-disk-race-starts-a-successor-incident ()
  "Changed recovery bytes stay unresolved and trigger a fresh exact sweep."
  (let ((skg--pending-recovery-incidents
         '(((incident-id old-incident))))
        (skg--reload-observation-incident-id "old-incident")
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
      (should-not (equal skg--reload-observation-incident-id
                         "old-incident"))
      (should (string-prefix-p
               "incident-" skg--reload-observation-incident-id))
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

(ert-deftest test-skg-reload-path-scan-uses-only-direct-regular-files ()
  "Nested .skg files and .skg-named directories are outside a source."
  (let* ((root (make-temp-file "skg-reload-paths" t))
         (source (expand-file-name "owned/source" root))
         (nested (expand-file-name "nested" source))
         (config (expand-file-name "skgconfig.toml" root))
         (skg-config-dir (file-name-as-directory root)))
    (unwind-protect
        (progn
          (make-directory nested t)
          (make-directory (expand-file-name "directory.skg" source))
          (with-temp-file config
            (insert "[[sources]]\npath = \"owned/source\"\n"))
          (with-temp-file (expand-file-name "direct.skg" source)
            (insert "pid: direct\n"))
          (with-temp-file (expand-file-name "nested.skg" nested)
            (insert "pid: nested\n"))
          (should (equal (skg--reload-all-skg-files)
                         (list (expand-file-name "direct.skg" source)))))
      (delete-directory root t))))

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

(ert-deftest test-skg-reload-observation-callback-only-enqueues-candidates ()
  "The notification callback does no content read, stat or hash."
  (let ((skg--server-source-inventory
         '((:name "s" :directory "/data/source")))
        (skg--reload-observation-paths (make-hash-table :test 'equal))
        (skg--reload-observation-sequence 0)
        skg--reload-observation-incident-id
        scheduled)
    (cl-letf (((symbol-function 'skg--schedule-reload-observation)
               (lambda (delay) (setq scheduled delay)))
              ((symbol-function 'file-attributes)
               (lambda (&rest _) (ert-fail "callback statted a file")))
              ((symbol-function 'insert-file-contents)
               (lambda (&rest _) (ert-fail "callback read a file"))))
      (skg--reload-file-notify-callback
       '(watch changed "/data/source/node.skg"))
      (should (= (hash-table-count skg--reload-observation-paths) 1))
      (should (= scheduled 0.35))
      (should (string-prefix-p
               "incident-" skg--reload-observation-incident-id))
      (skg--reload-file-notify-callback
       '(watch changed "/data/source/nested/node.skg"))
      (should (= (hash-table-count skg--reload-observation-paths) 1)))))

(ert-deftest test-skg-reload-observation-reuses-live-watches ()
  "Hot client reload must not turn live watches into an expensive sweep."
  (let ((skg--reload-watch-descriptors '(watch-a watch-b))
        stopped added swept)
    (cl-letf (((symbol-function 'file-notify-valid-p) (lambda (_) t))
              ((symbol-function 'skg-stop-reload-observation)
               (lambda () (setq stopped t)))
              ((symbol-function 'file-notify-add-watch)
               (lambda (&rest _) (setq added t)))
              ((symbol-function 'skg--request-reload-full-sweep)
               (lambda () (setq swept t))))
      (skg-start-reload-observation)
      (should-not stopped)
      (should-not added)
      (should-not swept))))

(ert-deftest test-skg-reload-observation-replaces-stale-watches-and-sweeps ()
  "A stopped watcher leaves a gap that only an exact sweep can cover."
  (let ((skg--server-source-inventory nil)
        (skg--reload-watch-descriptors '(stale-watch))
        stopped swept)
    (cl-letf (((symbol-function 'file-notify-valid-p) (lambda (_) nil))
              ((symbol-function 'skg-stop-reload-observation)
               (lambda ()
                 (setq stopped t
                       skg--reload-watch-descriptors nil)))
              ((symbol-function 'skg--source-paths) (lambda () nil))
              ((symbol-function 'skg--request-reload-full-sweep)
               (lambda () (setq swept t))))
      (skg-start-reload-observation)
      (should stopped)
      (should swept))))

(ert-deftest test-skg-reload-observation-deferred-retains_same_incident ()
  (let ((skg--reload-observation-paths (make-hash-table :test 'equal))
        (skg--reload-observation-in-flight t)
        skg--reload-observation-timer
        skg--reload-observation-incident-id
        scheduled)
    (puthash "/s/a.skg" 7 skg--reload-observation-paths)
    (cl-letf (((symbol-function 'skg--schedule-reload-observation)
               (lambda (delay) (setq scheduled delay))))
      (skg--finish-reload-observation
       '(("/s/a.skg" . 7)) nil "incident-original"
       '((deferred true) (terminal-status complete)))
      (should (equal (gethash "/s/a.skg" skg--reload-observation-paths) 7))
      (should (equal skg--reload-observation-incident-id
                     "incident-original"))
      (should (= scheduled 1.0)))))

(ert-deftest test-skg-reload-observation-does-not-erase_a_newer_event ()
  (let ((skg--reload-observation-paths (make-hash-table :test 'equal))
        (skg--reload-observation-in-flight t)
        (skg--reload-observation-full-sweep nil)
        skg--reload-observation-timer
        skg--reload-observation-incident-id
        scheduled)
    ;; Sequence 7 was sent; sequence 8 arrived while it was in flight.
    (puthash "/s/a.skg" 8 skg--reload-observation-paths)
    (cl-letf (((symbol-function 'skg--schedule-reload-observation)
               (lambda (delay) (setq scheduled delay))))
      (skg--finish-reload-observation
       '(("/s/a.skg" . 7)) nil "incident-old"
       '((terminal-status complete)))
      (should (equal (gethash "/s/a.skg" skg--reload-observation-paths) 8))
      (should (string-prefix-p
               "incident-" skg--reload-observation-incident-id))
      (should (= scheduled 0)))))

(provide 'test-skg-reload)
