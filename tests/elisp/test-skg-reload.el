(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'cl-lib)
(require 'heralds-minor-mode)
(require 'skg-reload)
(require 'skg-request-reload-paths)

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
          (cl-letf (((symbol-function 'skg-reload-paths)
                     (lambda (paths ids incident callback)
                       (setq submitted (list paths ids incident callback)))))
            (skg--submit-reload-selection))
          (should-not (car submitted))
          (should (equal (cadr submitted) '("same")))
          (should (string-prefix-p "incident-" (caddr submitted)))
          (should (functionp (cadddr submitted))))
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
