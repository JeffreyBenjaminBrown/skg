;;; Two-user fork/pull/subscribe-back/pull integration driver. -*- lexical-binding: t; -*-

(load-file (expand-file-name "elisp/skg-init.el" (getenv "SKG_PROJECT_ROOT")))
(load-file (expand-file-name "tests/integration/test-wait.el"
                             (getenv "SKG_PROJECT_ROOT")))

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defconst happy-path--unlock-limit-seconds 4.0)
(defvar happy-path--warning-fingerprints (make-hash-table :test #'equal))

(defun happy-path--append-result (kind scope message)
  (with-temp-buffer
    (insert (format "%s\t%s\t%s\n" kind scope
                    (replace-regexp-in-string "[\n\r]+" " | " message)))
    (write-region (point-min) (point-max)
                  (getenv "SKG_TEST_FAILURES") t 'silent)))

(defun happy-path-failure (scope format-string &rest arguments)
  (let ((message-text (apply #'format format-string arguments)))
    (message "FAIL [%s]: %s" scope message-text)
    (happy-path--append-result "FAIL" scope message-text)
    nil))

(defun happy-path-fatal (scope format-string &rest arguments)
  (apply #'happy-path-failure scope format-string arguments)
  (kill-emacs 2))

(defun happy-path-check (condition scope format-string &rest arguments)
  (if condition
      (progn
        (message "ok [%s]: %s" scope
                 (apply #'format format-string arguments))
        condition)
    (apply #'happy-path-failure scope format-string arguments)))

(defun happy-path-warning (scope message-text)
  (let ((fingerprint (format "%s\0%s" scope message-text)))
    (unless (gethash fingerprint happy-path--warning-fingerprints)
      (puthash fingerprint t happy-path--warning-fingerprints)
      (happy-path-failure scope "warning returned: %s" message-text))))

;; Capture warnings at the shared response-formatting and presentation
;; boundaries.  The explicit handshake hook covers structured telescope load
;; warnings, whose UI intentionally does not call `display-warning'.
(defun happy-path--around-errors-and-warnings
    (original errors warnings)
  (dolist (warning warnings)
    (happy-path-warning "server-response" (format "%s" warning)))
  (funcall original errors warnings))

(defun happy-path--around-display-warning
    (original type message-text &optional level buffer-name)
  (happy-path-warning
   "display-warning"
   (format "%s/%s: %s" type (or level :warning) message-text))
  (funcall original type message-text level buffer-name))

(defun happy-path--around-handshake-warnings (original response)
  (dolist (warning (or (cadr (assoc 'telescope-warnings response)) nil))
    (happy-path-warning
     "startup-telescope"
     (format "%s: %s"
             (cadr (assoc 'pid warning))
             (cadr (assoc 'message warning)))))
  (funcall original response))

(defun happy-path--around-message (original format-string &rest arguments)
  ;; Some protocol failures are deliberately non-signaling so an incident can
  ;; continue to a safe terminal state.  They must still fail this aggregate
  ;; integration test instead of disappearing into the batch *Messages* log.
  (let ((rendered
         (condition-case nil
             (if arguments
                 (apply #'format format-string arguments)
               (format "%s" format-string))
           (error (format "%S %S" format-string arguments)))))
    (cond
     ((string-prefix-p "SKG request failed:" rendered)
      (happy-path-failure "protocol-response" "%s" rendered))
     ((and (string-match-p "\\`SKG\\b" rendered)
           (string-match-p "\\bwarning\\b" (downcase rendered)))
      (happy-path-warning "message" rendered)))
    (apply original format-string arguments)))

(advice-add 'skg-errors-and-warnings-to-org-string
            :around #'happy-path--around-errors-and-warnings)
(advice-add 'display-warning :around #'happy-path--around-display-warning)
(advice-add 'skg--show-handshake-telescope-warnings
            :around #'happy-path--around-handshake-warnings)
(advice-add 'message :around #'happy-path--around-message)

(defun happy-path--root-buffer (id)
  (seq-find
   (lambda (buffer)
     (and (buffer-live-p buffer)
          (with-current-buffer buffer
            (and (derived-mode-p 'skg-content-view-mode)
                 (save-excursion
                   (goto-char (point-min))
                   (looking-at-p
                    (format "^\\* .*[(]id %s[)]"
                            (regexp-quote id))))))))
   (buffer-list)))

(defun happy-path--request-fresh-view (id &optional bypass-override)
  (skg-request-single-root-content-view-from-id
   id nil bypass-override nil nil nil t)
  (or (skg-test-wait-for
       (lambda () (happy-path--root-buffer id)) 20)
      (happy-path-fatal
       "view" "no fresh content view appeared for id %s" id)))

(defun happy-path--buffer-text (buffer)
  (with-current-buffer buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defun happy-path--wait-for-response (scope)
  (or (skg-test-wait-for-response 30)
      (happy-path-fatal scope "server response did not settle within 30 seconds")))

(defun happy-path--confirmation-buffer ()
  (seq-find
   (lambda (buffer)
     (with-current-buffer buffer
       (and skg--buffer-record
            (eq (skg--buffer-record-kind skg--buffer-record)
                'fork-confirmation))))
   (skg-registered-buffers)))

(defun happy-path--skg-files (directory)
  (directory-files directory t "\\.skg\\'"))

(defun happy-path--pid-from-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (re-search-forward "^pid:[ \t]*\\(.+\\)$" nil t)
      (string-trim (match-string 1)))))

(defun happy-path--write-clone-id (clone-id)
  (with-temp-file
      (expand-file-name "economics-of-china.id"
                        (getenv "SKG_TEST_WORK_ROOT"))
    (insert clone-id "\n")))

(defun happy-path--read-clone-id ()
  (let ((file (expand-file-name "economics-of-china.id"
                                (getenv "SKG_TEST_WORK_ROOT"))))
    (unless (file-readable-p file)
      (happy-path-fatal "fixture" "clone-id handoff file is absent: %s" file))
    (string-trim
     (with-temp-buffer
       (insert-file-contents file)
       (buffer-string)))))

(defun happy-path--git (scope repository &rest arguments)
  (with-temp-buffer
    (let ((status (apply #'call-process
                         "git" nil t nil "-C" repository arguments)))
      (unless (and (integerp status) (= status 0))
        (happy-path-failure
         scope "git %s exited %S: %s"
         (string-join arguments " ") status (string-trim (buffer-string))))
      (and (integerp status) (= status 0)))))

(defun happy-path--commit-and-push (scope repository message-text)
  (happy-path--git scope repository "add" "-A")
  (happy-path--git scope repository "commit" "-m" message-text)
  (happy-path--git scope repository "push"))

(defun happy-path--buffer-locked-p (buffer)
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and skg--maintenance-lock-overlay
              (overlayp skg--maintenance-lock-overlay)
              (overlay-buffer skg--maintenance-lock-overlay)))))

(defun happy-path--record-timing (label elapsed)
  (message "TIMING: %s pull-to-unlock %.3f seconds" label elapsed)
  (with-temp-buffer
    (insert (format "%s\t%.3f seconds\n" label elapsed))
    (write-region (point-min) (point-max)
                  (getenv "SKG_TEST_TIMINGS") t 'silent)))

(defun happy-path--pull-and-measure-unlock (label buffer)
  "Pull all repositories and return seconds until BUFFER is unlocked.
The clock starts immediately before `skg-pull-all'.  The test requires seeing
the maintenance lock first, records the first subsequent unlocked instant,
then waits for the incident to finish before returning."
  (let ((started (float-time))
        (deadline (+ (float-time) 100))
        saw-lock
        unlocked-at)
    (condition-case error-data
        (skg-pull-all)
      (error
       (happy-path-failure
        label "pull could not start: %s" (error-message-string error-data))))
    (while (and (< (float-time) deadline)
                (or (not unlocked-at) skg--maintenance-client-incident))
      (when (happy-path--buffer-locked-p buffer)
        (setq saw-lock t))
      (when (and saw-lock
                 (not unlocked-at)
                 (not (happy-path--buffer-locked-p buffer)))
        (setq unlocked-at (float-time)))
      (accept-process-output nil 0.02))
    (unless saw-lock
      (happy-path-failure label "the target buffer was never observed locked"))
    (unless unlocked-at
      (happy-path-failure label "the target buffer was not unlocked within 100 seconds")
      (setq unlocked-at (float-time)))
    (when skg--maintenance-client-incident
      (happy-path-failure
       label "maintenance was still active after the 100-second deadline"))
    (let ((elapsed (- unlocked-at started)))
      (happy-path--record-timing label elapsed)
      (happy-path-check
       (<= elapsed happy-path--unlock-limit-seconds)
       label
       "pull-to-unlock was %.3f seconds (limit %.1f seconds)"
       elapsed happy-path--unlock-limit-seconds)
      elapsed)))

(defun happy-path--initialize-client ()
  (let ((config (getenv "SKG_TEST_CONFIG")))
    ;; `skg-port' is already bound by skg-state, so skg-client-init's
    ;; compatibility `defvar' does not replace it in a batch test.
    (setq skg-port (string-to-number (getenv "SKG_TEST_PORT")))
    (condition-case error-data
        (skg-client-init config)
      (error
       (happy-path-fatal
        "client-init" "%s" (error-message-string error-data))))
    (happy-path-check
     (eq skg--connection-handshake-state 'verified)
     "client-init" "connection handshake is verified")))

(defun happy-path--economist-first ()
  (let* ((root (getenv "SKG_TEST_WORK_ROOT"))
         (repository
          (expand-file-name "economist-client/owned/economist-public" root))
         (foreign-view
          (happy-path--request-fresh-view "chinese-economics" t)))
    ;; Editing a definitive foreign node is the ordinary implicit-fork gesture.
    (with-current-buffer foreign-view
      (goto-char (point-min))
      (unless (search-forward "Chinese Economics" (line-end-position) t)
        (happy-path-fatal "economist-fork" "foreign title was not present"))
      (replace-match "Economics of China" t t)
      (skg-request-save-buffer))
    (let ((confirmation
           (skg-test-wait-for #'happy-path--confirmation-buffer 20)))
      (unless confirmation
        (happy-path-fatal "economist-fork" "fork confirmation did not appear"))
      (with-current-buffer confirmation
        (goto-char (point-min))
        (unless (re-search-forward "^\\* (skg (node (source " nil t)
          (happy-path-fatal
           "economist-fork" "confirmation had no clone headline: %s"
           (buffer-string)))
        (beginning-of-line)
        (skg--change-source-at-point "economist-public")
        (skg-approve-fork)))
    (happy-path--wait-for-response "economist-fork")

    (let* ((files (cl-remove-if
                   (lambda (file)
                     (string= (file-name-nondirectory file) "economics.skg"))
                   (happy-path--skg-files repository)))
           (clone-file
            (or (skg-test-wait-for
                 (lambda ()
                   (let ((found
                          (cl-remove-if
                           (lambda (file)
                             (string= (file-name-nondirectory file)
                                      "economics.skg"))
                           (happy-path--skg-files repository))))
                     (and (= (length found) 1) (car found))))
                 20)
                (happy-path-fatal
                 "economist-fork" "expected one new clone file; initially saw %S"
                 files)))
           (clone-id (happy-path--pid-from-file clone-file)))
      (unless clone-id
        (happy-path-fatal
         "economist-fork" "could not read pid from clone file %s" clone-file))
      (happy-path--write-clone-id clone-id)
      (happy-path-check
       (with-temp-buffer
         (insert-file-contents clone-file)
         (and (search-forward "title: Economics of China" nil t)
              (search-forward "subscribes_to:" nil t)
              (search-forward "overrides_view_of:" nil t)))
       "economist-fork"
       "clone %s has its new title, subscription, and override" clone-id)

      ;; Economics starts empty, so this new membership is both first and only.
      (let ((economics-view (happy-path--request-fresh-view "economics")))
        (with-current-buffer economics-view
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert (format
                   "** (skg (node (id %s) (source economist-public) indef)) Economics of China\n"
                   clone-id))
          (skg-request-save-buffer))
        (happy-path--wait-for-response "economist-contains")
        (happy-path-check
         (string-match-p (regexp-quote (format "(id %s)" clone-id))
                         (happy-path--buffer-text economics-view))
         "economist-contains"
         "Economics contains clone %s as its first content node" clone-id))

      (happy-path--commit-and-push
       "economist-push" repository
       "Fork Chinese Economics into Economics"))))

(defun happy-path--insert-subscription (buffer clone-id)
  (with-current-buffer buffer
    (goto-char (point-min))
    (unless (re-search-forward "^\\*\\* (skg subscribeeCol)[ \t]*$" nil t)
      (happy-path-fatal
       "china-scholar-subscribe" "no writable subscribeeCol appeared: %s"
       (buffer-string)))
    (forward-line 1)
    (insert (format
             "*** (skg (node (id %s) (source economist-public) indef)) Economics of China\n"
             clone-id))
    (skg-request-save-buffer)))

(defun happy-path--china-scholar ()
  (let* ((root (getenv "SKG_TEST_WORK_ROOT"))
         (repository
          (expand-file-name
           "china-scholar-client/owned/china-scholar-public" root))
         (clone-id (happy-path--read-clone-id))
         (china-view (happy-path--request-fresh-view "china")))
    (happy-path-check
     (string-match-p "(id chinese-economics)"
                     (happy-path--buffer-text china-view))
     "china-scholar-before-pull"
     "China initially contains China-Scholar's Chinese Economics")

    (happy-path--pull-and-measure-unlock "china-scholar-pull" china-view)
    (happy-path-check
     (buffer-live-p china-view)
     "china-scholar-pull" "the original China buffer remains live")

    ;; Bypass override selection: this is explicitly China-Scholar's original,
    ;; whose inbound graph stats should now name Economist's subscriber.
    (let ((original-view
           (happy-path--request-fresh-view "chinese-economics" t)))
      (happy-path-check
       (string-match-p "(subscribes (in 1" (happy-path--buffer-text original-view))
       "china-scholar-observes-subscriber"
       "Chinese Economics reports an inbound subscriber")
      (with-current-buffer original-view
        (goto-char (point-min))
        (skg-show-collection-subscribes))
      (happy-path--wait-for-response "china-scholar-show-subscriptions")
      (happy-path-check
       (and (string-match-p "subscriberCol" (happy-path--buffer-text original-view))
            (string-match-p (regexp-quote (format "(id %s)" clone-id))
                            (happy-path--buffer-text original-view)))
       "china-scholar-observes-subscriber"
       "the subscriber collection names clone %s" clone-id)
      (happy-path--insert-subscription original-view clone-id)
      (happy-path--wait-for-response "china-scholar-subscribe")
      (happy-path-check
       (with-temp-buffer
         (insert-file-contents
          (expand-file-name "chinese-economics.skg" repository))
         (and (search-forward "subscribes_to:" nil t)
              (search-forward clone-id nil t)))
       "china-scholar-subscribe"
       "Chinese Economics now subscribes back to clone %s" clone-id))

    (happy-path--commit-and-push
     "china-scholar-push" repository
     "Subscribe Chinese Economics back to Economist's fork")))

(defun happy-path--first-content-id (buffer)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward
             "^\\*\\* (skg (node (id \\([^ )]+\\)" nil t)
        (match-string-no-properties 1)))))

(defun happy-path--economist-final ()
  (let* ((clone-id (happy-path--read-clone-id))
         (economics-view (happy-path--request-fresh-view "economics")))
    (happy-path-check
     (equal (happy-path--first-content-id economics-view) clone-id)
     "economist-before-pull"
     "Economics has clone %s as its first content node" clone-id)

    (happy-path--pull-and-measure-unlock "economist-pull" economics-view)
    (happy-path-check
     (buffer-live-p economics-view)
     "economist-pull" "the original Economics buffer remains live")

    (let ((clone-view (happy-path--request-fresh-view clone-id)))
      (happy-path-check
       (string-match-p "Economics of China" (happy-path--buffer-text clone-view))
       "economist-final-view" "the fresh view has the fork's title")
      (happy-path-check
       (string-match-p "(subscribes (in 1" (happy-path--buffer-text clone-view))
       "economist-final-view"
       "Economics of China reports China-Scholar's inbound subscription"))))

(defun happy-path-main ()
  (run-at-time
   170 nil
   (lambda ()
     (happy-path-fatal "phase-timeout" "Emacs phase exceeded 170 seconds")))
  (happy-path--initialize-client)
  (pcase (getenv "SKG_TEST_PHASE")
    ("economist-first" (happy-path--economist-first))
    ("china-scholar" (happy-path--china-scholar))
    ("economist-final" (happy-path--economist-final))
    (phase (happy-path-fatal "runner" "unknown phase %S" phase)))
  (message "Phase complete: %s" (getenv "SKG_TEST_PHASE"))
  (kill-emacs 0))

(happy-path-main)
