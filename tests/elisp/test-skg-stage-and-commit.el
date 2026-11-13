;;; test-skg-stage-and-commit.el --- Stage-and-commit client tests -*- lexical-binding: t; -*-

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'cl-lib)
(require 'ert)
(require 'org)
(require 'skg-buffer)
(require 'skg-request-stage-and-commit)

(defconst test--stage-and-commit-needs-confirmation
  "((needs-confirmation . t) (readiness-summary (ready 1) (changed 3) (foreign 0) (order-only 0) (notEditable 0)) (errors ()) (warnings ()))")

(defun test--stage-and-commit-run (typed body)
  "Run BODY with the network faked and every `read-string' answered
from TYPED, a list of strings. BODY receives the sent requests (newest
first, via a function), the handlers and the prompts."
  (let ((sent nil) (prompts nil) (handlers (make-hash-table :test 'eq))
        (skg--rerender-after-empty-stream nil))
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust) (lambda () 'fake-proc))
              ((symbol-function 'process-send-string)
               (lambda (_proc text) (push text sent)))
              ((symbol-function 'skg--begin-stream) #'ignore)
              ((symbol-function 'skg--lock-all-skg-buffers) #'ignore)
              ((symbol-function 'skg--register-rerender-stream-handlers) #'ignore)
              ((symbol-function 'skg--wrap-rerender-done-with-after-empty-stream) #'ignore)
              ((symbol-function 'skg-lp-reset) #'ignore)
              ((symbol-function 'skg-register-response-handler)
               (lambda (type handler _one-shot) (puthash type handler handlers)))
              ((symbol-function 'read-string)
               (lambda (prompt &rest _)
                 (push prompt prompts)
                 (pop typed))))
      (funcall body (lambda () sent) handlers (lambda () prompts)))))

(ert-deftest test-stage-and-commit-requires-clean-views-and-a-message ()
  (with-temp-buffer
    (skg-content-view-mode)
    (setq-local skg-view-id "dirty-view")
    (insert "* (skg node (gnid a)) a\n")
    (set-buffer-modified-p t)
    (let ((err (should-error (skg-stage-and-commit) :type 'user-error)))
      (should (string-match-p "Save or revert" (cadr err))))
    (set-buffer-modified-p nil)
    (test--stage-and-commit-run
     '("  ")
     (lambda (sent _handlers _prompts)
       (should-error (skg-stage-and-commit) :type 'user-error)
       (should (null (funcall sent)))))))

(ert-deftest test-stage-and-commit-resends-confirmed-only-on-yes ()
  (test--stage-and-commit-run
   '("the message" "yes")
   (lambda (sent handlers prompts)
     (skg-stage-and-commit)
     (should (string-match-p (regexp-quote "(request . \"stage and commit\")")
                             (car (funcall sent))))
     (should (string-match-p (regexp-quote "(message-lines \"the message\")")
                             (car (funcall sent))))
     (should-not (string-match-p "confirmed" (car (funcall sent))))
     (funcall (gethash 'stage-and-commit handlers) 'fake-proc
              test--stage-and-commit-needs-confirmation)
     (should (= 1 (length (funcall prompts))))
     (should skg--rerender-after-empty-stream)
     (funcall skg--rerender-after-empty-stream)
     (should (string-match-p "1 of 3 changed files are ready"
                             (car (funcall prompts))))
     (should (= 2 (length (funcall sent))))
     (should (string-match-p (regexp-quote "(confirmed . t)")
                             (car (funcall sent))))
     (should (string-match-p (regexp-quote "(message-lines \"the message\")")
                             (car (funcall sent))))))
  (test--stage-and-commit-run
   '("the message" "y")
   (lambda (sent handlers _prompts)
     (skg-stage-and-commit)
     (funcall (gethash 'stage-and-commit handlers) 'fake-proc
              test--stage-and-commit-needs-confirmation)
     (funcall skg--rerender-after-empty-stream)
     (should (= 1 (length (funcall sent)))))))

(ert-deftest test-stage-and-commit-sends-a-message-of-several-lines-on-one-line ()
  "Each line of the message is its own string, so the request, which
the server reads up to a newline, holds no newline but its last."
  (test--stage-and-commit-run
   nil
   (lambda (sent _handlers _prompts)
     (skg--stage-and-commit-send "summary\n\n  indented \\n \"quoted\"" nil)
     (let ((request (car (funcall sent))))
       (should (string-match-p
                (regexp-quote
                 "(message-lines \"summary\" \"\" \"  indented \\\\n \\\"quoted\\\"\")")
                request))
       (should (= 1 (cl-count ?\n request)))
       (should (string-suffix-p "\n" request))))))

(ert-deftest test-stage-and-commit-prompt-names-already-staged-entries ()
  "Entries already staged are counted in the prompt, by gitrepo, and
listed in a buffer of their own, even when every changed file is ready;
\"yes\" resends confirmed."
  (test--stage-and-commit-run
   '("the message" "yes")
   (lambda (sent handlers prompts)
     (skg-stage-and-commit)
     (funcall (gethash 'stage-and-commit handlers) 'fake-proc
              "((needs-confirmation . t) (already-staged ((\"/data/public\" \"notes.txt\" \"a b.skg\") (\"/data/private\" \"c.skg\"))) (readiness-summary (ready 3) (changed 3) (foreign 0) (order-only 0) (notEditable 0)) (errors ()) (warnings ()))")
     (funcall skg--rerender-after-empty-stream)
     (should (equal (car (funcall prompts))
                    "Commit ready. All 3 of 3 changed files are ready. Already staged, so committed too: /data/public: 2 paths, /data/private: 1 path (listed in *skg already staged*). Type yes to commit anyway: "))
     (should (equal (with-current-buffer "*skg already staged*"
                      (buffer-substring-no-properties (point-min) (point-max)))
                    "* /data/public\nnotes.txt\na b.skg\n* /data/private\nc.skg\n"))
     (should (string-match-p (regexp-quote "(confirmed . t)")
                             (car (funcall sent)))))))

(defun test--stage-and-commit-framed (&rest payloads)
  "PAYLOADS, each length-prefixed as the server sends it, in one chunk."
  (apply #'concat
         (mapcar (lambda (payload)
                   (let ((bytes (encode-coding-string payload 'utf-8)))
                     (concat (format "Content-Length: %d\r\n\r\n" (length bytes))
                             bytes)))
                 payloads)))

(defun test--stage-and-commit-through-the-dispatcher (answer)
  "Send a stage-and-commit through the real prologue and dispatcher, the
server answering with the needs-confirmation PAYLOAD below and its empty
rerender stream, all in one chunk; ANSWER, a function of the prompt,
stands for the user at the prompt. Return the state left once the
stream is done and the timers it scheduled have run: a plist of :sent
(the requests, newest first), :messages (newest first), :prompts,
:stream (`skg--stream-in-progress'), :pending (`skg-lp--pending-count')
and :handlers (the response types still registered)."
  (let ((skg-response-handler-map nil)
        (skg-lp--pending-count 0)
        (skg-lp--buf (unibyte-string))
        (skg-lp--bytes-left nil)
        (skg--stream-in-progress nil)
        (skg--rerender-after-empty-stream nil)
        (skg--rerender-overPrivateText-retry nil)
        (skg--rerender-overPrivateText-challenged nil)
        (sent nil) (messages nil) (prompts nil) (scheduled nil))
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust) (lambda () 'fake-proc))
              ((symbol-function 'skg--lock-all-skg-buffers) #'ignore)
              ((symbol-function 'skg--unlock-all-save-locked) #'ignore)
              ((symbol-function 'skg--unlock-buffers-not-in-view-id-list) #'ignore)
              ((symbol-function 'process-send-string)
               (lambda (_proc text) (push text sent)))
              ((symbol-function 'run-at-time)
               (lambda (_delay _repeat fn &rest args)
                 (push (cons fn args) scheduled)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages)))
              ((symbol-function 'read-string)
               (lambda (prompt &rest _)
                 (push prompt prompts)
                 (funcall answer prompt))))
      (skg--stage-and-commit-send "the message" nil)
      (skg-lp-handle-generic-chunk
       'fake-proc
       (test--stage-and-commit-framed
        "((response-type stage-and-commit) (needs-confirmation . t) (already-staged ((\"/data/public\" \"a.skg\" \"b.skg\") (\"/data/private\" \"c.skg\"))) (readiness-summary (ready 3) (changed 3) (foreign 0) (order-only 0) (notEditable 0)) (errors ()) (warnings ()))"
        "((response-type rerender-lock) (lock-views ()))"
        "((response-type rerender-done) (errors ()) (warnings ()))"))
      (let ((stream skg--stream-in-progress)
            (pending skg-lp--pending-count))
        (while scheduled
          (let ((timer (car (last scheduled))))
            (setq scheduled (butlast scheduled))
            (apply (car timer) (cdr timer))))
        (list :sent sent :messages messages :prompts prompts
              :stream stream :pending pending
              :handlers (mapcar #'car skg-response-handler-map))))))

(ert-deftest test-stage-and-commit-a-declined-confirmation-ends-the-stream ()
  "The prompt comes after the empty rerender stream is done, so neither
a word other than \"yes\" nor a quit at the prompt can leave the
stream in progress; either says nothing was committed."
  (dolist (answer (list (lambda (_prompt) "no")
                        (lambda (_prompt) (signal 'quit nil))))
    (let ((state (test--stage-and-commit-through-the-dispatcher answer)))
      (should (null (plist-get state :stream)))
      (should (= 0 (plist-get state :pending)))
      (should (null (plist-get state :handlers)))
      (should (= 1 (length (plist-get state :prompts))))
      (should (= 1 (length (plist-get state :sent))))
      (should (equal (car (plist-get state :messages)) "Not committed.")))))

(ert-deftest test-stage-and-commit-confirmed-after-the-stream-resends ()
  (let ((state (test--stage-and-commit-through-the-dispatcher
                (lambda (_prompt) "yes"))))
    (should (= 2 (length (plist-get state :sent))))
    (should (string-match-p (regexp-quote "(confirmed . t)")
                            (car (plist-get state :sent))))
    (should (string-match-p (regexp-quote "(message-lines \"the message\")")
                            (car (plist-get state :sent))))))

(ert-deftest test-stage-and-commit-result-message ()
  (should (equal (skg--stage-and-commit-result-message
                  (read "((commits ((public 5f135fea1585a3253a5e) (private bff3a57e0670))) (errors ()) (warnings ()))"))
                 "Committed in public (5f135fe), private (bff3a57)."))
  (should (equal (skg--stage-and-commit-result-message
                  (read "((commits ()) (errors (\"skgrepo private: no git author is configured\")) (warnings ()))"))
                 "Nothing was committed. Errors: skgrepo private: no git author is configured")))

(ert-deftest test-stage-and-commit-rerenders-an-emptied-view ()
  "The server writes an empty view as '(content \"\")'; it empties the buffer."
  (with-temp-buffer
    (skg-content-view-mode)
    (setq-local skg-view-id "diff-view: all changes")
    (insert "* (skg node (gnid a)) a\n")
    (skg--apply-streamed-view-update
     "((view-id \"diff-view: all changes\") (content \"\"))" 'rerender "rerender-view")
    (should (equal (buffer-string) ""))))
