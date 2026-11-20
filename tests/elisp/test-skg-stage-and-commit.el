;;; test-skg-stage-and-commit.el --- Stage-and-commit client tests -*- lexical-binding: t; -*-

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'cl-lib)
(require 'ert)
(require 'org)
(require 'skg-buffer)
(require 'skg-keymaps-and-aliases)
(require 'skg-request-stage-and-commit)

(defconst test--stage-and-commit-needs-confirmation
  "((needs-confirmation . t) (processed-summary (processed 1) (changed 3) (foreign 0) (order-only 0) (notEditable 0)) (errors ()) (warnings ()))")

(defun test--stage-and-commit-run (typed body)
  "Run BODY with the network faked and every `read-string' answered
from TYPED, a list of strings. BODY receives the sent requests (newest
first, via a function), the handlers and the prompts. No commit-message
buffer is left over from before or after."
  (let ((sent nil) (prompts nil) (handlers (make-hash-table :test 'eq))
        (skg--rerender-after-empty-stream nil))
    (test--stage-and-commit-kill-message-buffer)
    (unwind-protect
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
            (funcall body (lambda () sent) handlers (lambda () prompts)))
      (test--stage-and-commit-kill-message-buffer))))

(defun test--stage-and-commit-kill-message-buffer ()
  (when (get-buffer skg--commit-message-buffer-name)
    (kill-buffer skg--commit-message-buffer-name)))

(defun test--stage-and-commit-write-and-send (text &optional opener)
  "Open the commit-message buffer by OPENER (`skg-stage-and-commit' by
default), write TEXT in it, and press C-c C-c."
  (funcall (or opener #'skg-stage-and-commit))
  (with-current-buffer skg--commit-message-buffer-name
    (insert text)
    (funcall (key-binding (kbd "C-c C-c")))))

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
     nil
     (lambda (sent _handlers _prompts)
       (should-error (test--stage-and-commit-write-and-send " \n\t\n")
                     :type 'user-error)
       (should (null (funcall sent)))
       (should (get-buffer skg--commit-message-buffer-name))))))

(ert-deftest test-stage-and-commit-takes-the-message-from-its-buffer ()
  "C-c C-c sends the buffer's text less its trailing whitespace, and
keeps the buffer; a message left there is kept for the next attempt."
  (test--stage-and-commit-run
   nil
   (lambda (sent _handlers _prompts)
     (skg-stage-and-commit)
     (with-current-buffer skg--commit-message-buffer-name
       (should (derived-mode-p 'skg-commit-message-mode))
       (should (eq (key-binding (kbd "C-c C-k")) #'skg-commit-message-cancel))
       (insert "kept"))
     (skg-stage-and-commit)
     (with-current-buffer skg--commit-message-buffer-name
       (should (equal (buffer-string) "kept"))
       (erase-buffer))
     (test--stage-and-commit-write-and-send "  summary\n\nbody  \n\n \n")
     (should (string-match-p
              (regexp-quote "(message-lines \"  summary\" \"\" \"body\") (processed-only . t))")
              (car (funcall sent))))
     (should (equal (with-current-buffer skg--commit-message-buffer-name
                      (buffer-string))
                    "  summary\n\nbody  \n\n \n")))))

(ert-deftest test-stage-and-commit-keeps-the-message-until-it-is-moot ()
  "The commit-message buffer goes once something is committed, or once
nothing had changed and nothing failed; when nothing was processed,
everything processed waits, after a failure, a refusal or a declined
confirmation it stays, and the line says so."
  (dolist (case
           '(("((commits ((public 5f135fea))) (errors ()) (warnings ()))"
              nil "Committed in public (5f135fe).")
             ("((commits ((\"public\" \"5f135fea\"))) (committed-nodes 1) (left-nodes 2) (waiting ()) (errors ()) (warnings ()))"
              nil "Committed 1 node, in public (5f135fe). 2 changed nodes are left.")
             ("((commits ((public 5f135fea))) (errors (\"skgrepo private: no git author is configured\")) (warnings ()))"
              nil "Committed in public (5f135fe). Errors: skgrepo private: no git author is configured")
             ("((commits ()) (errors ()) (warnings ()))"
              nil "Nothing has changed, so nothing was committed.")
             ("((commits ()) (committed-nodes 0) (left-nodes 5) (waiting ()) (errors ()) (warnings ()))"
              t "Nothing is processed, so nothing was committed; C-c r C commits everything. Your message is kept in *skg commit message*.")
             ("((commits ()) (committed-nodes 0) (left-nodes 2) (waiting ((\"a\" \"Plans\" names-new \"b\" \"Seeds\"))) (errors ()) (warnings ()))"
              t "Nothing was committed. 2 changed nodes are left; 1 of them is processed but waits: 'Plans' names 'Seeds', which is new and does not go in. Your message is kept in *skg commit message*.")
             ("((commits ()) (errors (\"skgrepo private: no git author is configured\")) (warnings ()))"
              t "Nothing was committed. Errors: skgrepo private: no git author is configured Your message is kept in *skg commit message*.")
             ("((errors (\"A commit message is required.\")) (warnings ()))"
              t "Nothing was committed. Errors: A commit message is required. Your message is kept in *skg commit message*.")))
    (test--stage-and-commit-run
     nil
     (lambda (_sent handlers _prompts)
       (let ((messages nil))
         (cl-letf (((symbol-function 'message)
                    (lambda (format-string &rest args)
                      (push (apply #'format format-string args) messages))))
           (test--stage-and-commit-write-and-send "the message")
           (funcall (gethash 'stage-and-commit handlers) 'fake-proc (car case)))
         (should (equal (car messages) (nth 2 case)))
         (should (eq (and (get-buffer skg--commit-message-buffer-name) t)
                     (nth 1 case)))))))
  (test--stage-and-commit-run
   '("no")
   (lambda (sent handlers _prompts)
     (let ((messages nil))
       (cl-letf (((symbol-function 'message)
                  (lambda (format-string &rest args)
                    (push (apply #'format format-string args) messages))))
         (test--stage-and-commit-write-and-send
          "the message" #'skg-stage-and-commit-everything)
         (funcall (gethash 'stage-and-commit handlers) 'fake-proc
                  test--stage-and-commit-needs-confirmation)
         (funcall skg--rerender-after-empty-stream))
       (should (= 1 (length (funcall sent))))
       (should (equal (car messages)
                      "Not committed; your message is kept in *skg commit message*."))
       (skg-stage-and-commit)
       (should (equal (with-current-buffer skg--commit-message-buffer-name
                        (buffer-string))
                      "the message"))))))

(ert-deftest test-stage-and-commit-cancel-sends-nothing-and-says-so ()
  (test--stage-and-commit-run
   nil
   (lambda (sent _handlers _prompts)
     (let ((messages nil))
       (cl-letf (((symbol-function 'message)
                  (lambda (format-string &rest args)
                    (push (apply #'format format-string args) messages))))
         (skg-stage-and-commit)
         (with-current-buffer skg--commit-message-buffer-name
           (insert "never sent")
           (funcall (key-binding (kbd "C-c C-k")))))
       (should (null (funcall sent)))
       (should-not (get-buffer skg--commit-message-buffer-name))
       (should (string-match-p "Commit canceled" (car messages)))))))

(ert-deftest test-stage-and-commit-the-plain-key-commits-the-processed-and-the-capital-everything ()
  "C-c r c and C-c r C open one buffer; its C-c C-c commits the
processed nodes or every changed node, whichever opened it last, and
its header line says which."
  (should (eq (lookup-key skg-content-view-mode-map (kbd "C-c r c"))
              #'skg-stage-and-commit))
  (should (eq (lookup-key skg-content-view-mode-map (kbd "C-c r C"))
              #'skg-stage-and-commit-everything))
  (test--stage-and-commit-run
   nil
   (lambda (sent _handlers _prompts)
     (skg-stage-and-commit-everything)
     (with-current-buffer skg--commit-message-buffer-name
       (should (string-match-p "commits every changed node" header-line-format))
       (insert "kept"))
     (test--stage-and-commit-write-and-send "" #'skg-stage-and-commit)
     (should (string-match-p (regexp-quote "(processed-only . t)")
                             (car (funcall sent))))
     (with-current-buffer skg--commit-message-buffer-name
       (should (string-match-p "commits the processed nodes" header-line-format)))
     (test--stage-and-commit-write-and-send "" #'skg-stage-and-commit-everything)
     (should (= 2 (length (funcall sent))))
     (should-not (string-match-p "processed-only" (car (funcall sent))))
     (should (string-match-p (regexp-quote "(message-lines \"kept\")")
                             (car (funcall sent)))))))

(ert-deftest test-stage-and-commit-resends-confirmed-only-on-yes ()
  "Only a commit of everything is asked about; \"yes\" resends it
confirmed, still without '(processed-only . t)'."
  (test--stage-and-commit-run
   '("yes")
   (lambda (sent handlers prompts)
     (test--stage-and-commit-write-and-send
      "the message" #'skg-stage-and-commit-everything)
     (should (string-match-p (regexp-quote "(request . \"stage and commit\")")
                             (car (funcall sent))))
     (should (string-match-p (regexp-quote "(message-lines \"the message\")")
                             (car (funcall sent))))
     (should-not (string-match-p "confirmed\\|processed-only" (car (funcall sent))))
     (funcall (gethash 'stage-and-commit handlers) 'fake-proc
              test--stage-and-commit-needs-confirmation)
     (should (null (funcall prompts)))
     (should skg--rerender-after-empty-stream)
     (funcall skg--rerender-after-empty-stream)
     (should (equal (car (funcall prompts))
                    "1 of 3 changed nodes are processed. Type yes to commit anyway: "))
     (should (= 2 (length (funcall sent))))
     (should (string-match-p (regexp-quote "(confirmed . t)")
                             (car (funcall sent))))
     (should-not (string-match-p "processed-only" (car (funcall sent))))
     (should (string-match-p (regexp-quote "(message-lines \"the message\")")
                             (car (funcall sent))))))
  (test--stage-and-commit-run
   '("y")
   (lambda (sent handlers _prompts)
     (test--stage-and-commit-write-and-send
      "the message" #'skg-stage-and-commit-everything)
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
     (skg--stage-and-commit-send "summary\n\n  indented \\n \"quoted\"" t nil)
     (let ((request (car (funcall sent))))
       (should (string-match-p
                (regexp-quote
                 "(message-lines \"summary\" \"\" \"  indented \\\\n \\\"quoted\\\"\")")
                request))
       (should (= 1 (cl-count ?\n request)))
       (should (string-suffix-p "\n" request))))))

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
      (skg--stage-and-commit-send "the message" nil nil)
      (skg-lp-handle-generic-chunk
       'fake-proc
       (test--stage-and-commit-framed
        "((response-type stage-and-commit) (needs-confirmation . t) (processed-summary (processed 1) (changed 3) (foreign 0) (order-only 0) (waiting 0) (notEditable 0)) (errors ()) (warnings ()))"
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
                 "Nothing was committed. Errors: skgrepo private: no git author is configured"))
  (should (equal (skg--stage-and-commit-result-message
                  (read "((commits ()) (committed-nodes 0) (left-nodes 0) (waiting ()) (errors ()) (warnings ()))"))
                 "Nothing has changed, so nothing was committed.")))

(ert-deftest test-stage-and-commit-result-message-says-what-waits-and-why ()
  "The wire's example answer, and each reason a processed node waits."
  (should (equal (skg--stage-and-commit-result-message
                  (read "((commits ((\"public\" \"9c1e40d2\") (\"private\" \"41aa07fe\"))) (committed-nodes 2) (left-nodes 2) (waiting ((\"PID-A\" \"Plans for spring\" names-new \"PID-B\" \"Seed order\"))) (errors ()) (warnings ()))"))
                 "Committed 2 nodes, in public (9c1e40d), private (41aa07f). 2 changed nodes are left; 1 of them is processed but waits: 'Plans for spring' names 'Seed order', which is new and does not go in."))
  (should (equal (skg--stage-and-commit-result-message
                  (read "((commits ((\"public\" \"9c1e40d2\"))) (committed-nodes 1) (left-nodes 3) (waiting ((\"x\" \"X\" still-named \"y\" \"Y\") (\"m\" \"M\" merge \"n\" \"N\") (\"p\" \"P\" someday \"q\" \"Q\"))) (errors ()) (warnings ()))"))
                 "Committed 1 node, in public (9c1e40d). 3 changed nodes are left; 3 of them are processed but wait: 'X' is deleted, but 'Y', which does not go in, still names it; 'M' is part of a merge with 'N', which does not go in; 'P' waits for 'Q' (someday)."))
  (should (equal (skg--stage-and-commit-result-message
                  (read "((commits ((\"public\" \"9c1e40d2\"))) (committed-nodes 3) (left-nodes 1) (waiting ()) (errors (\"skgrepo private: git commit failed\")) (warnings ()))"))
                 "Committed 3 nodes, in public (9c1e40d). 1 changed node is left. Errors: skgrepo private: git commit failed")))

(ert-deftest test-stage-and-commit-result-message-says-what-no-mark-covers ()
  "A node left because it changed in a way no mark covers is named,
with how it changed, and the line says what can commit it."
  (should (equal (skg--stage-and-commit-result-message
                  (read "((commits ((\"public\" \"9c1e40d2\"))) (committed-nodes 1) (left-nodes 3) (waiting ((\"x\" \"X\" still-named \"y\" \"Y\"))) (unmarkable ((\"h\" \"H\" (\"its file changed in a way the diff view does not show (formatting, say)\")) (\"t\" \"T\" (\"its file changed in a way the diff view does not show (formatting, say)\")))) (errors ()) (warnings ()))"))
                 "Committed 1 node, in public (9c1e40d). 3 changed nodes are left; 1 of them is processed but waits: 'X' is deleted, but 'Y', which does not go in, still names it; 2 of them changed in ways no mark covers, which C-c r C or Git can commit: 'H' (its file changed in a way the diff view does not show (formatting, say)); 'T' (its file changed in a way the diff view does not show (formatting, say))."))
  (should (equal (skg--stage-and-commit-result-message
                  (read "((commits ()) (committed-nodes 0) (left-nodes 1) (waiting ()) (unmarkable ((\"s\" \"S\" (\"its file changed in a way the diff view does not show (formatting, say)\")))) (errors ()) (warnings ()))"))
                 "Nothing was committed. 1 changed node is left; 1 of them changed in a way no mark covers, which C-c r C or Git can commit: 'S' (its file changed in a way the diff view does not show (formatting, say)).")))

(ert-deftest test-stage-and-commit-rerenders-an-emptied-view ()
  "The server writes an empty view as '(content \"\")'; it empties the buffer."
  (with-temp-buffer
    (skg-content-view-mode)
    (setq-local skg-view-id "diff-view: all changes")
    (insert "* (skg node (gnid a)) a\n")
    (skg--apply-streamed-view-update
     "((view-id \"diff-view: all changes\") (content \"\"))" 'rerender "rerender-view")
    (should (equal (buffer-string) ""))))
