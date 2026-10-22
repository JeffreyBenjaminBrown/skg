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
    (insert "* (skg (node (id a))) a\n")
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
     (should (string-match-p (regexp-quote "(message . \"the message\")")
                             (car (funcall sent))))
     (should-not (string-match-p "confirmed" (car (funcall sent))))
     (funcall (gethash 'stage-and-commit handlers) 'fake-proc
              test--stage-and-commit-needs-confirmation)
     (should (string-match-p "1 of 3 changed files are ready"
                             (car (funcall prompts))))
     (should skg--rerender-after-empty-stream)
     (funcall skg--rerender-after-empty-stream)
     (should (= 2 (length (funcall sent))))
     (should (string-match-p (regexp-quote "(confirmed . t)")
                             (car (funcall sent))))
     (should (string-match-p (regexp-quote "(message . \"the message\")")
                             (car (funcall sent))))))
  (test--stage-and-commit-run
   '("the message" "y")
   (lambda (sent handlers _prompts)
     (skg-stage-and-commit)
     (funcall (gethash 'stage-and-commit handlers) 'fake-proc
              test--stage-and-commit-needs-confirmation)
     (should-not skg--rerender-after-empty-stream)
     (should (= 1 (length (funcall sent)))))))

(ert-deftest test-stage-and-commit-prompt-names-already-staged-entries ()
  "Entries already staged are listed in the prompt, by gitrepo, even
when every changed file is ready; \"yes\" resends confirmed."
  (test--stage-and-commit-run
   '("the message" "yes")
   (lambda (sent handlers prompts)
     (skg-stage-and-commit)
     (funcall (gethash 'stage-and-commit handlers) 'fake-proc
              "((needs-confirmation . t) (already-staged ((\"/data/public\" \"notes.txt\" \"a b.skg\") (\"/data/private\" \"c.skg\"))) (readiness-summary (ready 3) (changed 3) (foreign 0) (order-only 0) (notEditable 0)) (errors ()) (warnings ()))")
     (should (equal (car (funcall prompts))
                    "COMMIT READY. All 3 of 3 changed files are ready. Staged already, so committed too: /data/public (notes.txt, a b.skg); /data/private (c.skg). Type yes to commit anyway: "))
     (funcall skg--rerender-after-empty-stream)
     (should (string-match-p (regexp-quote "(confirmed . t)")
                             (car (funcall sent)))))))

(ert-deftest test-stage-and-commit-result-message ()
  (should (equal (skg--stage-and-commit-result-message
                  (read "((commits ((public 5f135fea1585a3253a5e) (private bff3a57e0670))) (errors ()) (warnings ()))"))
                 "Committed in public (5f135fe), private (bff3a57)."))
  (should (equal (skg--stage-and-commit-result-message
                  (read "((commits ()) (errors (\"skgrepo private: no git author is configured\")) (warnings ()))"))
                 "Nothing was committed. Errors: skgrepo private: no git author is configured")))

(ert-deftest test-stage-and-commit-redraws-an-emptied-view ()
  "The server writes an empty view as '(content \"\")'; it empties the buffer."
  (with-temp-buffer
    (skg-content-view-mode)
    (setq-local skg-view-id "diff-view: all changes")
    (insert "* (skg (node (id a))) a\n")
    (skg--apply-streamed-view-update
     "((view-id \"diff-view: all changes\") (content \"\"))" 'rerender "rerender-view")
    (should (equal (buffer-string) ""))))
