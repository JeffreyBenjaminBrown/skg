;;; test-skg-rebuild-session-lifetime-stores.el --- The rebuild command, which also reloads the config -*- lexical-binding: t; -*-

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'cl-lib)
(require 'skg-request-rebuild-session-lifetime-stores)

(defun test--rebuild-run (body)
  "Run BODY with the network faked. BODY receives the sent requests
(newest first, via a function) and the registered handlers."
  (let ((sent nil) (handlers (make-hash-table :test 'eq)))
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust) (lambda () 'fake-proc))
              ((symbol-function 'process-send-string)
               (lambda (_proc text) (push text sent)))
              ((symbol-function 'skg-lp-reset) #'ignore)
              ((symbol-function 'ding) #'ignore)
              ((symbol-function 'run-at-time)
               (lambda (_time _repeat function &rest args) (apply function args)))
              ((symbol-function 'skg-request-rerender-all-views)
               (lambda () (push "rerender all views" sent)))
              ((symbol-function 'skg-register-response-handler)
               (lambda (type handler _one-shot) (puthash type handler handlers))))
      (funcall body (lambda () sent) handlers))))

(ert-deftest test-rebuild-refetches-the-herald-rules-then-rerenders-all-views ()
  "A successful rebuild may have reloaded a config with other skgrepo
labels, so the herald rules are fetched again; then every view
without unsaved edits is rerendered from the rebuilt graph."
  (test--rebuild-run
   (lambda (sent handlers)
     (skg-rebuild-session-lifetime-stores)
     (should (equal (funcall sent)
                    '("((request . \"rebuild session-lifetime stores\"))\n")))
     (funcall (gethash 'rebuild-session-lifetime-stores handlers)
              'fake-proc
              "((response-type rebuild-session-lifetime-stores) (content \"Config reloaded; rebuilt.\"))")
     (should (equal (funcall sent)
                    '("rerender all views"
                      "((request . \"herald rules\"))\n"
                      "((request . \"rebuild session-lifetime stores\"))\n"))))))

(ert-deftest test-a-failed-rebuild-fetches-nothing-more ()
  (test--rebuild-run
   (lambda (sent handlers)
     (skg-rebuild-session-lifetime-stores)
     (funcall (gethash 'rebuild-session-lifetime-stores handlers)
              'fake-proc
              "((response-type rebuild-session-lifetime-stores) (content \"Rebuild failed: Reloading config from x: bad\"))")
     (should (equal (length (funcall sent)) 1)))))

(provide 'test-skg-rebuild-session-lifetime-stores)
