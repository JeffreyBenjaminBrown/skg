;;; test-skg-streaming-request.el --- The prologue streaming commands share -*- lexical-binding: t; -*-

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'cl-lib)
(require 'ert)
(require 'skg-request-rerender-all-views)
(require 'skg-request-repo-sets)

(ert-deftest test-skg-send-streaming-request-sends-after-the-prologue ()
  "The request goes out last, after the stream begins, the buffers
lock, and every handler is registered, one-shot, and the receiver
reset."
  (let ((calls nil))
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust) (lambda () 'fake-proc))
              ((symbol-function 'skg--begin-stream)
               (lambda (label) (push (list 'begin-stream label) calls)))
              ((symbol-function 'skg--lock-all-skg-buffers)
               (lambda () (push '(lock) calls)))
              ((symbol-function 'skg--register-rerender-stream-handlers)
               (lambda () (push '(rerender-handlers) calls)))
              ((symbol-function 'skg--wrap-rerender-done-with-after-empty-stream)
               (lambda () (push '(wrap-rerender-done) calls)))
              ((symbol-function 'skg-register-response-handler)
               (lambda (type _handler one-shot)
                 (push (list 'handler type one-shot) calls)))
              ((symbol-function 'skg-lp-reset)
               (lambda () (push '(reset) calls)))
              ((symbol-function 'process-send-string)
               (lambda (proc text) (push (list 'send proc text) calls))))
      (skg--send-streaming-request
       "the label" 'the-answer '((request . "a request") (id . "x"))
       #'ignore `((other-answer . ,#'ignore)) t)
      (should (equal (nreverse calls)
                     '((begin-stream "the label")
                       (lock)
                       (rerender-handlers)
                       (wrap-rerender-done)
                       (handler the-answer t)
                       (handler other-answer t)
                       (reset)
                       (send fake-proc
                             "((request . \"a request\") (id . \"x\"))\n"))))
      ;; Without RETRY-AFTER-EMPTY-STREAM, rerender-done is not wrapped.
      (setq calls nil)
      (skg--send-streaming-request
       "the label" 'the-answer '((request . "a request")) #'ignore)
      (should-not (assq 'wrap-rerender-done calls))
      (should (eq 'send (car (car calls))))
      ;; An over-private-text retry is registered after the answer
      ;; handlers, before the reset, naming the answer to cancel.
      (setq calls nil)
      (cl-letf (((symbol-function
                  'skg--register-rerender-overPrivateText-confirmation)
                 (lambda (_retry unfired-response-type)
                   (push (list 'confirmation unfired-response-type) calls))))
        (skg--send-streaming-request
         "the label" 'the-answer '((request . "a request")) #'ignore
         nil nil #'ignore))
      (should (equal (cdr (nreverse calls))
                     '((lock)
                       (rerender-handlers)
                       (handler the-answer t)
                       (confirmation the-answer)
                       (reset)
                       (send fake-proc
                             "((request . \"a request\"))\n")))))))

(ert-deftest test-skg-send-streaming-request-unwinds-a-failed-send ()
  "When the send fails, the handlers it registered go, the pending
count returns to where it was, the stream ends and the buffers unlock."
  (let ((skg-response-handler-map nil)
        (skg-lp--pending-count 0)
        (skg--stream-in-progress nil)
        (unlocked nil))
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust) (lambda () 'fake-proc))
              ((symbol-function 'skg--lock-all-skg-buffers) #'ignore)
              ((symbol-function 'skg--unlock-all-save-locked)
               (lambda () (setq unlocked t)))
              ((symbol-function 'process-send-string)
               (lambda (&rest _) (error "connection lost"))))
      (should-error
       (skg--send-streaming-request
        "the label" 'the-answer '((request . "a request")) #'ignore
        `((other-answer . ,#'ignore)) t #'ignore))
      (should (null skg-response-handler-map))
      (should (= 0 skg-lp--pending-count))
      (should (null skg--stream-in-progress))
      (should unlocked))))

(ert-deftest test-skg-restrict-repo-set-confirmation-cancels-its-answer-and-retries ()
  "The repo-set switch goes through the prologue: an over-private-text
challenge cancels its one-shot 'skgrepo-restriction' handler, and once
the empty rerender stream is done the retry resends the request with
the approved PIDs."
  (let ((skg-response-handler-map nil)
        (skg-lp--pending-count 0)
        (skg--stream-in-progress nil)
        (skg--rerender-overPrivateText-retry nil)
        (skg--rerender-overPrivateText-challenged nil)
        (sent nil)
        (scheduled nil))
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust) (lambda () 'fake-proc))
              ((symbol-function 'skg--lock-all-skg-buffers) #'ignore)
              ((symbol-function 'skg--unlock-all-save-locked) #'ignore)
              ((symbol-function 'skg--unlock-buffers-not-in-view-id-list) #'ignore)
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
              ((symbol-function 'run-at-time)
               (lambda (_delay _repeat fn &rest args)
                 (push (cons fn args) scheduled)))
              ((symbol-function 'process-send-string)
               (lambda (_proc text) (push text sent))))
      (skg-restrict-repo-set "rs")
      (should (= 1 (length sent)))
      (should (string-match-p "set skgrepo restriction" (car sent)))
      (should-not (string-match-p "approved-overPrivateText-pids" (car sent)))
      (should (assq 'skgrepo-restriction skg-response-handler-map))
      (should (= 3 skg-lp--pending-count)) ; lock, done, the answer
      (skg-lp--dispatch-by-type
       nil
       "((response-type overPrivateText-telescope-confirmation) \
         (operation rerender) (pids (p1 p2)) (prompt \"Include?\"))")
      (should-not (assq 'skgrepo-restriction skg-response-handler-map))
      (should (= 2 skg-lp--pending-count))
      (skg-lp--dispatch-by-type
       nil "((response-type rerender-lock) (lock-views ()))")
      (skg-lp--dispatch-by-type
       nil "((response-type rerender-done) (errors ()) (warnings ()))")
      (should (= 0 skg-lp--pending-count))
      (should (null skg--stream-in-progress))
      (should (= 1 (length scheduled)))
      (let ((retry (pop scheduled)))
        (apply (car retry) (cdr retry)))
      (should (= 2 (length sent)))
      (let ((retried (read (car sent))))
        (should (equal (cdr (assq 'name retried)) "rs"))
        (should (equal (cdr (assq 'approved-overPrivateText-pids retried))
                       '("p1" "p2"))))
      (should (assq 'skgrepo-restriction skg-response-handler-map)))))

(ert-deftest test-skg-response-errors-line-joins-the-errors ()
  (should (equal (skg--response-errors-line
                  (read "((errors (\"first\" \"second\")) (warnings (\"w\")))"))
                 "first; second"))
  (should (equal (skg--response-errors-line
                  (read "((errors (\"only\")))"))
                 "only"))
  (should (null (skg--response-errors-line
                 (read "((errors ()) (warnings (\"w\")))"))))
  (should (null (skg--response-errors-line
                 (read "((content \"x\"))")))))

(provide 'test-skg-streaming-request)
