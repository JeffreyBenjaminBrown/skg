;;; test-skg-streaming-request.el --- The prologue streaming commands share -*- lexical-binding: t; -*-

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'cl-lib)
(require 'ert)
(require 'skg-request-rerender-all-views)

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
      (should (eq 'send (car (car calls)))))))

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
