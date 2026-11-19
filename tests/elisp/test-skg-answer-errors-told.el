;;; test-skg-answer-errors-told.el --- A handler that fails on the server's answer says so -*- lexical-binding: t; -*-
;;;
;;; Each handler below catches any error it meets while applying an
;;; answer from the server. It logs the error, but the log is off by
;;; default, so it must also say so in the echo area
;;; (`skg-tell-answer-not-applied'); otherwise a save whose answer
;;; could not be applied looks like a save that did nothing.

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'cl-lib)
(require 'skg-length-prefix)
(require 'skg-request-save)
(require 'skg-request-rerender-all-views)
(require 'skg-request-diff-view)

(defconst test-skg-answer-errors--unreadable "("
  "A payload `read' fails on (end-of-file).")

(defmacro test-skg-answer-errors--told (&rest body)
  "Run BODY with the save stream's cleanup stubbed out, and return
the echo-area messages it showed, oldest first."
  (declare (indent 0))
  `(let ((told nil)
         (skg-response-handler-map nil)
         (skg-lp--pending-count 0))
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args)
                  (when fmt (push (apply #'format fmt args) told))))
               ((symbol-function 'skg--end-stream) #'ignore)
               ((symbol-function 'skg--unlock-all-save-locked) #'ignore))
       ,@body)
     (nreverse told)))

(defun test-skg-answer-errors--should-tell (told answer consequence)
  "TOLD holds one message naming ANSWER, CONSEQUENCE and an error."
  (should (cl-some
           (lambda (shown)
             (and (string-prefix-p (concat "skg: " consequence ": ") shown)
                  (string-match-p
                   (regexp-quote
                    (format "applying the server's %s answer failed (" answer))
                   shown)))
           told)))

(ert-deftest test-skg-save-result-error-is-told ()
  "The case that hid a whole day of saves: the save's answer arrived,
and redrawing the buffer signaled void-function."
  (let ((told
         (test-skg-answer-errors--told
           (cl-letf (((symbol-function 'skg--redraw-saved-buffer)
                      (lambda (&rest _)
                        (signal 'void-function '(skg-gone-after-reload)))))
             (skg-handle-save-sexp
              "((response-type save-result) (content \"* x\n\") (errors ()) (warnings ()))")))))
    (test-skg-answer-errors--should-tell
     told "save-result"
     "the server may have saved, but the buffer was not updated")
    (should (cl-some (lambda (shown)
                       (string-match-p "skg-gone-after-reload" shown))
                     told))))

(ert-deftest test-skg-broad-save-lock-error-is-told ()
  (test-skg-answer-errors--should-tell
   (test-skg-answer-errors--told
     (skg--broad-save-lock-handler "((response-type save-lock))"))
   "save-lock" "views stay locked until the save ends"))

(ert-deftest test-skg-save-relax-lock-error-is-told ()
  (test-skg-answer-errors--should-tell
   (test-skg-answer-errors--told
     (skg--save-relax-lock-handler
      "view-id-saved" "((response-type save-relax-lock) (lock-views malformed))"))
   "save-relax-lock" "views stay locked until the save ends"))

(ert-deftest test-skg-streamed-view-update-error-is-told ()
  "Shared by the save's collateral views and the rerender's views;
only a save's says the server may have saved."
  (test-skg-answer-errors--should-tell
   (test-skg-answer-errors--told
     (skg--apply-streamed-view-update
      test-skg-answer-errors--unreadable 'save "collateral-view" t))
   "collateral-view"
   "the server may have saved, but a view was not redrawn")
  (test-skg-answer-errors--should-tell
   (test-skg-answer-errors--told
     (skg--apply-streamed-view-update
      test-skg-answer-errors--unreadable 'rerender "rerender-view"))
   "rerender-view" "a view was not redrawn"))

(ert-deftest test-skg-fork-confirmation-error-is-told ()
  (let ((source (generate-new-buffer " *skg-answer-errors-fork*")))
    (unwind-protect
        (test-skg-answer-errors--should-tell
         (test-skg-answer-errors--told
           (skg--fork-confirmation-handler
            source test-skg-answer-errors--unreadable))
         "fork-confirmation"
         "nothing was saved, and the fork confirmation was not shown")
      (kill-buffer source))))

(ert-deftest test-skg-delete-from-confirmation-error-is-told ()
  (let ((source (generate-new-buffer " *skg-answer-errors-delete-from*")))
    (unwind-protect
        (test-skg-answer-errors--should-tell
         (test-skg-answer-errors--told
           (skg--delete-from-confirmation-handler
            source test-skg-answer-errors--unreadable nil nil nil nil))
         "delete-from-confirmation"
         "the save stopped for a confirmation, which failed, so it may not have saved")
      (kill-buffer source))))

(ert-deftest test-skg-strandeds-confirmation-error-is-told ()
  (let ((source (generate-new-buffer " *skg-answer-errors-strandeds*")))
    (unwind-protect
        (test-skg-answer-errors--should-tell
         (test-skg-answer-errors--told
           (skg--strandeds-confirmation-handler
            source test-skg-answer-errors--unreadable nil nil nil))
         "strandeds-confirmation"
         "the save stopped for a confirmation, which failed, so it may not have saved")
      (kill-buffer source))))

(ert-deftest test-skg-telescope-hoist-confirmation-error-is-told ()
  (let ((source (generate-new-buffer " *skg-answer-errors-hoist*")))
    (unwind-protect
        (test-skg-answer-errors--should-tell
         (test-skg-answer-errors--told
           (skg--telescope-hoist-confirmation-handler
            source test-skg-answer-errors--unreadable nil nil))
         "telescope-hoist-confirmation"
         "the save stopped for a confirmation, which failed, so it may not have saved")
      (kill-buffer source))))

(ert-deftest test-skg-save-text-release-confirmation-error-is-told ()
  (test-skg-answer-errors--should-tell
   (test-skg-answer-errors--told
     (skg--save-text-release-confirmation-handler
      test-skg-answer-errors--unreadable))
   "overPrivateText-telescope-confirmation"
   "the server saved, but the views it withheld were not redrawn"))

(ert-deftest test-skg-rerender-lock-error-is-told ()
  (test-skg-answer-errors--should-tell
   (test-skg-answer-errors--told
     (skg--register-rerender-stream-handlers)
     (funcall (cadr (assoc 'rerender-lock skg-response-handler-map))
              nil test-skg-answer-errors--unreadable))
   "rerender-lock"
   "every view was unlocked before the rerender redrew it"))

(ert-deftest test-skg-diff-view-error-is-told ()
  (let (outcome)
    (test-skg-answer-errors--should-tell
     (test-skg-answer-errors--told
       (setq outcome (skg--handle-diffView-response
                      nil test-skg-answer-errors--unreadable
                      "all changes" "diff-view-id")))
     "diff-view" "the diff view was not shown")
    (should (eq outcome 'failed))))

(ert-deftest test-skg-dispatch-error-is-told ()
  "A handler that lets an error out reaches the dispatcher, which
names the answer's response type; one it cannot read, it calls
unreadable."
  (let ((told
         (test-skg-answer-errors--told
           (skg-register-response-handler
            'save-result
            (lambda (_tcp-proc _payload)
              (signal 'void-function '(skg-gone-after-reload)))
            t)
           (skg-lp--dispatch-by-type nil "((response-type save-result))")
           (skg-lp--dispatch-by-type nil test-skg-answer-errors--unreadable))))
    (dolist (answer '("save-result" "unreadable"))
      (test-skg-answer-errors--should-tell
       told answer
       "the server may have acted (saved, say), but the client did not finish handling its answer"))))

(provide 'test-skg-answer-errors-told)
