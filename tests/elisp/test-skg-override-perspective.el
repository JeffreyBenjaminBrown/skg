;;; test-skg-overrides-followed.el --- Switching whose overrides a view follows -*- lexical-binding: t; -*-

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'cl-lib)
(require 'ert)
(require 'org)
(require 'skg-buffer)
(require 'skg-request-save)
(require 'skg-request-overrides-followed)

(defun test--overrides-followed-run (body)
  "Run BODY with the network faked. BODY receives a function giving
the sent requests (newest first) and the handlers."
  (let ((sent nil) (handlers (make-hash-table :test 'eq)))
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust) (lambda () 'fake-proc))
              ((symbol-function 'process-send-string)
               (lambda (_proc text) (push text sent)))
              ((symbol-function 'skg--begin-stream) #'ignore)
              ((symbol-function 'skg--lock-all-skg-buffers) #'ignore)
              ((symbol-function 'skg--register-rerender-stream-handlers) #'ignore)
              ((symbol-function 'skg--register-rerender-overPrivateText-confirmation) #'ignore)
              ((symbol-function 'skg-lp-reset) #'ignore)
              ((symbol-function 'skg-register-response-handler)
               (lambda (type handler _one-shot) (puthash type handler handlers))))
      (funcall body (lambda () sent) handlers))))

(defmacro test--in-view (view-id &rest body)
  "Run BODY in a clean content view named VIEW-ID."
  (declare (indent 1))
  `(with-temp-buffer
     (skg-content-view-mode)
     (setq-local skg-view-id ,view-id)
     (insert "* (skg node (gnid r)) r\n")
     (set-buffer-modified-p nil)
     ,@body))

(ert-deftest test-overrides-followed-refuses-a-buffer-with-unsaved-edits ()
  (test--in-view "r"
    (set-buffer-modified-p t)
    (test--overrides-followed-run
     (lambda (sent _handlers)
       (let ((err (should-error (skg-switch-overrides-followed)
                                :type 'user-error)))
         (should (string-match-p "unsaved edits" (cadr err))))
       (should (null (funcall sent)))))))

(ert-deftest test-overrides-followed-asks-for-the-other-choice ()
  "A fresh view asks to follow the user's; one that does, its root authors'."
  (test--in-view "r"
    (test--overrides-followed-run
     (lambda (sent handlers)
       (skg-switch-overrides-followed)
       (should (string-match-p
                (regexp-quote "(request . \"set overrides followed\") (view-id . \"r\") (overrides-followed . \"user\")")
                (car (funcall sent))))
       (should (gethash 'overrides-followed handlers))
       (setq skg-overrides-followed "user")
       (skg-switch-overrides-followed)
       (should (string-match-p
                (regexp-quote "(overrides-followed . \"root authors\")")
                (car (funcall sent))))))))

(ert-deftest test-overrides-followed-answer-sets-the-buffer-and-the-mode-line ()
  "The mode line's process part is an :eval form, which a batch Emacs's
'format-mode-line' never evaluates, so the form is evaluated here."
  (test--in-view "r"
    (cl-letf (((symbol-function 'message) #'ignore))
      (skg--overrides-followed-handler
       nil "((response-type overrides-followed) (view-id \"r\") (overrides-followed \"user\"))")
      (should (equal skg-overrides-followed "user"))
      (should (equal (eval (cadr mode-line-process) t) ":your overrides"))
      (skg--overrides-followed-handler
       nil "((response-type overrides-followed) (view-id \"r\") (overrides-followed \"root authors\"))")
      (should (null skg-overrides-followed))
      (should (null (eval (cadr mode-line-process) t))))))

(ert-deftest test-overrides-followed-refusal-changes-nothing-and-says-why ()
  (test--in-view "p"
    (let ((shown nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq shown (apply #'format format-string args)))))
        (skg--overrides-followed-handler
         nil "((response-type overrides-followed) (view-id \"p\") (overrides-followed \"root authors\") (content \"Every tree in this buffer is rooted at a node you own, so it shows your overrides already.\"))")
        (should (null skg-overrides-followed))
        (should (string-match-p "shows your overrides already" shown))))))

(ert-deftest test-overrides-followed-rides-in-the-save-request-and-survives-a-mode-change ()
  (test--in-view "r"
    (let ((position '(:point-lines-below-focused-headline 0 :point-column 0
                      :point-screen-lines-below-window-start 0)))
      (should-not (assq 'overrides-followed (skg--save-request-sexp "r" position)))
      (setq skg-overrides-followed "user")
      (skg-content-view-mode)
      (should (equal skg-overrides-followed "user"))
      (should (equal (cdr (assq 'overrides-followed (skg--save-request-sexp "r" position)))
                     "user")))))

(ert-deftest test-overrides-followed-is-on-C-c-v-p ()
  (should (eq (lookup-key skg-content-view-mode-map (kbd "C-c v p"))
              #'skg-switch-overrides-followed)))

(provide 'test-skg-overrides-followed)
