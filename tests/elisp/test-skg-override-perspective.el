;;; test-skg-override-perspective.el --- Cycling a view's perspective, whose overrides it follows -*- lexical-binding: t; -*-

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'cl-lib)
(require 'ert)
(require 'org)
(require 'skg-buffer)
(require 'skg-request-save)
(require 'skg-request-override-perspective)

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

(ert-deftest test-override-perspective-refuses-a-buffer-with-unsaved-edits ()
  (test--in-view "r"
    (set-buffer-modified-p t)
    (test--overrides-followed-run
     (lambda (sent _handlers)
       (let ((err (should-error (skg-override-perspective)
                                :type 'user-error)))
         (should (string-match-p "unsaved edits" (cadr err))))
       (should (null (funcall sent)))))))

(ert-deftest test-override-perspective-asks-for-the-next-whatever-the-buffer-shows ()
  "The server computes the cycle, so the client always asks for the next."
  (test--in-view "r"
    (test--overrides-followed-run
     (lambda (sent handlers)
       (skg-override-perspective)
       (should (string-match-p
                (regexp-quote "(request . \"set overrides followed\") (view-id . \"r\") (overrides-followed . \"next\")")
                (car (funcall sent))))
       (should (gethash 'overrides-followed handlers))
       (skg--set-override-perspective "owner of tracy" "tracy's overrides")
       (skg-override-perspective)
       (should (string-match-p
                (regexp-quote "(overrides-followed . \"next\")")
                (car (funcall sent))))))))

(ert-deftest test-override-perspective-answer-sets-the-buffer-names-it-and-marks-the-mode-line ()
  "The mode line's process part is an :eval form, which a batch Emacs's
'format-mode-line' never evaluates, so the form is evaluated here."
  (test--in-view "r"
    (let ((shown nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq shown (apply #'format format-string args)))))
        (skg--override-perspective-handler
         nil "((response-type overrides-followed) (view-id \"r\") (overrides-followed \"user\") (perspective-name \"your overrides\"))")
        (should (equal skg-overrides-followed "user"))
        (should (equal shown "Perspective: your overrides."))
        (should (equal (eval (cadr mode-line-process) t) ":your overrides"))
        (skg--override-perspective-handler
         nil "((response-type overrides-followed) (view-id \"r\") (overrides-followed \"owner of tracy\") (perspective-name \"tracy's overrides\"))")
        (should (equal skg-overrides-followed "owner of tracy"))
        (should (equal shown "Perspective: tracy's overrides."))
        (should (equal (eval (cadr mode-line-process) t) ":tracy's overrides"))
        (skg--override-perspective-handler
         nil "((response-type overrides-followed) (view-id \"r\") (overrides-followed \"root authors\") (perspective-name \"each root author's overrides (tracy's)\"))")
        (should (null skg-overrides-followed))
        (should (null skg-override-perspective-name))
        (should (equal shown "Perspective: each root author's overrides (tracy's)."))
        (should (null (eval (cadr mode-line-process) t)))))))

(ert-deftest test-override-perspective-refusal-changes-nothing-and-says-why ()
  (test--in-view "p"
    (let ((shown nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq shown (apply #'format format-string args)))))
        (skg--override-perspective-handler
         nil "((response-type overrides-followed) (view-id \"p\") (overrides-followed \"root authors\") (perspective-name \"each root author's overrides (yours)\") (content \"No one's overrides would show this buffer differently from its root authors'.\"))")
        (should (null skg-overrides-followed))
        (should (string-match-p "No one's overrides" shown))))))

(ert-deftest test-override-perspective-rides-in-the-save-request-and-survives-a-mode-change ()
  (test--in-view "r"
    (let ((position '(:point-lines-below-focused-headline 0 :point-column 0
                      :point-screen-lines-below-window-start 0)))
      (should-not (assq 'overrides-followed (skg--save-request-sexp "r" position)))
      (skg--set-override-perspective "owner of tracy" "tracy's overrides")
      (skg-content-view-mode)
      (should (equal skg-overrides-followed "owner of tracy"))
      (should (equal skg-override-perspective-name "tracy's overrides"))
      (should (equal (cdr (assq 'overrides-followed (skg--save-request-sexp "r" position)))
                     "owner of tracy")))))

(ert-deftest test-override-perspective-is-on-C-c-v-o-and-C-c-v-p-is-free ()
  (should (eq (lookup-key skg-content-view-mode-map (kbd "C-c v o"))
              #'skg-override-perspective))
  (should-not (lookup-key skg-content-view-mode-map (kbd "C-c v p"))))

(provide 'test-skg-override-perspective)
