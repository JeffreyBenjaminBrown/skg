;;; test-skg-perspective.el --- Cycling a view's perspective -*- lexical-binding: t; -*-

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'cl-lib)
(require 'ert)
(require 'org)
(require 'skg-buffer)
(require 'skg-request-save)
(require 'skg-request-perspective)

(defun test--perspective-run (body)
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

(ert-deftest test-perspective-refuses-a-buffer-with-unsaved-edits ()
  (test--in-view "r"
    (set-buffer-modified-p t)
    (test--perspective-run
     (lambda (sent _handlers)
       (let ((err (should-error (skg-cycle-perspective)
                                :type 'user-error)))
         (should (string-match-p "unsaved edits" (cadr err))))
       (should (null (funcall sent)))))))

(ert-deftest test-perspective-asks-for-the-next-whatever-the-buffer-shows ()
  "The server computes the cycle, so the client always asks for the next."
  (test--in-view "r"
    (test--perspective-run
     (lambda (sent handlers)
       (skg-cycle-perspective)
       (should (string-match-p
                (regexp-quote "(request . \"set perspective\") (view-id . \"r\") (perspective . \"next\")")
                (car (funcall sent))))
       (should (gethash 'perspective handlers))
       (skg--set-perspective "skgrepo tracy" "the perspective of tracy")
       (skg-cycle-perspective)
       (should (string-match-p
                (regexp-quote "(perspective . \"next\")")
                (car (funcall sent))))))))

(ert-deftest test-perspective-answer-sets-the-buffer-names-it-and-marks-the-mode-line ()
  "The mode line's process part is an :eval form, which a batch Emacs's
'format-mode-line' never evaluates, so the form is evaluated here."
  (test--in-view "r"
    (let ((shown nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq shown (apply #'format format-string args)))))
        (skg--cycle-perspective-handler
         nil "((response-type perspective) (view-id \"r\") (perspective \"skgrepo main\") (perspective-name \"the perspective of main\"))")
        (should (equal skg-perspective "skgrepo main"))
        (should (equal shown "Showing the perspective of main."))
        (should (equal (eval (cadr mode-line-process) t) ":the perspective of main"))
        (skg--cycle-perspective-handler
         nil "((response-type perspective) (view-id \"r\") (perspective \"skgrepo tracy\") (perspective-name \"the perspective of tracy\"))")
        (should (equal skg-perspective "skgrepo tracy"))
        (should (equal shown "Showing the perspective of tracy."))
        (should (equal (eval (cadr mode-line-process) t) ":the perspective of tracy"))
        (skg--cycle-perspective-handler
         nil "((response-type perspective) (view-id \"r\") (perspective \"root homes\") (perspective-name \"the perspective of each root's home skgrepo (tracy)\"))")
        (should (null skg-perspective))
        (should (null skg-perspective-name))
        (should (equal shown "Showing the perspective of each root's home skgrepo (tracy)."))
        (should (null (eval (cadr mode-line-process) t)))))))

(ert-deftest test-perspective-refusal-changes-nothing-and-says-why ()
  (test--in-view "p"
    (let ((shown nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq shown (apply #'format format-string args)))))
        (skg--cycle-perspective-handler
         nil "((response-type perspective) (view-id \"p\") (perspective \"root homes\") (perspective-name \"the perspective of each root's home skgrepo (main)\") (content \"No other perspective would show this buffer differently from the perspective of each root's home skgrepo.\"))")
        (should (null skg-perspective))
        (should (string-match-p "No other perspective" shown))))))

(ert-deftest test-perspective-rides-in-the-save-request-and-survives-a-mode-change ()
  (test--in-view "r"
    (let ((position '(:point-lines-below-focused-headline 0 :point-column 0
                      :point-screen-lines-below-window-start 0)))
      (should-not (assq 'perspective (skg--save-request-sexp "r" position)))
      (skg--set-perspective "skgrepo tracy" "the perspective of tracy")
      (skg-content-view-mode)
      (should (equal skg-perspective "skgrepo tracy"))
      (should (equal skg-perspective-name "the perspective of tracy"))
      (should (equal (cdr (assq 'perspective (skg--save-request-sexp "r" position)))
                     "skgrepo tracy")))))

(ert-deftest test-perspective-is-on-C-c-v-o-and-C-c-v-p-is-free ()
  (should (eq (lookup-key skg-content-view-mode-map (kbd "C-c v o"))
              #'skg-cycle-perspective))
  (should-not (lookup-key skg-content-view-mode-map (kbd "C-c v p"))))

(provide 'test-skg-perspective)
